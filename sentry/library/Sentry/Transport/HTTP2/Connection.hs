-- | HTTP\/2 connection management for Sentry transports.
--
-- Maintains a single long-lived HTTP\/2 TLS connection to the Sentry ingest
-- endpoint via ALPN @h2@ negotiation.  The @https://@ DSN scheme is required;
-- plaintext h2c is not supported.
--
-- The connection is established lazily on the first envelope send and is kept
-- alive for multiplexed use — the intended lifecycle is one connection per
-- 'ConnManager' for the life of the transport.
--
-- == Threading model
--
-- 'sendEnvelope' is designed to be called from a single worker thread (the
-- 'Sentry.Transport.Executor.Async.AsyncExecutor' worker).  The connection
-- state is stored in an 'IORef' that is read and written only by that worker,
-- so no additional locking is needed.
--
-- The HTTP\/2 runner ('Network.HTTP2.TLS.Client.runWithConfig') is forked on
-- its own 'Async' thread.  The runner thread blocks until the shutdown 'MVar'
-- is filled or the server disconnects.
module Sentry.Transport.HTTP2.Connection
  ( -- * Connection target
    ConnTarget (..),
    connTargetFromDsn,

    -- * Reconnect decision
    ReconnectDecision (..),
    reconnectAfter,
    exponentialBackoff,

    -- * Connection manager
    ConnManager,
    newConnManager,
    closeConnManager,

    -- * Sending
    sendEnvelope,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Concurrent.STM (atomically, newEmptyTMVarIO, orElse, putTMVar, readTVar, registerDelay, retry, takeTMVar)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Encoding
import Network.HTTP.Semantics qualified as HTTPSemantics
import Network.HTTP.Types (RequestHeaders, ResponseHeaders, methodPost)
import Network.HTTP2.Client qualified as HTTP2
import Network.HTTP2.TLS.Client qualified as HTTP2TLS
import Network.Socket (PortNumber)
import Patrol qualified
import Patrol.Constant qualified as Patrol.Constant
import Patrol.Type.Dsn qualified as Patrol.Dsn
import Sentry.Transport.HTTP.Request (Compression)
import Sentry.Transport.HTTP.Request qualified as Request
import Sentry.Transport.Delivery qualified as Delivery
import UnliftIO.Exception (catchAny)

-- | The static parameters needed to establish an HTTP\/2 connection for a
-- given DSN.
type ConnTarget :: Type
data ConnTarget = ConnTarget
  { -- | Hostname without port; used for both the TCP connection and the TLS
    -- SNI\/HTTP\/2 @:authority@ pseudo-header.
    host :: String,
    -- | TCP port.  Defaults to 443 when the DSN omits it.
    port :: PortNumber,
    -- | Pre-built request path, e.g. @\"/api\/42\/envelope\/\"@.
    path :: ByteString,
    -- | Pre-built Sentry-specific HTTP headers.
    headers :: RequestHeaders,
    -- | Body encoding.
    compression :: Compression
  }

-- | Derive a 'ConnTarget' from a 'Patrol.Dsn'.
--
-- The @https://@ scheme is required; the HTTP\/2 transport does not support
-- plaintext h2c.
connTargetFromDsn :: Compression -> Patrol.Dsn -> ConnTarget
connTargetFromDsn compression dsn =
  ConnTarget
    { host = Text.unpack dsn.host,
      port = maybe 443 fromIntegral dsn.port,
      path = Encoding.encodeUtf8 $ dsn.path <> "api/" <> dsn.projectId <> "/envelope/",
      headers =
        [ ("content-type", Patrol.Constant.applicationXSentryEnvelope),
          ("user-agent", Encoding.encodeUtf8 Patrol.Constant.userAgent),
          ("x-sentry-auth", Patrol.Dsn.intoAuthorization dsn)
        ]
          <> case compression of
            Request.None -> []
            Request.Gzip -> [("content-encoding", "gzip")],
      compression
    }

-- | Decision returned by a reconnect policy after a failed connection attempt.
--
-- A reconnect policy is simply an @'IO' 'ReconnectDecision'@ action.  It
-- runs on the worker thread after 'tryConnect' fails, so blocking inside
-- the action (e.g. via 'threadDelay') applies natural back-pressure
-- without a separate retry loop.
--
-- The default policy is @pure 'DontReconnect'@.  Use 'reconnectAfter' or
-- 'exponentialBackoff' for common retry patterns.
type ReconnectDecision :: Type
data ReconnectDecision
  = -- | Do not attempt to reconnect.  Subsequent sends fail immediately
    -- without touching the network.
    DontReconnect
  | -- | Allow another connection attempt, applying the enclosed policy on
    -- the /next/ failure.  This enables chained patterns like exponential
    -- backoff: each step returns a new action with a longer delay.
    DoReconnect (IO ReconnectDecision)

-- | Reconnect after a fixed delay, retrying indefinitely.
--
-- > reconnectAfter 5_000_000  -- retry every 5 s
reconnectAfter :: Int -> IO ReconnectDecision
reconnectAfter delayMicros = do
  threadDelay delayMicros
  pure $ DoReconnect (reconnectAfter delayMicros)

-- | Reconnect with exponential backoff, retrying indefinitely.
--
-- The delay starts at @initial@, is multiplied by @factor@ after each
-- failure, and is capped at @maxDelay@.
--
-- > exponentialBackoff 1_000_000 2.0 60_000_000
-- >   -- 1 s → 2 s → 4 s → … → 60 s → 60 s → …
exponentialBackoff ::
  -- | Initial delay in microseconds.
  Int ->
  -- | Backoff multiplier (e.g. @2.0@ to double each step).
  Double ->
  -- | Maximum delay in microseconds.
  Int ->
  IO ReconnectDecision
exponentialBackoff initial factor maxDelay = go initial
  where
    go delay = do
      threadDelay delay
      let next = min maxDelay (round (fromIntegral delay * factor))
      pure $ DoReconnect (go next)

-- | An established HTTP\/2 connection.
type ActiveConn :: Type
data ActiveConn = ActiveConn
  { -- | Captured @http2@ @sendRequest@ callback.
    --
    -- Calling this issues a single HTTP\/2 stream on the shared connection.
    doSend :: HTTP2.Request -> (HTTP2.Response -> IO ()) -> IO (),
    -- | Fill this 'MVar' to signal the runner thread to return.
    connShutdown :: MVar (),
    -- | The runner thread (executing 'HTTP2TLS.runWithConfig').
    connThread :: Async ()
  }

-- | Manages a single lazy HTTP\/2 connection.
type ConnManager :: Type
data ConnManager = ConnManager
  { activeConn :: IORef (Maybe ActiveConn),
    target :: ConnTarget,
    -- | Whether to validate TLS server certificates.
    validateCert :: Bool,
    -- | How long to wait for the HTTP\/2 handshake before giving up,
    -- in microseconds.  See 'Http2TransportOptions.connectTimeout'.
    connectTimeout :: Int,
    -- | The policy to restore when a connection attempt succeeds, so the
    -- next failure starts from the beginning of the backoff sequence.
    initialReconnectPolicy :: IO ReconnectDecision,
    -- | The policy action to run after the /next/ failed 'tryConnect'.
    -- 'Nothing' means a prior 'DontReconnect' decision was already made;
    -- all future sends will fail immediately without touching the network.
    currentReconnectPolicy :: IORef (Maybe (IO ReconnectDecision))
  }

-- | Create a new 'ConnManager'.  No connection is established yet.
newConnManager :: ConnTarget -> Bool -> Int -> IO ReconnectDecision -> IO ConnManager
newConnManager target validateCert connectTimeout initialReconnectPolicy = do
  activeConn <- newIORef Nothing
  currentReconnectPolicy <- newIORef (Just initialReconnectPolicy)
  pure ConnManager{activeConn, target, validateCert, connectTimeout, initialReconnectPolicy, currentReconnectPolicy}

-- | Signal the runner thread to return and cancel it. Idempotent: safe to call
-- on an already-dead connection (the 'tryPutMVar' and 'cancel' both no-op).
teardownConn :: ActiveConn -> IO ()
teardownConn conn = do
  _ <- tryPutMVar conn.connShutdown ()
  Async.cancel conn.connThread

-- | Gracefully close the active connection, if any, and wait for the runner
-- thread to terminate.
--
-- Safe to call multiple times.
closeConnManager :: ConnManager -> IO ()
closeConnManager mgr =
  readIORef mgr.activeConn >>= \case
    Nothing -> pure ()
    Just conn -> do
      writeIORef mgr.activeConn Nothing
      teardownConn conn

-- | Attempt to establish a connection and store it in 'activeConn'.
--
-- On failure (DNS, TLS, refused, …) 'activeConn' is left as 'Nothing'.
tryConnect :: ConnManager -> IO ()
tryConnect mgr = do
  connShutdown <- newEmptyMVar
  -- 'TMVar' rather than 'MVar': the one-shot rendezvous for 'sendRequest'
  -- must compose with STM's 'orElse' so all three outcomes below can be
  -- checked in a single atomic transaction — no race between the timeout
  -- firing and the var being filled.
  sendRequestVar <- newEmptyTMVarIO
  timeoutVar <- registerDelay mgr.connectTimeout

  -- Fork the HTTP/2 runner thread.
  --
  -- The 'runWithConfig' callback publishes 'sendRequest' and then blocks on
  -- 'connShutdown', keeping the connection alive for the transport's lifetime.
  thread <-
    Async.async $
      runHttp2 mgr.target mgr.validateCert $
        \sendReq _aux -> do
          atomically $ putTMVar sendRequestVar sendReq
          takeMVar connShutdown

  -- Wait for whichever comes first in a single atomic STM transaction:
  --
  --  1. 'sendRequestVar' filled → handshake complete; store the connection.
  --  2. Runner thread done      → connection failed before publishing (DNS
  --                               error, TLS rejection, …).
  --  3. Timeout expired         → runner alive but stuck (TCP connected,
  --                               server SETTINGS frame never received).
  result <-
    atomically $
      (Just <$> takeTMVar sendRequestVar)
        `orElse` (Async.pollSTM thread >>= maybe retry (\_ -> pure Nothing))
        `orElse` do
          expired <- readTVar timeoutVar
          unless expired retry
          pure Nothing

  case result of
    Nothing ->
      -- Connection not established: cancel the runner (no-op if already dead).
      Async.cancel thread
    Just sendReq ->
      writeIORef mgr.activeConn $
        Just
          ActiveConn
            { doSend = sendReq,
              connShutdown,
              connThread = thread
            }

-- | Run an HTTP\/2 'HTTP2.Client' action over TLS to 'ConnTarget'.
runHttp2 :: ConnTarget -> Bool -> HTTP2.Client a -> IO a
runHttp2 tgt validateCert client =
  let settings =
        HTTP2TLS.defaultSettings
          { HTTP2TLS.settingsValidateCert = validateCert
          }
      clientConfig = HTTP2TLS.defaultClientConfig settings tgt.host
   in HTTP2TLS.runWithConfig clientConfig settings tgt.host tgt.port client

-- | Send an envelope over the HTTP\/2 connection, returning an 'Outcome'.
--
-- Connects lazily on the first call; if the runner thread has exited, attempt
-- to reconnect before sending.
--
-- Any exception during the send is caught, the connection torn down, and a
-- 'NetworkFailure' returned.
sendEnvelope :: ConnManager -> Patrol.Envelope -> IO Delivery.Outcome
sendEnvelope mgr envelope = do
  mConn <- getOrConnect mgr
  case mConn of
    Left err -> pure (Delivery.NetworkFailure err)
    Right conn -> sendOnConn mgr conn envelope

-- | Return the current 'ActiveConn', reconnecting if necessary.
getOrConnect :: ConnManager -> IO (Either Text ActiveConn)
getOrConnect mgr =
  readIORef mgr.activeConn >>= \case
    Just conn -> do
      status <- Async.poll conn.connThread
      case status of
        -- Runner still alive — reuse.
        Nothing -> pure (Right conn)
        -- Runner exited (server disconnect) — reconnect.
        Just _ -> do
          writeIORef mgr.activeConn Nothing
          reconnect
    Nothing ->
      reconnect
  where
    reconnect =
      readIORef mgr.currentReconnectPolicy >>= \case
        -- A prior 'DontReconnect' decision: fail fast without touching the network.
        Nothing ->
          pure (Left "HTTP/2 transport has given up reconnecting")
        Just policy -> do
          tryConnect mgr
          readIORef mgr.activeConn >>= \case
            Just conn -> do
              -- Success: restore the initial policy so the next failure
              -- starts from the beginning of the backoff sequence.
              writeIORef mgr.currentReconnectPolicy (Just mgr.initialReconnectPolicy)
              pure (Right conn)
            Nothing -> do
              -- Failure: run the policy action (may block for a delay).
              decision <- policy
              case decision of
                DontReconnect ->
                  writeIORef mgr.currentReconnectPolicy Nothing
                DoReconnect nextPolicy ->
                  writeIORef mgr.currentReconnectPolicy (Just nextPolicy)
              pure (Left "HTTP/2 connection could not be established")

-- | Perform a single envelope POST on an established 'ActiveConn'.
--
-- 'doSend' returns @IO ()@ — http2 fixes its result to the runner callback's
-- type — so the 'Outcome' is recovered through an 'IORef'. That is safe only
-- because 'doSend' invokes the response continuation synchronously and returns
-- once it has run: the ref is always written (by the continuation on success,
-- or the handler on failure) before we read it below.
sendOnConn :: ConnManager -> ActiveConn -> Patrol.Envelope -> IO Delivery.Outcome
sendOnConn mgr conn envelope = do
  outcomeRef <- newIORef Nothing
  let body = Request.serializeBody mgr.target.compression envelope
      req =
        HTTP2.requestStreaming
          methodPost
          mgr.target.path
          mgr.target.headers
          $ \write _flush ->
            write (Builder.lazyByteString body)
  ( conn.doSend req $ \resp -> do
      drainBody resp
      let headers = toResponseHeaders (HTTP2.responseHeaders resp)
          outcome = case HTTP2.responseStatus resp of
            Nothing ->
              Delivery.NetworkFailure "HTTP/2 response missing :status pseudo-header"
            Just status ->
              Delivery.Responded status headers
      writeIORef outcomeRef (Just outcome)
    )
    `catchAny` \e -> do
      -- Abandon the connection so the next send reconnects.
      writeIORef mgr.activeConn Nothing
      teardownConn conn
      writeIORef outcomeRef (Just $ Delivery.NetworkFailure (Text.pack (show e)))
  readIORef outcomeRef >>= \case
    Just outcome -> pure outcome
    -- Unreachable given the synchronicity guarantee above; kept as a defensive
    -- total fallback rather than an 'error'.
    Nothing -> pure (Delivery.NetworkFailure "HTTP/2 send completed without response")

-- | Drain the HTTP\/2 response body chunks until EOF.
--
-- Required to release the HTTP\/2 stream.
drainBody :: HTTP2.Response -> IO ()
drainBody resp = do
  chunk <- HTTP2.getResponseBodyChunk resp
  unless (BS.null chunk) (drainBody resp)

-- | Extract case-insensitive header names from an @http2@
-- 'HTTPSemantics.TokenHeaderTable'.
toResponseHeaders :: HTTPSemantics.TokenHeaderTable -> ResponseHeaders
toResponseHeaders tht =
  [(HTTPSemantics.tokenKey tok, val) | (tok, val) <- fst tht]
