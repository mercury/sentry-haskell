-- | HTTP\/2 connection management for Sentry transports.
--
-- @stability: experimental@
--
-- __Status:__ this is the connection layer for the experimental HTTP\/2
-- transport ('Sentry.Transport.HTTP2.Async')
-- 
-- Maintains a single long-lived HTTP\/2 TLS connection to the Sentry ingest
-- endpoint via ALPN @h2@ negotiation.
--
-- A single long-lived HTTP\/2 TLS connection is maintained for the life of the
-- connection; it is is established when the first envelope is sent, and
-- reconnected transparently if the server closes it.
--
-- == Threading model
--
-- 'sendEnvelope' is designed to be called from a single worker thread (the
-- 'Sentry.Transport.Executor.Async.AsyncExecutor' worker).  The connection
-- state is stored in an 'IORef' that is read and written only by that worker,
-- so no additional locking is needed.
--
-- The HTTP\/2 runner ('Network.HTTP2.TLS.Client.runWithConfig') is forked on
-- its own 'Async' thread, which blocks until the shutdown 'MVar' is filled or
-- the server disconnects.
module Sentry.Transport.HTTP2.Connection
  ( -- * Connection target
    ConnTarget (..),
    connTargetFromDsn,

    -- * HTTP\/2 protocol settings
    Http2Settings (..),
    applyHttp2Settings,

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
import Data.Default (Default (def))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Encoding
import Network.HTTP.Semantics qualified as HTTPSemantics
import Network.HTTP.Types (RequestHeaders, ResponseHeaders, methodPost)
import Network.HTTP2.Client qualified as HTTP2
import Network.HTTP2.TLS.Client qualified as HTTP2TLS
import Network.Run.TCP qualified as NetworkRun
import Network.Socket (PortNumber, SocketOption (NoDelay))
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
    -- | TCP port; defaults to 443 when the DSN omits it.
    port :: PortNumber,
    -- | Pre-built request path, e.g. @\"/api\/42\/envelope\/\"@.
    path :: ByteString,
    -- | Pre-built Sentry-specific HTTP headers.
    headers :: RequestHeaders,
    -- | Body encoding.
    compression :: Compression
  }

-- | Derive a 'ConnTarget' from a 'Patrol.Dsn'.
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

-- | HTTP\/2 protocol-level settings applied when establishing a connection.
--
-- These map directly onto the fields of 'Network.HTTP2.TLS.Client.Settings'.
-- 'Nothing' values leave the corresponding @http2-tls@ default unchanged.
type Http2Settings :: Type
data Http2Settings = Http2Settings
  { -- | Enable @TCP_NODELAY@ on the client socket.
    --
    -- When @True@ (the default), each envelope write flushes immediately;
    -- with Nagle enabled, delayed-ACK on the server side can add up to
    -- ~40 ms of latency per small bursty send.
    --
    -- Leave at the default unless you send very few very large envelopes
    -- and care more about throughput than latency.
    tcpNoDelay :: Bool,
    -- | Maximum number of HTTP\/2 PING frames accepted per second from the
    -- peer before sending @ENHANCE_YOUR_CALM@ (CVE-2019-9512).
    --
    -- Default: @Just 100@.
    --
    -- The @http2-tls@ library default is 10\/s, which is low enough to trigger
    -- against some aggressive load balancers or TLS-terminating proxies.
    --
    -- Set to @Just maxBound@ to disable this protection entirely when
    -- connecting to a fully trusted peer.
    pingRateLimit :: Maybe Int,
    -- | Maximum number of empty DATA\/HEADERS\/CONTINUATION frames accepted
    -- per second from the peer (CVE-2019-9518).
    --
    -- Default: @Nothing@ (keep @http2-tls@ default, currently 4\/s).
    emptyFrameRateLimit :: Maybe Int,
    -- | Maximum number of SETTINGS frames accepted per second from the peer
    -- (CVE-2019-9515).
    --
    -- Default: @Nothing@ (keep @http2-tls@ default, currently 4\/s).
    settingsRateLimit :: Maybe Int,
    -- | Maximum number of RST_STREAM frames accepted per second from the peer
    -- (CVE-2023-44487, \"Rapid Reset\").
    --
    -- Default: @Nothing@ (keep @http2-tls@ default, currently 4\/s).
    rstRateLimit :: Maybe Int,
    -- | HTTP\/2 connection flow-control window size in bytes.
    --
    -- Default: @Nothing@ (keep @http2-tls@ default, currently 16 MiB).
    -- Only relevant under concurrent stream use; the current transport sends
    -- one stream at a time so the default is safe.
    connectionWindowSize :: Maybe Int,
    -- | HTTP\/2 per-stream flow-control window size in bytes
    -- (@SETTINGS_INITIAL_WINDOW_SIZE@).
    --
    -- Default: @Nothing@ (keep @http2-tls@ default, currently 256 KiB).
    streamWindowSize :: Maybe Int,
    -- | Maximum number of concurrent incoming streams announced to the peer
    -- (@SETTINGS_MAX_CONCURRENT_STREAMS@).
    --
    -- Default: @Nothing@ (keep @http2-tls@ default, currently 64).
    maxConcurrentStreams :: Maybe Int
  }

-- | Defaults: TCP_NODELAY enabled, ping rate limit raised to 100\/s,
-- all other knobs at @http2-tls@ library defaults.
instance Default Http2Settings where
  def =
    Http2Settings
      { tcpNoDelay = True,
        pingRateLimit = Just 100,
        emptyFrameRateLimit = Nothing,
        settingsRateLimit = Nothing,
        rstRateLimit = Nothing,
        connectionWindowSize = Nothing,
        streamWindowSize = Nothing,
        maxConcurrentStreams = Nothing
      }

-- | Apply an 'Http2Settings' override record to a base 'HTTP2TLS.Settings'.
--
-- 'Nothing' fields leave the corresponding base value unchanged; 'Just'
-- fields replace it.
--
-- 'tcpNoDelay' installs a custom 'HTTP2TLS.settingsOpenClientSocket' that
-- opens the socket with @TCP_NODELAY@ set.
applyHttp2Settings :: Http2Settings -> HTTP2TLS.Settings -> HTTP2TLS.Settings
applyHttp2Settings s base =
  base
    { HTTP2TLS.settingsPingRateLimit =
        fromMaybe (HTTP2TLS.settingsPingRateLimit base) s.pingRateLimit,
      HTTP2TLS.settingsEmptyFrameRateLimit =
        fromMaybe (HTTP2TLS.settingsEmptyFrameRateLimit base) s.emptyFrameRateLimit,
      HTTP2TLS.settingsSettingsRateLimit =
        fromMaybe (HTTP2TLS.settingsSettingsRateLimit base) s.settingsRateLimit,
      HTTP2TLS.settingsRstRateLimit =
        fromMaybe (HTTP2TLS.settingsRstRateLimit base) s.rstRateLimit,
      HTTP2TLS.settingsConnectionWindowSize =
        fromMaybe (HTTP2TLS.settingsConnectionWindowSize base) s.connectionWindowSize,
      HTTP2TLS.settingsStreamWindowSize =
        fromMaybe (HTTP2TLS.settingsStreamWindowSize base) s.streamWindowSize,
      HTTP2TLS.settingsConcurrentStreams =
        fromMaybe (HTTP2TLS.settingsConcurrentStreams base) s.maxConcurrentStreams,
      HTTP2TLS.settingsOpenClientSocket =
        if s.tcpNoDelay
          then \addr -> NetworkRun.openClientSocketWithOptions [(NoDelay, 1)] addr
          else HTTP2TLS.settingsOpenClientSocket base
    }

-- | Decision returned by a reconnect policy after a failed connection attempt.
--
-- A reconnect policy is simply an @'IO' 'ReconnectDecision'@ action, which
-- runs on the worker thread after 'tryConnect' fails.
--
-- The default policy is @pure 'DontReconnect'@; use 'reconnectAfter' or
-- 'exponentialBackoff' for common retry patterns.
type ReconnectDecision :: Type
data ReconnectDecision
  = -- | Do not attempt to reconnect; subsequent sends fail immediately without
    -- touching the network.
    DontReconnect
  | -- | Allow another connection attempt, applying the enclosed policy on
    -- the /next/ failure.
    --
    -- This enables chained patterns like exponential backoff: each step
    -- returns a new action with a longer delay.
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
    -- in microseconds.
    --
    -- See 'Http2TransportOptions.connectTimeout'.
    connectTimeout :: Int,
    -- | HTTP\/2 protocol settings applied on every new connection.
    http2Settings :: Http2Settings,
    -- | The policy to restore when a connection attempt succeeds, so the
    -- next failure starts from the beginning of the backoff sequence.
    initialReconnectPolicy :: IO ReconnectDecision,
    -- | The policy action to run after the /next/ failed 'tryConnect'.
    -- 
    -- 'Nothing' means a prior 'DontReconnect' decision was already made;
    -- all future sends will fail immediately without touching the network.
    currentReconnectPolicy :: IORef (Maybe (IO ReconnectDecision))
  }

-- | Create a new 'ConnManager'.  No connection is established yet.
newConnManager :: ConnTarget -> Bool -> Int -> Http2Settings -> IO ReconnectDecision -> IO ConnManager
newConnManager target validateCert connectTimeout http2Settings initialReconnectPolicy = do
  activeConn <- newIORef Nothing
  currentReconnectPolicy <- newIORef (Just initialReconnectPolicy)
  pure ConnManager{activeConn, target, validateCert, connectTimeout, http2Settings, initialReconnectPolicy, currentReconnectPolicy}

-- | Signal the runner thread to return and cancel it.
--
-- Idempotency: safe to call on an already-dead connection.
teardownConn :: ActiveConn -> IO ()
teardownConn conn = do
  _ <- tryPutMVar conn.connShutdown ()
  Async.cancel conn.connThread

-- | Gracefully close the active connection, if any, and wait for the runner
-- thread to terminate.
--
-- Idempotency: safe to call multiple times.
closeConnManager :: ConnManager -> IO ()
closeConnManager mgr =
  readIORef mgr.activeConn >>= \case
    Nothing -> pure ()
    Just conn -> do
      writeIORef mgr.activeConn Nothing
      teardownConn conn

-- | Attempt to establish a connection and store it in 'activeConn'.
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
      runHttp2 mgr.target mgr.validateCert mgr.http2Settings $
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
--
-- Applies 'applyHttp2Settings' to
-- @'HTTP2TLS.defaultSettings' { 'HTTP2TLS.settingsValidateCert' = validateCert }@.
runHttp2 :: ConnTarget -> Bool -> Http2Settings -> HTTP2.Client a -> IO a
runHttp2 tgt validateCert http2s client =
  let settings =
        applyHttp2Settings http2s $
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
