-- | HTTP\/2 connection management for Sentry transports.
--
-- @stability: experimental@
--
-- This is the connection layer for the experimental HTTP\/2 transport
-- ('Sentry.Transport.HTTP2.Async'), which maintains a single, long-lived
-- HTTP\/2 TLS connection to the Sentry ingest endpoint via ALPN @h2@
-- negotiation.
--
-- The connection is established when the first envelope is sent, and
-- reconnected transparently if closed by the server.
--
-- == Threading model
--
-- Connection lifecycle is a lock-free STM state machine ('State') stored in
-- a single 'Control.Concurrent.STM.TVar'.
--
-- The HTTP\/2 runner ('Network.HTTP2.TLS.Client.runWithConfig') runs on its
-- own thread, which blocks until the shutdown 'MVar' is filled or the server
-- disconnects.
--
-- When the runner exits for any reason it resets the state back to 'Idle'
-- so the next send can attempt to reconnect.
module Sentry.Transport.HTTP2.Connection
  ( -- * Connection endpoint
    Endpoint,
    mkEndpoint,

    -- * HTTP\/2 protocol settings
    Http2Settings (..),
    applyHttp2Settings,

    -- * Reconnect decision
    ReconnectDecision (..),
    reconnectAfter,
    exponentialBackoff,

    -- * Connection manager
    Manager,
    newManager,
    closeManager,

    -- * Sending
    sendEnvelope,
  )
where

import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newEmptyTMVarIO, newTVarIO, orElse, putTMVar, readTVar, registerDelay, retry, swapTVar, takeTMVar, writeTVar)
import Control.Monad (join, unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.Default (Default (def))
import Data.IORef (newIORef, readIORef, writeIORef)
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
import Sentry.Transport.Delivery qualified as Delivery
import Sentry.Transport.HTTP.Request (Compression)
import Sentry.Transport.HTTP.Request qualified as Request
import UnliftIO.Exception (catchAny, finally, mask, onException)
import Witch qualified

-- | The static parameters needed to establish an HTTP\/2 connection and POST
-- envelopes for a given DSN.
type Endpoint :: Type
data Endpoint = Endpoint
  { -- | Hostname without port; used for both the TCP connection and the TLS
    -- SNI\/HTTP\/2 @:authority@ pseudo-header.
    host :: String,
    -- | TCP port; defaults to 443 when the DSN omits it.
    port :: PortNumber,
    -- | Pre-built request path, e.g. @\"/api\/42\/envelope\/\"@.
    path :: ByteString,
    -- | Pre-built Sentry-specific HTTP headers, including @content-encoding@
    -- when the body is compressed.
    headers :: RequestHeaders,
    -- | Body encoding.
    compression :: Compression
  }

instance Witch.From Patrol.Dsn Endpoint where
  from = mkEndpoint def

-- | Build an 'Endpoint' from a 'Patrol.Dsn' with an explicit 'Compression'
-- setting.
--
-- Prefer the 'Witch.From' instance if you want 'Gzip' compression by default.
mkEndpoint :: Compression -> Patrol.Dsn -> Endpoint
mkEndpoint compression dsn =
  Endpoint
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
    -- Raise this limit if you experience connection resets from peers that send
    -- frequent PING frames.
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
    -- Relevant when multiple HTTP\/2 streams are open concurrently.
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
-- A reconnect policy is an @'IO' 'ReconnectDecision'@ action consulted by the
-- connection manager after 'tryConnect' fails.
--
-- The default policy is @pure 'DontReconnect'@; use 'reconnectAfter' or
-- 'exponentialBackoff' for common retry patterns.
type ReconnectDecision :: Type
data ReconnectDecision
  = -- | Do not attempt to reconnect; subsequent sends fail immediately without
    -- touching the network.
    DontReconnect
  | -- | Back off for the given number of microseconds, then attempt one
    -- reconnect; the enclosed action is the policy to consult on the /next/
    -- failure.
    --
    -- This enables chained patterns like exponential backoff, where each step
    -- returns a longer delay paired with the action for the step after it;
    -- because the delay is returned rather than performed, outgoing requests
    -- during the backoff window fail fast instead of blocking.
    ReconnectAfter !Int (IO ReconnectDecision)

-- | Reconnect after a fixed delay, retrying indefinitely.
--
-- > reconnectAfter 5_000_000  -- retry every 5 s
reconnectAfter :: Int -> IO ReconnectDecision
reconnectAfter delayMicros =
  pure $ ReconnectAfter delayMicros (reconnectAfter delayMicros)

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
    go delay =
      let next = min maxDelay (round (fromIntegral delay * factor))
       in pure $ ReconnectAfter delay (go next)

-- | An established HTTP\/2 connection.
type Active :: Type
data Active = Active
  { -- | Captured @http2@ @sendRequest@ callback.
    --
    -- Calling this issues a single HTTP\/2 stream on the shared connection.
    doSend :: HTTP2.Request -> (HTTP2.Response -> IO ()) -> IO (),
    -- | Fill this 'MVar' to signal the runner thread to return.
    shutdown :: MVar (),
    -- | The runner thread (executing 'HTTP2TLS.runWithConfig').
    thread :: Async ()
  }

-- | The lifecycle state of a 'Manager''s single HTTP\/2 connection.
--
--   * 'acquire' claims 'Idle' (or an elapsed 'Backoff') by writing
--     'Connecting'; concurrent callers then see 'Connecting' and park on
--     'retry'.
--   * the single claimant commits 'Connecting' to 'Connected', 'Backoff', or
--     'GaveUp'.
--   * the runner thread resets 'Connected' to 'Idle' when it exits.
--   * 'closeManager' moves any state to the terminal 'Closed'.
--
-- __NOTE__: Transitions are correct __so long as each edge has exactly one
-- possible mutator__.
type State :: Type
data State
  = -- | No connection; the next caller single-flights one.
    Idle
  | -- | A claimant is running the handshake; other callers 'retry' until it
    -- resolves.
    Connecting
  | -- | A live connection; the hot path is a single 'readTVar' of this.
    Connected !Active
  | -- | The last attempt failed.  Sends fail fast until the timer 'TVar' flips
    -- 'True', after which the next caller reconnects using the enclosed policy.
    Backoff !(TVar Bool) !(IO ReconnectDecision)
  | -- | A 'DontReconnect' decision was reached; all sends fail immediately
    -- without touching the network.
    GaveUp
  | -- | The manager has been shut down; terminal.
    Closed

-- | Manages a single lazy HTTP\/2 connection.
type Manager :: Type
data Manager = Manager
  { -- | The lock-free connection lifecycle state machine.
    state :: TVar State,
    target :: Endpoint,
    -- | Whether to validate TLS server certificates.
    validateCert :: Bool,
    -- | How long to wait for the HTTP\/2 handshake before giving up,
    -- in microseconds.
    --
    -- See 'Http2TransportOptions.connectTimeout'.
    connectTimeout :: Int,
    -- | HTTP\/2 protocol settings applied on every new connection.
    http2Settings :: Http2Settings,
    -- | The policy consulted on the first failure of a fresh connect attempt.
    initialReconnectPolicy :: IO ReconnectDecision
  }

-- | Create a new 'Manager'.  No connection is established yet.
newManager :: Endpoint -> Bool -> Int -> Http2Settings -> IO ReconnectDecision -> IO Manager
newManager target validateCert connectTimeout http2Settings initialReconnectPolicy = do
  state <- newTVarIO Idle
  pure Manager{state, target, validateCert, connectTimeout, http2Settings, initialReconnectPolicy}

-- | Signal the runner thread to return and cancel it.
teardown :: Active -> IO ()
teardown conn = do
  _ <- tryPutMVar conn.shutdown ()
  Async.cancel conn.thread

-- | Gracefully close the manager: move to the terminal 'Closed' state and tear
-- down a live connection if one is present.
--
-- An in-flight connect observes 'Closed' at its commit point and tears down the
-- connection it built rather than installing it, so no connection can outlive
-- the manager.
closeManager :: Manager -> IO ()
closeManager mgr =
  atomically (swapTVar mgr.state Closed) >>= \case
    Connected conn -> teardown conn
    _ -> pure ()

-- | Attempt to establish a connection, returning the live 'Active' or a
-- description of why it failed.
tryConnect :: Manager -> IO (Either Text Active)
tryConnect mgr = do
  shutdownVar <- newEmptyMVar
  sendRequestVar <- newEmptyTMVarIO
  timeoutVar <- registerDelay mgr.connectTimeout

  -- Fork the HTTP/2 runner thread.
  runner <-
    Async.async $
      runHttp2
        mgr.target
        mgr.validateCert
        mgr.http2Settings
        ( \sendReq _aux -> do
            atomically $ putTMVar sendRequestVar sendReq
            takeMVar shutdownVar
        )
        `finally` atomically
          ( modifyTVar' mgr.state \case
              Connected _ -> Idle
              other -> other
          )

  -- Wait for whichever comes first in a single atomic STM transaction:
  --
  --  1. 'sendRequestVar' filled → handshake complete; return the connection.
  --  2. Runner thread done      → connection failed before publishing (DNS
  --                               error, TLS rejection, …).
  --  3. Timeout expired         → runner alive but stuck (TCP connected,
  --                               server SETTINGS frame never received).
  --
  -- If this wait is interrupted (e.g. the claimant is cancelled mid-handshake),
  -- cancel the runner so it cannot leak.
  let awaitHandshake =
        atomically $
          (Just <$> takeTMVar sendRequestVar)
            `orElse` (Async.pollSTM runner >>= maybe retry (\_ -> pure Nothing))
            `orElse` do
              expired <- readTVar timeoutVar
              unless expired retry
              pure Nothing
  result <- awaitHandshake `onException` Async.cancel runner

  case result of
    Nothing -> do
      -- Connection not established: cancel the runner (no-op if already dead).
      Async.cancel runner
      pure (Left "HTTP/2 connection could not be established")
    Just sendReq ->
      pure $
        Right
          Active
            { doSend = sendReq,
              shutdown = shutdownVar,
              thread = runner
            }

-- | Run an HTTP\/2 'HTTP2.Client' action over TLS to 'Endpoint'.
--
-- Applies 'applyHttp2Settings' to
-- @'HTTP2TLS.defaultSettings' { 'HTTP2TLS.settingsValidateCert' = validateCert }@.
runHttp2 :: Endpoint -> Bool -> Http2Settings -> HTTP2.Client a -> IO a
runHttp2 tgt validateCert http2s client =
  let settings =
        applyHttp2Settings http2s $
          HTTP2TLS.defaultSettings
            { HTTP2TLS.settingsValidateCert = validateCert
            }
      clientConfig = HTTP2TLS.defaultClientConfig settings tgt.host
   in HTTP2TLS.runWithConfig clientConfig settings tgt.host tgt.port client

-- | Send an envelope over the shared HTTP\/2 connection, returning an
-- 'Outcome'.
--
-- Any exception during the send is caught and returned as a 'NetworkFailure'
-- for /this/ envelope only; it does not tear down the shared connection (a
-- connection-level failure is handled centrally by the runner-exit reset).
sendEnvelope :: Manager -> Patrol.Envelope -> IO Delivery.Outcome
sendEnvelope mgr envelope =
  acquire mgr >>= \case
    Left err -> pure (Delivery.NetworkFailure err)
    Right conn -> sendOn mgr conn envelope

-- | Acquire a live connection, single-flighting a connect when necessary.
acquire :: Manager -> IO (Either Text Active)
acquire mgr = join . atomically $ do
  readTVar mgr.state >>= \case
    -- Hot path: reuse the live connection without a lock.
    Connected conn -> pure . pure $ Right conn
    -- Someone else is connecting; park until they resolve it, then re-decide.
    Connecting -> retry
    -- Terminal / fail-fast states.
    Closed -> pure . pure $ Left "HTTP/2 connection manager closed"
    GaveUp -> pure . pure $ Left "HTTP/2 transport has given up reconnecting"
    -- No connection: claim the single-flight slot and connect.
    Idle -> claim mgr.initialReconnectPolicy
    -- Backed off after a failure: fail fast until the timer elapses, then the
    -- next caller claims a reconnect with the policy's continuation.
    Backoff timer policy ->
      readTVar timer >>= \case
        False -> pure . pure $ Left "HTTP/2 transport is backing off after a failed connect"
        True -> claim policy
  where
    -- Transition 'Connecting' inside this transaction so concurrent callers
    -- immediately park; run the (blocking) connect after the transaction.
    claim policy = do
      writeTVar mgr.state Connecting
      pure $ runConnect mgr policy

runConnect :: Manager -> IO ReconnectDecision -> IO (Either Text Active)
runConnect mgr policy = (`onException` resetClaim) $ mask $ \restore -> do
  -- 'tryConnect' self-cancels its runner if interrupted, so it leaks nothing.
  restore (tryConnect mgr) >>= \case
    Right conn -> commit conn -- masked: ends as 'Connected' or torn down
    Left err -> restore (handleFailure err)
  where
    -- Drop the 'Connecting' claim so the next caller retries; never clobber a
    -- 'Connected'/'Closed'/… that some other transition already installed.
    resetClaim =
      atomically $
        modifyTVar' mgr.state \case
          Connecting -> Idle
          other -> other

    -- Install the live connection, unless the manager was closed (or otherwise
    -- moved on) under us, in which case tear down the orphan we just built.
    commit conn = join . atomically $ do
      readTVar mgr.state >>= \case
        Connecting -> writeTVar mgr.state (Connected conn) >> pure (pure (Right conn))
        _ -> pure (teardown conn >> pure (Left "HTTP/2 connection manager closed"))

    -- Consult the policy and schedule a non-blocking 'Backoff', or give up
    -- permanently.
    handleFailure err = do
      decision <- policy
      join . atomically $
        readTVar mgr.state >>= \case
          Connecting -> case decision of
            DontReconnect -> writeTVar mgr.state GaveUp >> pure (pure (Left err))
            ReconnectAfter delayMicros nextPolicy -> pure $ do
              timer <- registerDelay delayMicros
              atomically $
                modifyTVar' mgr.state \case
                  Connecting -> Backoff timer nextPolicy
                  other -> other
              pure (Left err)
          -- Closed under us while connecting.
          _ -> pure (pure (Left "HTTP/2 connection manager closed"))

-- | Perform a single envelope POST on an established 'Active'.
--
-- 'doSend' returns @IO ()@, so the 'Outcome' is recovered via an 'IORef'.
--
-- This is safe /only/ because 'doSend' invokes the response continuation
-- synchronously and returns once it has run: the ref is always written (by the
-- continuation on success, or the handler on failure) before we read it below.
--
-- A failure here is reported as a 'NetworkFailure' for this envelope only.
sendOn :: Manager -> Active -> Patrol.Envelope -> IO Delivery.Outcome
sendOn mgr conn envelope = do
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
    `catchAny` \e ->
      writeIORef outcomeRef (Just . Delivery.NetworkFailure . Text.pack $ show e)
  readIORef outcomeRef >>= \case
    Just outcome -> pure outcome
    -- Unreachable given the synchronicity guarantee above; kept as a defensive
    -- total fallback rather than an 'error'.
    Nothing -> pure $ Delivery.NetworkFailure "HTTP/2 send completed without response"

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
