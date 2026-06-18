-- | Asynchronous HTTP\/2 transport for Sentry.
--
-- This transport wraps an 'AsyncExecutor' with HTTP\/2 envelope delivery via
-- 'Sentry.Transport.HTTP2.Connection.sendEnvelope'.  Envelopes are queued
-- and sent on a dedicated worker thread with rate limiting handled
-- automatically.
--
-- A single long-lived HTTP\/2 TLS connection (ALPN @h2@) is maintained for the
-- life of the transport.  The connection is established lazily on the first
-- send and reconnected transparently if the server closes it.  The @https://@
-- DSN scheme is required; plaintext h2c is not supported.
--
-- TLS certificate validation is enabled by default and can be disabled via
-- 'Http2TransportOptions.validateCert'.
--
-- == Note on OpenTelemetry instrumentation
--
-- Unlike the HTTP\/1.1 transports, this backend does __not__ flow through the
-- @hs-opentelemetry-instrumentation-http-client@ layer; OTel spans are
-- therefore __not__ emitted for HTTP\/2 envelope sends.
module Sentry.Transport.HTTP2.Async
  ( -- * Async HTTP\/2 Transport
    AsyncHttp2Transport (..),
    new,
    build,

    -- * Options
    Http2TransportOptions (..),

    -- * Re-exports
    Compression (..),
    ReconnectDecision (..),
    reconnectAfter,
    exponentialBackoff,
  )
where

import Data.Default (Default (def))
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Time.Clock (getCurrentTime)
import Patrol qualified
import Patrol.Type.Envelope qualified as Patrol.Envelope
import Patrol.Type.Headers qualified as Patrol.Headers
import Patrol.Type.Item qualified as Patrol.Item
import Patrol.Type.Items qualified as Patrol.Items
import Sentry.Client.Options (ClientOptions (..), TransportProvider (..))
import Sentry.ClientReport (ClientReports)
import Sentry.ClientReport qualified as ClientReport
import Sentry.Transport (SomeTransport (..), Transport (..))
import Sentry.Transport.Executor.Async (AsyncExecutor, ClientReportConfig (..))
import Sentry.Transport.Executor.Async qualified as AsyncExecutor
import Sentry.Transport.Executor.RateLimiter qualified as RateLimiter
import Sentry.Transport.HTTP.Request (Compression (..))
import Sentry.Transport.HTTP2.Connection (ConnManager, ReconnectDecision (..), exponentialBackoff, reconnectAfter)
import Sentry.Transport.HTTP2.Connection qualified as Connection
import Sentry.Transport.Delivery qualified as Delivery

-- | An asynchronous HTTP\/2 transport backed by an 'AsyncExecutor'.
type AsyncHttp2Transport :: Type
data AsyncHttp2Transport = AsyncHttp2Transport
  { executor :: AsyncExecutor,
    -- | The underlying connection manager; closed after the executor shuts down.
    connManager :: ConnManager
  }

-- | Configuration for 'AsyncHttp2Transport'.
type Http2TransportOptions :: Type
data Http2TransportOptions = Http2TransportOptions
  { -- | Body encoding for outgoing envelopes.
    compression :: Compression,
    -- | Whether to validate the TLS server certificate.
    --
    -- Defaults to @True@.  Set to @False@ only for development against
    -- self-signed certificates.
    validateCert :: Bool,
    -- | How long to wait for the HTTP\/2 handshake (TCP connect + server
    -- SETTINGS frame) before giving up, in microseconds.
    --
    -- Defaults to 30 seconds.
    connectTimeout :: Int,
    -- | Policy to apply after a failed connection attempt.
    --
    -- The policy is an @'IO' 'ReconnectDecision'@ action that runs on the
    -- worker thread; blocking inside it (e.g. 'reconnectAfter') applies
    -- back-pressure without a separate retry loop.
    --
    -- Defaults to @pure 'DontReconnect'@: give up after the first failure
    -- so the transport never silently hammers an unreachable endpoint.
    -- Use 'reconnectAfter' or 'exponentialBackoff' for retry behaviour.
    reconnectPolicy :: IO ReconnectDecision
  }

-- | Defaults: 'Gzip' compression, TLS certificate validation enabled,
-- 30-second connect timeout, no reconnect on failure.
instance Default Http2TransportOptions where
  def =
    Http2TransportOptions
      { compression = def,
        validateCert = True,
        connectTimeout = 30_000_000,
        reconnectPolicy = pure DontReconnect
      }

-- | Create a 'TransportProvider' that will build an 'AsyncHttp2Transport' when
-- called as part of 'Sentry.Client.new'.
--
-- Pass 'AsyncExecutor.defaultQueueSize' for @queueSize@ unless you have a
-- specific reason to tune it.
new :: Http2TransportOptions -> Int -> TransportProvider
new http2Opts queueSize = DeferredTransport \dsn clientOpts -> do
  clientReports <-
    if clientOpts.sendClientReports
      then Just <$> ClientReport.new
      else pure Nothing
  SomeTransport <$> build http2Opts clientReports queueSize dsn

-- | Build an 'AsyncHttp2Transport' directly, bypassing the 'TransportProvider'.
--
-- Use this when you need a transport handle directly (e.g. for testing or
-- profiling).
build ::
  Http2TransportOptions ->
  Maybe ClientReports ->
  Int ->
  Patrol.Dsn ->
  IO AsyncHttp2Transport
build opts clientReports queueSize dsn = do
  let toEnvelope report =
        Patrol.Envelope.Envelope
          { Patrol.Envelope.headers =
              Patrol.Headers.empty{Patrol.Headers.dsn = Just dsn},
            Patrol.Envelope.items =
              Patrol.Items.EnvelopeItems [Patrol.Item.ClientReport report]
          }
      reportConfig = fmap (\cr -> ClientReportConfig{accumulator = cr, toEnvelope}) clientReports
      connTarget = Connection.connTargetFromDsn opts.compression dsn
  connManager <- Connection.newConnManager connTarget opts.validateCert opts.connectTimeout opts.reconnectPolicy
  let sendFn envelope rateLimiter = do
        now <- getCurrentTime
        outcome <- Connection.sendEnvelope connManager envelope
        for_ (Delivery.discardReason outcome) \reason ->
          for_ (fmap (.accumulator) reportConfig) \cr ->
            ClientReport.recordEnvelopeDrop cr reason envelope
        pure $ RateLimiter.updateFromResponse rateLimiter now outcome
  executor <- AsyncExecutor.new queueSize reportConfig sendFn
  pure AsyncHttp2Transport{executor, connManager}

instance Transport AsyncHttp2Transport where
  send t = AsyncExecutor.send t.executor
  flush t = AsyncExecutor.flush t.executor

  -- Delegate to the executor's shutdown, then close the HTTP/2 connection.
  -- The connection is closed unconditionally (even on timeout) to free
  -- network resources.
  shutdown t timeout = do
    result <- AsyncExecutor.shutdown t.executor timeout
    Connection.closeConnManager t.connManager
    pure result

  recordDiscards t = AsyncExecutor.recordDiscards t.executor
