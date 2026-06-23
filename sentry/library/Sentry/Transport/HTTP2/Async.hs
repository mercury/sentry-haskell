-- | Asynchronous HTTP\/2 transport for Sentry.
--
-- @stability: experimental@
--
-- __Status:__ this transport is under active development and has not been as
-- thoroughly tested as the HTTP\/1.1 transports; it makes no API or runtime
-- stability guarantees.
--
-- Prefer the HTTP\/1.1 transport unless you specifically want a single
-- multiplexed connection, for example to minimise sockets and handshakes on
-- bursty, high-RTT links.
--
-- A single long-lived HTTP\/2 TLS connection is maintained for the life of the
-- transport; it is is established when the first envelope is sent, and
-- reconnected transparently if the server closes it.
--
-- TLS certificate validation is enabled by default and can be disabled via
-- 'Http2TransportOptions.validateCert' **for testing only**.
module Sentry.Transport.HTTP2.Async
  {-# WARNING in "x-sentry-experimental" "The HTTP/2 transport is experimental and it makes no API or runtime stability guarantees, please use the HTTP/1.1 transport instead. Silence with -Wno-x-sentry-experimental." #-}
  ( -- * Async HTTP\/2 Transport
    AsyncHttp2Transport (..),
    new,
    build,

    -- * Options
    Http2TransportOptions (..),

    -- * Re-exports
    Compression (..),
    Http2Settings (..),
    ReconnectDecision (..),
    reconnectAfter,
    exponentialBackoff,
  )
where

import Data.Default (Default (def))
import Data.Kind (Type)
import Patrol qualified
import Patrol.Type.Envelope qualified as Patrol.Envelope
import Patrol.Type.Headers qualified as Patrol.Headers
import Patrol.Type.Item qualified as Patrol.Item
import Patrol.Type.Items qualified as Patrol.Items
import Sentry.Client.Options (ClientOptions (..), TransportProvider (..))
import Sentry.ClientReport (ClientReports)
import Sentry.ClientReport qualified as ClientReport
import Sentry.Transport (SomeTransport (..), Transport (..))
import Sentry.Transport.Executor.Async (AsyncExecutor, ClientReportConfig (..), ExecutorOptions)
import Sentry.Transport.Executor.Async qualified as AsyncExecutor
import Sentry.Transport.HTTP.Request (Compression (..))
import Sentry.Transport.HTTP2.Connection (Http2Settings (..), ReconnectDecision (..), exponentialBackoff, reconnectAfter)
import Sentry.Transport.HTTP2.Connection qualified as Connection

-- | An asynchronous HTTP\/2 transport backed by an 'AsyncExecutor'.
type AsyncHttp2Transport :: Type
data AsyncHttp2Transport = AsyncHttp2Transport
  { executor :: AsyncExecutor,
    -- | The underlying connection manager; closed after the executor shuts down.
    manager :: Connection.Manager
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
    -- The policy is an @'IO' 'ReconnectDecision'@ action that /describes/ how
    -- long to back off ('ReconnectAfter') or whether to give up
    -- ('DontReconnect').
    --
    -- Defaults to @pure 'DontReconnect'@: give up after the first failure
    -- so the transport never silently hammers an unreachable endpoint.
    --
    -- Use 'reconnectAfter' or 'exponentialBackoff' for retry behavior.
    reconnectPolicy :: IO ReconnectDecision,
    -- | HTTP\/2 protocol-level settings (flow-control windows, rate-limit
    -- overrides, TCP_NODELAY).
    --
    -- Defaults to 'def': TCP_NODELAY enabled, ping rate limit raised to
    -- 100\/s, all other knobs at @http2-tls@ library defaults.
    http2Settings :: Http2Settings
  }

-- | Defaults: 'Gzip' compression, TLS certificate validation enabled,
-- 30-second connect timeout, no reconnect on failure, 'def' HTTP\/2 settings.
instance Default Http2TransportOptions where
  def =
    Http2TransportOptions
      { compression = def,
        validateCert = True,
        connectTimeout = 30_000_000,
        reconnectPolicy = pure DontReconnect,
        http2Settings = def
      }

-- | Create a 'TransportProvider' that will build an 'AsyncHttp2Transport' when
-- called as part of 'Sentry.Client.new'.
--
-- Pass 'Data.Default.def' for @executorOpts@ for the serial defaults, or
-- override the queue size\/concurrency (e.g. @def{concurrency = 8}@ to fan out
-- sends across the multiplexed connection).
new :: Http2TransportOptions -> ExecutorOptions -> TransportProvider
new http2Opts executorOpts = DeferredTransport \dsn clientOpts -> do
  clientReports <-
    if clientOpts.sendClientReports
      then Just <$> ClientReport.new
      else pure Nothing
  SomeTransport <$> build http2Opts clientReports executorOpts dsn

-- | Build an 'AsyncHttp2Transport' directly, bypassing the 'TransportProvider'.
--
-- Use this when you need a transport handle directly (e.g. for testing or
-- profiling).
build ::
  Http2TransportOptions ->
  Maybe ClientReports ->
  ExecutorOptions ->
  Patrol.Dsn ->
  IO AsyncHttp2Transport
build opts clientReports executorOpts dsn = do
  let toEnvelope report =
        Patrol.Envelope.Envelope
          { Patrol.Envelope.headers =
              Patrol.Headers.empty{Patrol.Headers.dsn = Just dsn},
            Patrol.Envelope.items =
              Patrol.Items.EnvelopeItems [Patrol.Item.ClientReport report]
          }
      reportConfig = fmap (\cr -> ClientReportConfig{accumulator = cr, toEnvelope}) clientReports
      endpoint = Connection.mkEndpoint opts.compression dsn
  manager <- Connection.newManager endpoint opts.validateCert opts.connectTimeout opts.http2Settings opts.reconnectPolicy
  let sendFn = Connection.sendEnvelope manager
  executor <- AsyncExecutor.new executorOpts reportConfig sendFn
  pure AsyncHttp2Transport{executor, manager}

instance Transport AsyncHttp2Transport where
  send t = AsyncExecutor.send t.executor
  flush t = AsyncExecutor.flush t.executor

  -- Delegate to the executor's shutdown, then close the HTTP/2 connection.
  -- The connection is closed unconditionally (even on timeout) to free
  -- network resources.
  shutdown t timeout = do
    result <- AsyncExecutor.shutdown t.executor timeout
    Connection.closeManager t.manager
    pure result

  recordDiscards t = AsyncExecutor.recordDiscards t.executor
