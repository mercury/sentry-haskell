-- | Asynchronous HTTP transport for Sentry.
--
-- This transport wraps an 'AsyncExecutor' with HTTP delivery via
-- 'Sentry.Transport.HTTP.Sync.sendEnvelope'. Envelopes are queued and sent
-- on a dedicated worker thread with rate limiting handled automatically.
module Sentry.Transport.HTTP.Async
  ( -- * Async HTTP Transport
    AsyncHttpTransport (..),
    new,
    build,

    -- * Re-exports
    Compression (..),
    HttpTransportOptions (..),
  )
where

import Data.Kind (Type)
import Network.HTTP.Client.TLS (getGlobalManager)
import OpenTelemetry.Instrumentation.HttpClient qualified as HttpClient
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
import Sentry.Transport.HTTP.Request qualified as Request
import Sentry.Transport.HTTP.Sync (HttpTransportOptions (..), sendEnvelope, toOutcome)

-- | An asynchronous HTTP transport backed by an 'AsyncExecutor'.
type AsyncHttpTransport :: Type
data AsyncHttpTransport = AsyncHttpTransport
  { executor :: AsyncExecutor
  }

-- | Create a 'TransportProvider' that will build an 'AsyncHttpTransport' when
-- called as part of 'Client.new'
--
-- Pass 'Data.Default.def' for @executorOpts@ for the serial defaults, or
-- override the queue size\/concurrency (e.g. @def{concurrency = 8}@ to fan out
-- sends across the connection pool).
new :: HttpTransportOptions -> ExecutorOptions -> TransportProvider
new httpOpts executorOpts = DeferredTransport \dsn clientOpts -> do
  manager <- maybe getGlobalManager pure httpOpts.manager
  clientReports <-
    if clientOpts.sendClientReports
      then Just <$> ClientReport.new
      else pure Nothing
  SomeTransport <$> build httpOpts clientReports executorOpts manager dsn

-- | Build an 'AsyncHttpTransport' directly, bypassing the 'TransportProvider'.
--
-- Use this when you need a transport handle directly (e.g. for testing or
-- profiling).
--
-- Pass 'Data.Default.def' for @executorOpts@ for the serial defaults, or
-- override the queue size\/concurrency.
build ::
  HttpTransportOptions ->
  Maybe ClientReports ->
  ExecutorOptions ->
  HttpClient.Manager ->
  Patrol.Dsn ->
  IO AsyncHttpTransport
build opts clientReports executorOpts manager dsn = do
  let toEnvelope report =
        Patrol.Envelope.Envelope
          { Patrol.Envelope.headers =
              Patrol.Headers.empty{Patrol.Headers.dsn = Just dsn},
            Patrol.Envelope.items =
              Patrol.Items.EnvelopeItems [Patrol.Item.ClientReport report]
          }
      reportConfig = fmap (\cr -> ClientReportConfig{accumulator = cr, toEnvelope}) clientReports
      template = Request.prepare opts.compression dsn
      sendFn envelope = toOutcome <$> sendEnvelope manager opts.instrumentation template envelope
  executor <- AsyncExecutor.new executorOpts reportConfig sendFn
  pure AsyncHttpTransport{executor}

instance Transport AsyncHttpTransport where
  send t = AsyncExecutor.send t.executor
  flush t = AsyncExecutor.flush t.executor
  shutdown t = AsyncExecutor.shutdown t.executor
  recordDiscards t = AsyncExecutor.recordDiscards t.executor
