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
import Sentry.Client.Options (ClientOptions (..), TransportProvider (..))
import Sentry.ClientReport (ClientReports)
import Sentry.ClientReport qualified as ClientReport
import Sentry.Transport (SomeTransport (..), Transport (..))
import Sentry.Transport.Encoding (Compression (..))
import Sentry.Transport.Executor.Async (AsyncExecutor)
import Sentry.Transport.Executor.Async qualified as AsyncExecutor
import Sentry.Transport.HTTP.Delivery qualified as HTTPDelivery
import Sentry.Transport.HTTP.Request qualified as Request
import Sentry.Transport.HTTP.Sync (HttpTransportOptions (..), sendEnvelope)

-- | An asynchronous HTTP transport backed by an 'AsyncExecutor'.
type AsyncHttpTransport :: Type
data AsyncHttpTransport = AsyncHttpTransport
  { executor :: AsyncExecutor
  }

-- | Create a 'TransportProvider' that will build an 'AsyncHttpTransport' when
-- called as part of 'Client.new'
--
-- Pass 'AsyncExecutor.defaultQueueSize' for @queueSize@ unless you have a
-- specific reason to tune it.
new :: HttpTransportOptions -> Int -> TransportProvider
new httpOpts queueSize = DeferredTransport \dsn clientOpts -> do
  manager <- maybe getGlobalManager pure httpOpts.manager
  clientReports <-
    if clientOpts.sendClientReports
      then Just <$> ClientReport.new
      else pure Nothing
  SomeTransport <$> build httpOpts clientReports queueSize manager dsn

-- | Build an 'AsyncHttpTransport' directly, bypassing the 'TransportProvider'.
--
-- Use this when you need a transport handle directly (e.g. for testing or
-- profiling).
--
-- Pass 'AsyncExecutor.defaultQueueSize' for @queueSize@ unless you have a
-- specific reason to tune it.
build ::
  HttpTransportOptions ->
  Maybe ClientReports ->
  Int ->
  HttpClient.Manager ->
  Patrol.Dsn ->
  IO AsyncHttpTransport
build opts clientReports queueSize manager dsn = do
  let reportConfig = fmap (\cr -> AsyncExecutor.clientReportConfig cr dsn) clientReports
      template = Request.prepare dsn
      sendFn envelope = do
        outcome <- sendEnvelope manager opts.instrumentation template opts.compression envelope
        HTTPDelivery.interpretNow outcome
  executor <- AsyncExecutor.new queueSize reportConfig sendFn
  pure AsyncHttpTransport{executor}

instance Transport AsyncHttpTransport where
  send t = AsyncExecutor.send t.executor
  flush t = AsyncExecutor.flush t.executor
  shutdown t = AsyncExecutor.shutdown t.executor
  recordDiscards t = AsyncExecutor.recordDiscards t.executor
