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
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Client.TLS (getGlobalManager)
import OpenTelemetry.Instrumentation.HttpClient qualified as HttpClient
import Patrol qualified
import Sentry.Client.Options (ClientOptions (..), TransportProvider (..))
import Sentry.ClientReport (ClientReports)
import Sentry.ClientReport qualified as ClientReport
import Sentry.Discard qualified as Discard
import Sentry.Transport (SomeTransport (..), Transport (..))
import Sentry.Transport.Encoding (Compression (..))
import Sentry.Transport.Executor.Async (AsyncExecutor)
import Sentry.Transport.Executor.Async qualified as AsyncExecutor
import Sentry.Transport.HTTP.Delivery qualified as HTTPDelivery
import Sentry.Transport.HTTP.Request qualified as Request
import Sentry.Transport.HTTP.Sync (HttpTransportOptions (..), sendRequest)

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
      then Just <$> (getCurrentTime >>= ClientReport.new)
      else pure Nothing
  SomeTransport <$> build httpOpts clientReports clientOpts.onDiscard queueSize manager dsn

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
  Maybe Discard.Callback ->
  Int ->
  HttpClient.Manager ->
  Patrol.Dsn ->
  IO AsyncHttpTransport
build opts clientReports onDiscard queueSize manager dsn = do
  let reportConfig = fmap (\cr -> AsyncExecutor.clientReportConfig cr dsn) clientReports
      template = Request.prepare dsn
      sendFn envelope = do
        outcome <- opts.wrapSender opts.compression (sendRequest manager opts.instrumentation . Request.attach template) envelope
        pure $ HTTPDelivery.interpret outcome
  executor <- AsyncExecutor.new queueSize reportConfig onDiscard sendFn
  pure AsyncHttpTransport{executor}

instance Transport AsyncHttpTransport where
  send t = AsyncExecutor.send t.executor
  flush t = AsyncExecutor.flush t.executor
  shutdown t = AsyncExecutor.shutdown t.executor
  recordDiscards t = AsyncExecutor.recordDiscards t.executor
