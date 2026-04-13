-- | Asynchronous HTTP transport for Sentry.
--
-- This transport wraps an 'AsyncExecutor' with HTTP delivery via
-- 'Sentry.Transport.HTTP.Sync.sendEnvelope'. Envelopes are queued and sent
-- on a dedicated worker thread with rate limiting handled automatically.
module Sentry.Transport.HTTP.Async
  ( -- * Async HTTP Transport
    AsyncHttpTransport (..),
    new,
  )
where

import Data.Foldable (fold)
import Data.Kind (Type)
import Data.Time.Clock (getCurrentTime)
import OpenTelemetry.Instrumentation.HttpClient (HttpClientInstrumentationConfig)
import OpenTelemetry.Instrumentation.HttpClient qualified as HttpClient
import Patrol qualified
import Sentry.Transport (Transport (..))
import Sentry.Transport.Executor.Async (AsyncExecutor)
import Sentry.Transport.Executor.Async qualified as AsyncExecutor
import Sentry.Transport.Executor.RateLimiter qualified as RateLimiter
import Sentry.Transport.HTTP.Request qualified as Request
import Sentry.Transport.HTTP.Sync (sendEnvelope)

-- | An asynchronous HTTP transport backed by an 'AsyncExecutor'.
type AsyncHttpTransport :: Type
data AsyncHttpTransport = AsyncHttpTransport
  { executor :: AsyncExecutor
  }

-- | Create a new 'AsyncHttpTransport'.
--
-- If OpenTelemetry instrumentation is not needed, pass 'Nothing' for the
-- configuration and the default (no-op) instrumentation will be used.
new ::
  Int ->
  HttpClient.Manager ->
  Maybe HttpClientInstrumentationConfig ->
  Patrol.Dsn ->
  IO AsyncHttpTransport
new queueSize manager otelConfig dsn = do
  let template = Request.prepare dsn
      sendFn envelope rateLimiter = do
        now <- getCurrentTime
        result <- sendEnvelope manager (fold otelConfig) template envelope
        pure $ RateLimiter.updateFromResponse rateLimiter now result
  executor <- AsyncExecutor.new queueSize sendFn
  pure AsyncHttpTransport{executor}

instance Transport AsyncHttpTransport where
  send t = AsyncExecutor.send t.executor
  flush t = AsyncExecutor.flush t.executor
  shutdown t = AsyncExecutor.shutdown t.executor
