-- | Synchronous HTTP transport for Sentry.
--
-- This transport sends envelopes synchronously using @http-client@, instrumented
-- with OpenTelemetry. Each call to 'send' blocks until the HTTP request
-- completes.
--
-- This is the simplest transport implementation and is useful for:
--
-- * CLI tools and short-lived processes
-- * Situations where you want delivery confirmation before proceeding
-- * As the underlying send function for 'Sentry.Transport.Executor.Async'
module Sentry.Transport.HTTP.Sync
  ( -- * Sync HTTP Transport
    SyncHttpTransport (..),
    new,
    sendEnvelope,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Atomics (atomicModifyIORefCAS_)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (fold)
import Data.Functor (void)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Kind (Type)
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Types qualified as HttpTypes
import OpenTelemetry.Instrumentation.HttpClient (HttpClientInstrumentationConfig)
import OpenTelemetry.Instrumentation.HttpClient qualified as HttpClient
import Patrol qualified
import Sentry.Transport (Transport (..))
import Sentry.Transport qualified as Sentry.Transport
import Sentry.Transport.Executor.RateLimiter (RateLimiter)
import Sentry.Transport.Executor.RateLimiter qualified as RateLimiter
import Sentry.Transport.HTTP.Request (PreparedRequest)
import Sentry.Transport.HTTP.Request qualified as Request

-- | A synchronous HTTP transport that blocks on each send.
type SyncHttpTransport :: Type
data SyncHttpTransport = SyncHttpTransport
  { rateLimiter :: IORef RateLimiter,
    sendFn :: Patrol.Envelope -> IO (Either HttpClient.HttpExceptionContent (HttpClient.Response ()))
  }

-- | Create a new 'SyncHttpTransport' with a fresh 'RateLimiter'.
--
-- The HTTP request template (URL, auth, content-type, user-agent) is built
-- once here and reused for every envelope; only the body is swapped per send.
--
-- If OpenTelemetry instrumentation is not needed, pass 'Nothing' for the
-- configuration and the default (no-op) instrumentation will be used.
new ::
  HttpClient.Manager ->
  Maybe HttpClientInstrumentationConfig ->
  Patrol.Dsn ->
  IO SyncHttpTransport
new manager otelConfig dsn = do
  rateLimiter <- newIORef RateLimiter.new
  let template = Request.prepare dsn
      sendFn = sendEnvelope manager (fold otelConfig) template
  pure SyncHttpTransport{rateLimiter, sendFn}

-- | Send an envelope via HTTP, returning the response.
--
-- This is exposed for use as a building block (e.g. by the async executor's
-- send function, which needs to inspect the response for rate limit headers).
sendEnvelope ::
  (MonadIO m) =>
  HttpClient.Manager ->
  HttpClientInstrumentationConfig ->
  PreparedRequest ->
  Patrol.Envelope ->
  m (Either HttpClient.HttpExceptionContent (HttpClient.Response ()))
sendEnvelope manager otelConfig prepared envelope = do
  let request = Request.attach prepared envelope
  response <- liftIO $ HttpClient.httpLbs otelConfig request manager
  let status = HttpTypes.statusCode . HttpClient.responseStatus $ response
      responseNoBody = void response
  pure $
    if 200 <= status && status < 300
      then Right responseNoBody
      else
        let chunk = LBS.toStrict . LBS.take 1024 . HttpClient.responseBody $ response
         in Left $ HttpClient.StatusCodeException responseNoBody chunk
{-# INLINEABLE sendEnvelope #-}

instance Transport SyncHttpTransport where
  send transport envelope = do
    now <- getCurrentTime
    rl <- readIORef transport.rateLimiter
    case RateLimiter.filterEnvelope rl now envelope of
      Nothing -> pure ()
      Just filteredEnvelope -> do
        result <- transport.sendFn filteredEnvelope
        atomicModifyIORefCAS_ transport.rateLimiter \current ->
          RateLimiter.updateFromResponse current now result
    pure Sentry.Transport.SendProcessed
