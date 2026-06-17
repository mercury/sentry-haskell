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
    httpDiscardReason,
    -- * Re-exports
    Compression (..),
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Atomics (atomicModifyIORefCAS_)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (fold, for_)
import Data.Functor (void)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Kind (Type)
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Types qualified as HttpTypes
import OpenTelemetry.Instrumentation.HttpClient (HttpClientInstrumentationConfig)
import OpenTelemetry.Instrumentation.HttpClient qualified as HttpClient
import Patrol qualified
import Patrol.Type.DataCategory (DataCategory)
import Sentry.ClientReport (ClientReports, DiscardReason)
import Sentry.ClientReport qualified as ClientReport
import Sentry.Transport (Transport (..))
import Sentry.Transport qualified as Sentry.Transport
import Sentry.Transport.Executor.RateLimiter (RateLimiter)
import Sentry.Transport.Executor.RateLimiter qualified as RateLimiter
import Sentry.Transport.HTTP.Request (Compression (..), PreparedRequest)
import Sentry.Transport.HTTP.Request qualified as Request
import UnliftIO.Exception (handle, toException)

-- | A synchronous HTTP transport that blocks on each send.
type SyncHttpTransport :: Type
data SyncHttpTransport = SyncHttpTransport
  { rateLimiter :: IORef RateLimiter,
    sendFn :: Patrol.Envelope -> IO (Either HttpClient.HttpExceptionContent (HttpClient.Response ())),
    -- | Per-envelope drop accumulator; 'Nothing' when client reports are
    -- disabled (constructed with @sendClientReports = False@).
    clientReports :: Maybe ClientReports
  }

-- | Create a new 'SyncHttpTransport' with a fresh 'RateLimiter'.
--
-- When @sendClientReports@ is @True@ the transport will piggyback a
-- 'Patrol.Type.ClientReport.ClientReport' item onto every outgoing envelope
-- (using a forced drain so that even one-shot CLI processes flush their
-- report). Set to @False@ to opt out entirely.
--
-- Note: @sendClientReports@ is a separate parameter from
-- 'Sentry.Client.Options.ClientOptions.sendClientReports'; callers are
-- responsible for keeping the two consistent.
--
-- The HTTP request template (URL, auth, content-type, user-agent) is built
-- once here and reused for every envelope; only the body is swapped per send.
--
-- If OpenTelemetry instrumentation is not needed, pass 'Nothing' for the
-- configuration and the default (no-op) instrumentation will be used.
--
-- Pass 'def' for 'Compression' to use the default ('Gzip').
new ::
  Compression ->
  Bool ->
  HttpClient.Manager ->
  Maybe HttpClientInstrumentationConfig ->
  Patrol.Dsn ->
  IO SyncHttpTransport
new compression sendClientReports manager otelConfig dsn = do
  rateLimiter <- newIORef RateLimiter.new
  clientReports <-
    if sendClientReports
      then Just <$> ClientReport.new
      else pure Nothing
  let template = Request.prepare compression dsn
      sendFn = sendEnvelope manager (fold otelConfig) template
  pure SyncHttpTransport{rateLimiter, sendFn, clientReports}

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
sendEnvelope manager otelConfig prepared envelope =
  -- Network-level failures (connection refused, DNS, timeout, TLS) throw an
  -- 'HttpException'; fold them into the 'Left' branch so callers (notably the
  -- async worker thread) never see a thrown exception. Status-code errors are
  -- already values because the prepared request leaves @http-client@'s
  -- status-code checking as a no-op.
  liftIO . handle (pure . Left . exceptionContent) $ do
    let request = Request.attach prepared envelope
    response <- HttpClient.httpLbs otelConfig request manager
    let status = HttpTypes.statusCode . HttpClient.responseStatus $ response
        responseNoBody = void response
    pure $
      if 200 <= status && status < 300
        then Right responseNoBody
        else
          let chunk = LBS.toStrict . LBS.take 1024 . HttpClient.responseBody $ response
           in Left $ HttpClient.StatusCodeException responseNoBody chunk
{-# INLINEABLE sendEnvelope #-}

-- | Project an 'HttpException' down to its 'HttpClient.HttpExceptionContent'.
--
-- 'InvalidUrlException' carries no content (it is a configuration error rather
-- than a transport failure), so it is wrapped as an 'HttpClient.InternalException'.
exceptionContent :: HttpClient.HttpException -> HttpClient.HttpExceptionContent
exceptionContent = \case
  HttpClient.HttpExceptionRequest _ content -> content
  e@HttpClient.InvalidUrlException{} -> HttpClient.InternalException (toException e)

-- | Map an envelope send result to the 'DiscardReason' that should be recorded
-- for its items, if any.
--
--   * A 429 records nothing: the rate limiter accounts for it via
--     'RateLimiter.updateFromResponse', and the items are retried, not dropped.
--   * Any other non-2xx status is a 'ClientReport.SendError'.
--   * A transport\/network failure is a 'ClientReport.NetworkError'.
--   * A successful send records nothing.
httpDiscardReason ::
  Either HttpClient.HttpExceptionContent (HttpClient.Response ()) ->
  Maybe DiscardReason
httpDiscardReason = \case
  Right _ -> Nothing
  Left (HttpClient.StatusCodeException resp _)
    | HttpClient.responseStatus resp == HttpTypes.tooManyRequests429 -> Nothing
    | otherwise -> Just ClientReport.SendError
  Left _ -> Just ClientReport.NetworkError
{-# INLINEABLE httpDiscardReason #-}

instance Transport SyncHttpTransport where
  send transport envelope = do
    now <- getCurrentTime
    rl <- readIORef transport.rateLimiter
    let filtered = RateLimiter.filterEnvelope rl now envelope
    -- Account for every item dropped by rate limiting, whether the whole
    -- envelope was filtered out or only some of its items.
    for_ transport.clientReports \cr ->
      ClientReport.recordItemDrops cr ClientReport.RatelimitBackoff filtered.dropped
    case filtered.kept of
      Nothing -> pure Sentry.Transport.SendProcessed
      Just filteredEnvelope -> do
        -- Piggyback any pending client report (forced — no background drainer).
        piggybacked <- case transport.clientReports of
          Nothing -> pure filteredEnvelope
          Just cr -> do
            mReport <- ClientReport.takePending cr now True
            pure $ maybe filteredEnvelope (`ClientReport.attach` filteredEnvelope) mReport
        result <- transport.sendFn piggybacked
        -- Record send/network failures (a 429 is accounted for by the rate
        -- limiter, not as a drop) against the pre-piggyback envelope.
        for_ (httpDiscardReason result) \reason ->
          for_ transport.clientReports \cr ->
            ClientReport.recordEnvelopeDrop cr reason filteredEnvelope
        atomicModifyIORefCAS_ transport.rateLimiter \current ->
          RateLimiter.updateFromResponse current now result
        pure Sentry.Transport.SendProcessed

  recordDiscards :: SyncHttpTransport -> DiscardReason -> DataCategory -> Int -> IO ()
  recordDiscards transport reason category n =
    for_ transport.clientReports \reports ->
      ClientReport.record reports reason category n
