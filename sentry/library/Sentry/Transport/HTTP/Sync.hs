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
    HttpTransportOptions (..),
    new,
    build,
    sendEnvelope,
    toOutcome,

    -- * Re-exports
    Compression (..),
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Atomics (atomicModifyIORefCAS_)
import Data.ByteString.Lazy qualified as LBS
import Data.Default (Default (def))
import Data.Foldable (for_)
import Data.Functor (void)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Kind (Type)
import Data.Text qualified as Text
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Types qualified as HttpTypes
import OpenTelemetry.Instrumentation.HttpClient (HttpClientInstrumentationConfig)
import OpenTelemetry.Instrumentation.HttpClient qualified as HttpClient
import Patrol qualified
import Patrol.Type.DataCategory (DataCategory)
import Sentry.Client.Options (ClientOptions (..), TransportProvider (..))
import Sentry.ClientReport (ClientReports, DiscardReason)
import Sentry.ClientReport qualified as ClientReport
import Sentry.Transport (SomeTransport (..), Transport (..))
import Sentry.Transport qualified as Sentry.Transport
import Sentry.Transport.Delivery qualified as Delivery
import Sentry.Transport.Executor.RateLimiter (RateLimiter)
import Sentry.Transport.Executor.RateLimiter qualified as RateLimiter
import Sentry.Transport.HTTP.Request (Compression (..), PreparedRequest)
import Sentry.Transport.HTTP.Request qualified as Request
import UnliftIO.Exception (handle, toException)
import Witch qualified

-- | A synchronous HTTP transport that blocks on each send.
type SyncHttpTransport :: Type
data SyncHttpTransport = SyncHttpTransport
  { rateLimiter :: IORef RateLimiter,
    sendFn :: Patrol.Envelope -> IO (Either HttpClient.HttpExceptionContent (HttpClient.Response ())),
    clientReports :: Maybe ClientReports
  }

-- | Shared configuration options for HTTP transports.
--
-- Use 'def' to get the defaults and override individual fields as needed, or
-- use 'Witch.from' to start from an existing 'HttpClient.Manager':
--
-- > SyncHttpTransport.new def
-- > SyncHttpTransport.new (Witch.from manager)
-- > SyncHttpTransport.new def{compression = NoCompression}
-- > AsyncHttpTransport.new def AsyncExecutor.defaultQueueSize
type HttpTransportOptions :: Type
data HttpTransportOptions = HttpTransportOptions
  { -- | Body encoding for outgoing envelopes.
    compression :: Compression,
    -- | HTTP client connection manager.
    manager :: Maybe HttpClient.Manager,
    -- | OpenTelemetry instrumentation config.
    instrumentation :: HttpClientInstrumentationConfig
  }

-- | Defaults: 'Gzip' compression, global HTTP manager, no OTel instrumentation.
instance Default HttpTransportOptions where
  def =
    HttpTransportOptions
      { compression = def,
        manager = Nothing,
        instrumentation = mempty
      }

-- | Construct 'HttpTransportOptions' from an existing 'HttpClient.Manager'.
instance Witch.From HttpClient.Manager HttpTransportOptions where
  from manager = def{manager = Just manager}

-- | Create a 'TransportProvider' that will build a 'SyncHttpTransport' when
-- called as part of 'Client.new'
--
-- When no 'HttpClient.Manager' is provided, fall back to the global manager.
new :: HttpTransportOptions -> TransportProvider
new httpOpts = DeferredTransport \dsn clientOpts -> do
  manager <- maybe getGlobalManager pure httpOpts.manager
  clientReports <-
    if clientOpts.sendClientReports
      then Just <$> ClientReport.new
      else pure Nothing
  SomeTransport <$> build httpOpts clientReports manager dsn

-- | Build a 'SyncHttpTransport' directly, bypassing 'TransportProvider'.
--
-- Use this when you need a transport handle directly (e.g. for testing,
-- profiling, or as the send function for an async executor).
--
-- The HTTP request template is built once from @dsn@ and reused for every
-- envelope; the body is attached on each send call.
build ::
  HttpTransportOptions ->
  Maybe ClientReports ->
  HttpClient.Manager ->
  Patrol.Dsn ->
  IO SyncHttpTransport
build opts clientReports manager dsn = do
  rateLimiter <- newIORef RateLimiter.new
  let template = Request.prepare opts.compression dsn
      sendFn = sendEnvelope manager opts.instrumentation template
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
  -- async worker thread) never see a thrown exception.
  --
  -- Status-code errors are already values because the prepared request sets
  -- @http-client@'s status-code check to a no-op.
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

-- | Convert an @http-client@ result to a transport-neutral 'Outcome'.
--
-- Both @2xx@ and non-2xx responses fold into 'Responded'; connection-level
-- errors become 'NetworkFailure'.
toOutcome ::
  Either HttpClient.HttpExceptionContent (HttpClient.Response ()) ->
  Delivery.Outcome
toOutcome = \case
  Right response ->
    Delivery.Responded
      (HttpClient.responseStatus response)
      (HttpClient.responseHeaders response)
  Left (HttpClient.StatusCodeException response _) ->
    Delivery.Responded
      (HttpClient.responseStatus response)
      (HttpClient.responseHeaders response)
  Left other ->
    Delivery.NetworkFailure (Text.pack (show other))
{-# INLINEABLE toOutcome #-}

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
        outcome <- toOutcome <$> transport.sendFn piggybacked
        -- Record send/network failures (a 429 is accounted for by the rate
        -- limiter, not as a drop) against the pre-piggyback envelope.
        for_ (Delivery.discardReason outcome) \reason ->
          for_ transport.clientReports \cr ->
            ClientReport.recordEnvelopeDrop cr reason filteredEnvelope
        atomicModifyIORefCAS_ transport.rateLimiter \current ->
          RateLimiter.updateFromResponse current now outcome
        pure Sentry.Transport.SendProcessed

  recordDiscards :: SyncHttpTransport -> DiscardReason -> DataCategory -> Int -> IO ()
  recordDiscards transport reason category n =
    for_ transport.clientReports \reports ->
      ClientReport.record reports reason category n
