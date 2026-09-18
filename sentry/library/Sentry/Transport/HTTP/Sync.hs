-- | Synchronous HTTP transport for Sentry.
--
-- This transport sends envelopes synchronously using @http-client@, instrumented
-- with OpenTelemetry. Each call to 'send' blocks until the HTTP request
-- completes.
--
-- This is the simplest transport implementation and is useful for:
--
-- * CLI tools and short-lived processes
-- * Situations where you want HTTP acknowledgement or failure before proceeding
-- * As the underlying send function for 'Sentry.Transport.Executor.Async'
module Sentry.Transport.HTTP.Sync
  ( -- * Sync HTTP Transport
    SyncHttpTransport (..),
    HttpTransportOptions (..),
    new,
    build,
    buildWithSender,
    sendEnvelope,
    sendRequest,

    -- * Re-exports
    Compression (..),
  )
where

import Control.Exception (evaluate, try)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Atomics (atomicModifyIORefCAS_)
import Data.ByteString qualified as BS
import Data.Default (Default (def))
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Kind (Type)
import Data.Text qualified as Text
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Client.TLS (getGlobalManager)
import OpenTelemetry.Instrumentation.HttpClient (HttpClientInstrumentationConfig)
import OpenTelemetry.Instrumentation.HttpClient qualified as HttpClient
import Patrol qualified
import Patrol.Type.DataCategory (DataCategory)
import Sentry.Client.Options (ClientOptions (..), TransportProvider (..))
import Sentry.ClientReport (ClientReports, DiscardReason)
import Sentry.ClientReport qualified as ClientReport
import Sentry.Discard qualified as Discard
import Sentry.Transport (SomeTransport (..), Transport (..))
import Sentry.Transport qualified as Sentry.Transport
import Sentry.Transport.Delivery qualified as Delivery
import Sentry.Transport.Encoding (Compression (..))
import Sentry.Transport.Encoding qualified as Encoding
import Sentry.Transport.Executor.RateLimiter (RateLimiter)
import Sentry.Transport.Executor.RateLimiter qualified as RateLimiter
import Sentry.Transport.HTTP.Delivery qualified as HTTPDelivery
import Sentry.Transport.HTTP.Request (PreparedRequest)
import Sentry.Transport.HTTP.Request qualified as Request
import UnliftIO.Exception (handle)
import Witch qualified

-- | A synchronous HTTP transport that blocks on each send.
type SyncHttpTransport :: Type
data SyncHttpTransport = SyncHttpTransport
  { rateLimiter :: IORef RateLimiter,
    sendFn :: Patrol.Envelope -> IO HTTPDelivery.Outcome,
    clientReports :: Maybe ClientReports,
    onDiscard :: Maybe Discard.Callback
  }

-- | Shared configuration options for HTTP transports.
--
-- Use 'def' to get the defaults and override individual fields as needed, or
-- use 'Witch.from' to start from an existing 'HttpClient.Manager':
--
-- > SyncHttpTransport.new def
-- > SyncHttpTransport.new (Witch.from manager)
-- > SyncHttpTransport.new def{compression = None}
-- > AsyncHttpTransport.new def AsyncExecutor.defaultQueueSize
type HttpTransportOptions :: Type
data HttpTransportOptions = HttpTransportOptions
  { -- | Body encoding for outgoing envelopes.
    compression :: Compression,
    -- | HTTP client connection manager.
    manager :: Maybe HttpClient.Manager,
    -- | OpenTelemetry instrumentation config.
    instrumentation :: HttpClientInstrumentationConfig,
    -- | Wrap the outgoing envelope send function produced by this transport.
    --
    -- The transport applies this after filtering and attaching reports; set it
    -- to @Instrument.observing report@ to observe delivery attempts.
    --
    -- Callbacks run on the sending thread and must finish promptly.
    --
    -- > def{wrapSender = Instrument.observing report}
    wrapSender ::
      Compression ->
      (Encoding.EncodedBody -> IO HTTPDelivery.Outcome) ->
      Patrol.Envelope ->
      IO HTTPDelivery.Outcome
  }

-- | Defaults: 'Gzip' compression, global HTTP manager, no OTel instrumentation.
instance Default HttpTransportOptions where
  def =
    HttpTransportOptions
      { compression = def,
        manager = Nothing,
        instrumentation = mempty,
        wrapSender = \compression sender envelope ->
          evaluate (Encoding.encode compression envelope) >>= sender
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
      then Just <$> (getCurrentTime >>= ClientReport.new)
      else pure Nothing
  SomeTransport <$> build httpOpts clientReports clientOpts.onDiscard manager dsn

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
  Maybe Discard.Callback ->
  HttpClient.Manager ->
  Patrol.Dsn ->
  IO SyncHttpTransport
build opts clientReports onDiscard manager dsn =
  buildWithSender clientReports onDiscard $
    opts.wrapSender opts.compression (sendRequest manager opts.instrumentation . Request.attach (Request.prepare dsn))

-- | Build a 'SyncHttpTransport' from the given send function.
buildWithSender ::
  Maybe ClientReports ->
  Maybe Discard.Callback ->
  (Patrol.Envelope -> IO HTTPDelivery.Outcome) ->
  IO SyncHttpTransport
buildWithSender clientReports onDiscard sendFn = do
  rateLimiter <- newIORef RateLimiter.new
  pure SyncHttpTransport{rateLimiter, sendFn, clientReports, onDiscard}

-- | Encode and send an envelope, reporting what HTTP had to say about it.
sendEnvelope ::
  (MonadIO m) =>
  HttpClient.Manager ->
  HttpClientInstrumentationConfig ->
  PreparedRequest ->
  Compression ->
  Patrol.Envelope ->
  m HTTPDelivery.Outcome
sendEnvelope manager otelConfig prepared compression envelope =
  sendRequest manager otelConfig (Request.attach prepared (Encoding.encode compression envelope))
{-# INLINEABLE sendEnvelope #-}

sendRequest ::
  (MonadIO m) =>
  HttpClient.Manager ->
  HttpClientInstrumentationConfig ->
  HttpClient.Request ->
  m HTTPDelivery.Outcome
sendRequest manager otelConfig request =
  liftIO $
    handle handleException $
      HttpClient.withResponse' otelConfig request manager handleResponse
  where
    handleResponse :: HttpClient.Response HttpClient.BodyReader -> IO HTTPDelivery.Outcome
    handleResponse response = do
      receivedAt <- getCurrentTime
      -- Headers determine delivery policy even if an HTTP error interrupts
      -- draining. Other exceptions propagate; withResponse' closes the response.
      void $ try @HttpClient.HttpException $ drainBody (HttpClient.responseBody response)
      pure $ HTTPDelivery.Responded receivedAt (HttpClient.responseStatus response) (HttpClient.responseHeaders response)

    handleException :: HttpClient.HttpException -> IO HTTPDelivery.Outcome
    handleException = \case
      HttpClient.HttpExceptionRequest _ (HttpClient.StatusCodeException response _) -> do
        receivedAt <- getCurrentTime
        pure $ HTTPDelivery.Responded receivedAt (HttpClient.responseStatus response) (HttpClient.responseHeaders response)
      HttpClient.HttpExceptionRequest _ content ->
        pure $ HTTPDelivery.NetworkFailure (Text.pack $ show content)
      exception@HttpClient.InvalidUrlException{} ->
        pure $ HTTPDelivery.NetworkFailure (Text.pack $ show exception)

    drainBody :: HttpClient.BodyReader -> IO ()
    drainBody reader = do
      chunk <- HttpClient.brRead reader
      unless (BS.null chunk) (drainBody reader)
{-# INLINEABLE sendRequest #-}

instance Transport SyncHttpTransport where
  send transport envelope = do
    now <- getCurrentTime
    rl <- readIORef transport.rateLimiter
    let filtered = RateLimiter.filterEnvelope rl now envelope
    -- Account for every item dropped by rate limiting, whether the whole
    -- envelope was filtered out or only some of its items.
    Discard.recordItems transport.clientReports transport.onDiscard ClientReport.RatelimitBackoff filtered.dropped
    case filtered.kept of
      Nothing -> pure Sentry.Transport.SendFailed_Other
      Just filteredEnvelope -> do
        -- Piggyback any pending client report (forced — no background drainer).
        piggybacked <- case transport.clientReports of
          Nothing -> pure filteredEnvelope
          Just cr -> do
            mReport <- ClientReport.takePending cr now True
            pure $ maybe filteredEnvelope (`ClientReport.attach` filteredEnvelope) mReport
        httpOutcome <- transport.sendFn piggybacked
        -- The deadline for a relative rate-limit header is dated from the
        -- response, not from the pre-send @now@ used for filtering above.
        let outcome = HTTPDelivery.interpret httpOutcome
        -- Record failures against attempted items, excluding the piggybacked
        -- report. Upstream accounts for HTTP 429 rejections.
        atomicModifyIORefCAS_ transport.rateLimiter \current ->
          RateLimiter.apply current outcome.rateLimits
        Delivery.recordOutcome transport.clientReports transport.onDiscard filteredEnvelope outcome
        pure $ case outcome.disposition of
          Delivery.Accepted -> Sentry.Transport.SendProcessed
          Delivery.Rejected _ -> Sentry.Transport.SendFailed_Other

  recordDiscards :: SyncHttpTransport -> DiscardReason -> DataCategory -> Int -> IO ()
  recordDiscards transport reason category n =
    for_ transport.clientReports \reports ->
      ClientReport.record reports reason category n
