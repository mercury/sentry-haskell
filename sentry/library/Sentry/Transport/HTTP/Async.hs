-- | Asynchronous HTTP transport for Sentry.
--
-- This transport wraps an 'AsyncExecutor' with HTTP delivery via
-- 'Sentry.Transport.HTTP.Sync.sendEnvelope'. Envelopes are queued and sent
-- on a dedicated worker thread with rate limiting handled automatically.
module Sentry.Transport.HTTP.Async
  ( -- * Async HTTP Transport
    AsyncHttpTransport (..),
    new,
    -- * Re-exports
    Compression (..),
  )
where

import Data.Foldable (fold, for_)
import Data.Kind (Type)
import Data.Time.Clock (getCurrentTime)
import OpenTelemetry.Instrumentation.HttpClient (HttpClientInstrumentationConfig)
import OpenTelemetry.Instrumentation.HttpClient qualified as HttpClient
import Patrol qualified
import Patrol.Type.Envelope qualified as Patrol.Envelope
import Patrol.Type.Headers qualified as Patrol.Headers
import Patrol.Type.Item qualified as Patrol.Item
import Patrol.Type.Items qualified as Patrol.Items
import Sentry.ClientReport qualified as ClientReport
import Sentry.Transport (Transport (..))
import Sentry.Transport.Executor.Async (AsyncExecutor, ClientReportConfig (..))
import Sentry.Transport.Executor.Async qualified as AsyncExecutor
import Sentry.Transport.Executor.RateLimiter qualified as RateLimiter
import Sentry.Transport.HTTP.Request (Compression (..))
import Sentry.Transport.HTTP.Request qualified as Request
import Sentry.Transport.HTTP.Sync (httpDiscardReason, sendEnvelope)

-- | An asynchronous HTTP transport backed by an 'AsyncExecutor'.
type AsyncHttpTransport :: Type
data AsyncHttpTransport = AsyncHttpTransport
  { executor :: AsyncExecutor
  }

-- | Create a new 'AsyncHttpTransport'.
--
-- When @sendClientReports@ is @True@ the transport will piggyback
-- 'Patrol.Type.ClientReport.ClientReport' items onto outgoing envelopes
-- (every ≥30 s) and force-drain on flush\/shutdown so locally discarded
-- events are reported to Sentry.  Set to @False@ to opt out entirely.
--
-- Note: @sendClientReports@ is a separate parameter from
-- 'Sentry.Client.Options.ClientOptions.sendClientReports'; callers are
-- responsible for keeping the two consistent (pass
-- @opts.sendClientReports@ here when constructing a transport for a
-- 'Sentry.Client.Options.ClientOptions' value).
--
-- If OpenTelemetry instrumentation is not needed, pass 'Nothing' for the
-- configuration and the default (no-op) instrumentation will be used.
--
-- Pass 'def' for 'Compression' to use the default ('Gzip').
new ::
  Compression ->
  Int ->
  Bool ->
  HttpClient.Manager ->
  Maybe HttpClientInstrumentationConfig ->
  Patrol.Dsn ->
  IO AsyncHttpTransport
new compression queueSize sendClientReports manager otelConfig dsn = do
  mReports <-
    if sendClientReports
      then do
        cr <- ClientReport.new
        let toEnvelope report =
              Patrol.Envelope.Envelope
                { Patrol.Envelope.headers =
                    Patrol.Headers.empty{Patrol.Headers.dsn = Just dsn},
                  Patrol.Envelope.items =
                    Patrol.Items.EnvelopeItems [Patrol.Item.ClientReport report]
                }
        pure $ Just ClientReportConfig{accumulator = cr, toEnvelope}
      else pure Nothing
  let template = Request.prepare compression dsn
      sendFn envelope rateLimiter = do
        now <- getCurrentTime
        result <- sendEnvelope manager (fold otelConfig) template envelope
        -- Record send/network failures as drops (a 429 is accounted for by the
        -- rate limiter via updateFromResponse, not as a drop).
        for_ (httpDiscardReason result) \reason ->
          for_ (fmap (.accumulator) mReports) \cr ->
            ClientReport.recordEnvelopeDrop cr reason envelope
        pure $ RateLimiter.updateFromResponse rateLimiter now result
  executor <- AsyncExecutor.new queueSize mReports sendFn
  pure AsyncHttpTransport{executor}

instance Transport AsyncHttpTransport where
  send t = AsyncExecutor.send t.executor
  flush t = AsyncExecutor.flush t.executor
  shutdown t = AsyncExecutor.shutdown t.executor
  recordLostEvent t = AsyncExecutor.recordLostEvent t.executor
