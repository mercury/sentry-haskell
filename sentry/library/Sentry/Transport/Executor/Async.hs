-- | Asynchronous executor with dedicated worker thread.
--
-- This implementation mirrors the Rust SDK's @tokio_thread@ executor:
-- <https://github.com/getsentry/sentry-rust/blob/master/sentry/src/transports/tokio_thread.rs>
--
-- Uses a bounded queue with non-blocking sends (drops events when full) and a
-- dedicated worker thread for processing.
--
-- The executor is generic over the actual sending mechanism - you provide
-- a send function that handles envelope delivery:
--
-- > sendFn :: Patrol.Envelope -> RateLimiter -> IO RateLimiter
-- > transport <- Sentry.Transport.Executor.Async.new defaultQueueSize Nothing sendFn
--
-- This allows the same executor to work with different HTTP libraries or
-- custom delivery mechanisms.
module Sentry.Transport.Executor.Async
  ( AsyncExecutor,
    ClientReportConfig (..),
    RateLimiter,
    new,
    defaultQueueSize,
    send,
    flush,
    shutdown,
    recordDiscards,
  )
where

import Control.Concurrent.Async (asyncWithUnmask)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Control.Concurrent.STM.TBMQueue qualified as TBM
import Control.Concurrent.STM.TMVar (tryPutTMVar)
import Control.Exception (finally, mask)
import Control.Monad (void, when)
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Time.Clock (getCurrentTime)
import Patrol qualified
import Patrol.Type.ClientReport qualified as Patrol.ClientReport
import Sentry.ClientReport (ClientReports)
import Sentry.ClientReport qualified as ClientReport
import Sentry.Transport (Transport (..))
import Sentry.Transport.Executor.Async.Internal (AsyncExecutor (..), Task (..))
import Sentry.Transport.Executor.RateLimiter (RateLimiter)
import Sentry.Transport.Executor.RateLimiter qualified as RateLimiter
import UnliftIO.Exception (catchAny)

-- | Default queue size (matches Rust SDK's @tokio_thread@ executor).
defaultQueueSize :: Int
defaultQueueSize = 30

-- | Client-report piggybacking configuration for an 'AsyncExecutor'.
type ClientReportConfig :: Type
data ClientReportConfig = ClientReportConfig
  { -- | The shared discard accumulator, written to by 'recordDiscards' and
    -- the worker and drained to piggyback reports onto outgoing envelopes.
    accumulator :: ClientReports,
    -- | Builds a standalone envelope from a drained client report, used for the
    -- forced-drain sends on flush and shutdown.
    toEnvelope :: Patrol.ClientReport.ClientReport -> Patrol.Envelope
  }

-- | Create a new 'AsyncExecutor' with an exact, positive queue capacity.
-- Nonpositive capacities raise an IO exception.
--
-- The send function handles the actual envelope delivery and is called
-- by the worker thread for each envelope. It receives the current rate
-- limiter state and returns an updated state (typically parsed from
-- response headers).
--
-- Pass @Just@ a 'ClientReportConfig' to enable client-report piggybacking, or
-- 'Nothing' to disable client reports entirely.
--
-- Example:
--
-- > sendFn :: Patrol.Envelope -> RateLimiter -> IO RateLimiter
-- > sendFn envelope rateLimiter = do
-- >   -- Send envelope via HTTP, parse rate limit headers, handle retries...
-- >   newRateLimiter <- myFancySendEnvelopeFn envelope
-- >   -- Return updated rate limiter
-- >   pure newRateLimiter
-- >
-- > executor <- Sentry.Transport.Executor.Async.new defaultQueueSize Nothing sendFn
new ::
  Int ->
  Maybe ClientReportConfig ->
  (Patrol.Envelope -> RateLimiter -> IO RateLimiter) ->
  IO AsyncExecutor
new queueSize reports sendFn = mask \_ -> do
  when (queueSize <= 0) $ fail "Executor queue size must be positive"
  taskQueue <- TBM.newTBMQueueIO queueSize
  let clientReports = fmap (.accumulator) reports
  handle <- asyncWithUnmask $ \unmask ->
    unmask (mkWorker taskQueue reports sendFn) `finally` atomically (TBM.closeTBMQueue taskQueue)
  pure AsyncExecutor{taskQueue, handle, clientReports}

-- | Worker thread loop that processes tasks from the queue.
mkWorker ::
  TBMQueue Task ->
  Maybe ClientReportConfig ->
  (Patrol.Envelope -> RateLimiter -> IO RateLimiter) ->
  IO ()
mkWorker queue reports sendFn = loop RateLimiter.new
  where
    loop :: RateLimiter -> IO ()
    loop rateLimiter =
      atomically (TBM.readTBMQueue queue) >>= \case
        -- Drain any pending client report before exiting (best-effort).
        Nothing -> void $ drainReports rateLimiter
        -- Drain, then signal completion, carrying any rate limit learned from
        -- the drain's response into the rest of the loop.
        Just (Flush syncVar) -> do
          rateLimiter' <- drainReports rateLimiter
          atomically (tryPutTMVar syncVar ()) *> loop rateLimiter'
        Just (SendEnvelope envelope) -> do
          now <- getCurrentTime
          let filtered = RateLimiter.filterEnvelope rateLimiter now envelope
          -- Account for every item dropped by rate limiting, whether the whole
          -- envelope was filtered out or only some of its items.
          for_ (fmap (.accumulator) reports) \cr ->
            ClientReport.recordItemDrops cr ClientReport.RatelimitBackoff filtered.dropped
          case filtered.kept of
            Nothing -> loop rateLimiter
            Just filteredEnvelope -> do
              -- Piggyback any pending client report onto the outgoing envelope.
              piggybacked <- case reports of
                Nothing -> pure filteredEnvelope
                Just config -> do
                  mReport <- ClientReport.takePending config.accumulator now False
                  pure $ maybe filteredEnvelope (`ClientReport.attach` filteredEnvelope) mReport
              newRateLimiter <-
                -- a misbehaving sendFn must never kill the worker thread.
                --
                -- the built-in HTTP sendFn already converts transport failures
                -- into values, so this only catches genuinely unexpected
                -- errors.
                sendFn piggybacked rateLimiter `catchAny` \_ -> do
                  for_ (fmap (.accumulator) reports) \cr ->
                    ClientReport.recordEnvelopeDrop cr ClientReport.InternalSdkError filteredEnvelope
                  pure rateLimiter
              loop newRateLimiter

    -- Force-drain any pending client report, sending it immediately. Returns
    -- the (possibly updated) rate limiter so a limit learned from the drain's
    -- response is not lost. Best-effort: a failed send is swallowed and the
    -- prior rate limiter retained.
    drainReports :: RateLimiter -> IO RateLimiter
    drainReports rateLimiter = case reports of
      Nothing -> pure rateLimiter
      Just config -> do
        now <- getCurrentTime
        ClientReport.takePending config.accumulator now True >>= \case
          Nothing -> pure rateLimiter
          Just report ->
            sendFn (config.toEnvelope report) rateLimiter `catchAny` \_ -> pure rateLimiter
