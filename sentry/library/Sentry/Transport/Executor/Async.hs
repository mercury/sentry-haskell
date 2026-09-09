{-# LANGUAGE NumericUnderscores #-}

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
  ( AsyncExecutor (..),
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

import Control.Concurrent.Async (Async, asyncWithUnmask)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM (atomically, orElse)
import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Control.Concurrent.STM.TBMQueue qualified as TBM
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, readTMVar, tryPutTMVar)
import Control.Exception (SomeException, finally, mask, onException)
import Control.Monad (void, when)
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Time.Clock (NominalDiffTime, getCurrentTime)
import Patrol qualified
import Patrol.Type.ClientReport qualified as Patrol.ClientReport
import Patrol.Type.DataCategory (DataCategory)
import Sentry.ClientReport (ClientReports, DiscardReason)
import Sentry.ClientReport qualified as ClientReport
import Sentry.Transport (Transport (..))
import Sentry.Transport qualified as Sentry.Transport
import Sentry.Transport.Executor.RateLimiter (RateLimiter)
import Sentry.Transport.Executor.RateLimiter qualified as RateLimiter
import UnliftIO.Exception (catchAny, displayException)
import UnliftIO.Timeout qualified as UnliftIO (timeout)

-- | Tasks that can be sent to the worker thread.
type Task :: Type
data Task
  = -- | Send an envelope to Sentry.
    SendEnvelope Patrol.Envelope
  | -- | Flush pending envelopes, synchronizing completion with the caller.
    Flush (TMVar ())

-- | Asynchronous executor that can be used to construct a
-- 'Sentry.Transport.Transport' which processes 'Patrol.Type.Envelope.Envelope's
-- on a dedicated worker thread.
--
-- Uses a bounded queue with non-blocking sends. When the queue is full, events
-- are dropped and recorded as 'ClientReport.QueueOverflow' to prevent blocking
-- the application.
--
-- The send function is injected at construction time, making this transport
-- generic over the actual delivery mechanism.
type AsyncExecutor :: Type
data AsyncExecutor = AsyncExecutor
  { taskQueue :: TBMQueue Task,
    handle :: Async (),
    clientReports :: Maybe ClientReports
  }

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

instance Sentry.Transport.Transport AsyncExecutor where
  send :: AsyncExecutor -> Patrol.Envelope -> IO Sentry.Transport.SendResponse
  send executor envelope = do
    result <- atomically $ TBM.tryWriteTBMQueue executor.taskQueue (SendEnvelope envelope)
    case result of
      Nothing -> pure Sentry.Transport.SendFailed_Shutdown
      Just True -> pure Sentry.Transport.SendProcessed
      Just False -> do
        for_ executor.clientReports \cr ->
          ClientReport.recordEnvelopeDrop cr ClientReport.QueueOverflow envelope
        pure Sentry.Transport.SendFailed_QueueFull

  flush :: AsyncExecutor -> NominalDiffTime -> IO Sentry.Transport.FlushResponse
  flush executor duration = do
    result <- UnliftIO.timeout (toMicroseconds duration) do
      syncVar <- newEmptyTMVarIO
      admitted <- atomically $ TBM.tryWriteTBMQueue executor.taskQueue (Flush syncVar)
      case admitted of
        Nothing -> do
          Async.poll executor.handle >>= \case
            Just (Left err) -> pure $ Sentry.Transport.FlushFailed_Other (Text.pack $ displayException err)
            _ -> pure Sentry.Transport.FlushFailed_Shutdown
        Just False -> pure Sentry.Transport.FlushFailed_QueueFull
        Just True ->
          atomically $
            (Sentry.Transport.FlushSucceeded <$ readTMVar syncVar)
              `orElse` (workerFlushResult <$> Async.waitCatchSTM executor.handle)
    pure $ fromMaybe (Sentry.Transport.FlushFailed_TimedOut duration) result

  shutdown :: AsyncExecutor -> NominalDiffTime -> IO Sentry.Transport.ShutdownResponse
  shutdown executor duration = mask \restore -> do
    claimed <- atomically do
      closed <- TBM.isClosedTBMQueue executor.taskQueue
      TBM.closeTBMQueue executor.taskQueue
      pure (not closed)
    if not claimed
      then pure Sentry.Transport.ShutdownFailed_AlreadyShutdown
      else do
        result <-
          restore
            ( UnliftIO.timeout (toMicroseconds duration) do
                workerShutdownResult <$> Async.waitCatch executor.handle
            )
            `onException` Async.cancel executor.handle
        case result of
          Just success -> pure success
          Nothing -> do
            Async.cancel executor.handle
            pure $ Sentry.Transport.ShutdownFailed_TimedOut duration

  recordDiscards :: AsyncExecutor -> DiscardReason -> DataCategory -> Int -> IO ()
  recordDiscards executor reason category n =
    for_ executor.clientReports \reports ->
      ClientReport.record reports reason category n

workerFlushResult :: Either SomeException () -> Sentry.Transport.FlushResponse
workerFlushResult = \case
  Left err -> Sentry.Transport.FlushFailed_Other (Text.pack $ displayException err)
  Right () -> Sentry.Transport.FlushFailed_Other "Worker stopped before flush acknowledgement"

workerShutdownResult :: Either SomeException () -> Sentry.Transport.ShutdownResponse
workerShutdownResult = \case
  Left err -> Sentry.Transport.ShutdownFailed_Other (Text.pack $ displayException err)
  Right () -> Sentry.Transport.ShutdownSucceeded

-- | Nonpositive durations expire immediately; huge durations saturate.
toMicroseconds :: NominalDiffTime -> Int
toMicroseconds dt = fromInteger $ max 0 $ min (toInteger (maxBound :: Int)) (floor $ dt * 1_000_000)
