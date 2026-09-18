-- | The 'AsyncExecutor' constructor and its task queue, exposed for tests
-- that need to synchronize on queue state directly.
module Sentry.Transport.Executor.Async.Internal
  ( AsyncExecutor (..),
    Task (..),
  )
where

import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM (atomically, orElse)
import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Control.Concurrent.STM.TBMQueue qualified as TBM
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, readTMVar)
import Control.Exception (SomeException, mask, onException)
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Time.Clock (NominalDiffTime)
import Patrol qualified
import Patrol.Type.DataCategory (DataCategory)
import Sentry.ClientReport (ClientReports, DiscardReason)
import Sentry.ClientReport qualified as ClientReport
import Sentry.Discard qualified as Discard
import Sentry.Transport qualified as Sentry.Transport
import UnliftIO.Exception (displayException)
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
    clientReports :: Maybe ClientReports,
    onDiscard :: Maybe Discard.Callback
  }

instance Sentry.Transport.Transport AsyncExecutor where
  send :: AsyncExecutor -> Patrol.Envelope -> IO Sentry.Transport.SendResponse
  send executor envelope = do
    result <- atomically $ TBM.tryWriteTBMQueue executor.taskQueue (SendEnvelope envelope)
    case result of
      Nothing -> pure Sentry.Transport.SendFailed_Shutdown
      Just True -> pure Sentry.Transport.SendProcessed
      Just False -> do
        Discard.recordEnvelope executor.clientReports executor.onDiscard ClientReport.QueueOverflow envelope
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
