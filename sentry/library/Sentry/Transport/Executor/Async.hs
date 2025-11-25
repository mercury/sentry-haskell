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
-- > transport <- Sentry.Transport.Async.new sendFn clientOptions
--
-- This allows the same executor to work with different HTTP libraries or
-- custom delivery mechanisms.
module Sentry.Transport.Executor.Async
  ( AsyncExecutor (..),
    RateLimiter,
    new,
    defaultQueueSize,
    send,
    flush,
    shutdown,
  )
where

import Control.Concurrent.Async (Async, async)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.Chan.Unagi.Bounded qualified as Unagi
import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, tryPutMVar)
import Control.Monad (void)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (NominalDiffTime, getCurrentTime)
import GHC.Conc.Sync (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Patrol qualified
import Sentry.Transport (Transport (..))
import Sentry.Transport qualified as Sentry.Transport
import Sentry.Transport.Executor.RateLimiter (RateLimiter)
import Sentry.Transport.Executor.RateLimiter qualified as RateLimiter
import UnliftIO.Timeout qualified as UnliftIO (timeout)

-- | Tasks that can be sent to the worker thread.
type Task :: Type
data Task
  = -- | Send an envelope to Sentry.
    SendEnvelope Patrol.Envelope
  | -- | Flush pending envelopes, synchronizing completion with the caller.
    Flush (MVar ())
  | -- | Signal the worker thread to shut down.
    Shutdown

-- | Asynchronous executor that can be used to construct a
-- 'Sentry.Transport.Transport' which processes 'Patrol.Type.Envelope.Envelope's
-- on a dedicated worker thread..
--
-- Uses a bounded queue with non-blocking sends. When the queue is full, events
-- are silently dropped to prevent blocking the application.
--
-- The send function is injected at construction time, making this transport
-- generic over the actual delivery mechanism.
type AsyncExecutor :: Type
data AsyncExecutor = AsyncExecutor
  { taskQueue :: Unagi.InChan Task,
    shutdownRef :: TVar Bool,
    handle :: Async ()
  }

-- | Default queue (matches Rust SDK's @tokio_thread@ executor).
defaultQueueSize :: Int
defaultQueueSize = 30

-- | Create a new 'AsyncExecutor'.
--
-- The send function handles the actual envelope delivery and is called
-- by the worker thread for each envelope. It receives the current rate
-- limiter state and returns an updated state (typically parsed from
-- response headers).
--
-- Example:
--
-- > sendFn :: Patrol.Envelope -> RateLimiter -> IO RateLimiter
-- > sendFn envelope rateLimiter = do
-- >   -- Send envelope via HTTP, parse rate limit headers, handler retries...
-- >   newRateLimiter <- myFancySendEnvelopeFn envelope
-- >   -- Return updated rate limiter
-- >   pure newRateLimiter
-- >
-- > executor <- Sentry.Transport.Executor.Async.new defaultQueueSize sendFn
new :: Int -> (Patrol.Envelope -> RateLimiter -> IO RateLimiter) -> IO AsyncExecutor
new queueSize sendFn = do
  (taskQueue, outChan) <- Unagi.newChan queueSize
  shutdownRef <- newTVarIO False
  handle <- async $ mkWorker outChan sendFn
  pure AsyncExecutor{taskQueue, shutdownRef, handle}

-- | Worker thread loop that processes tasks from the queue.
mkWorker ::
  Unagi.OutChan Task ->
  (Patrol.Envelope -> RateLimiter -> IO RateLimiter) ->
  IO ()
mkWorker outChan sendFn = loop RateLimiter.new
  where
    loop :: RateLimiter -> IO ()
    loop rateLimiter =
      Unagi.readChan outChan >>= \case
        Shutdown -> pure ()
        -- signal that all events enqueued behind the flush request have been sent
        Flush syncVar -> tryPutMVar syncVar () *> loop rateLimiter
        SendEnvelope envelope -> do
          now <- getCurrentTime
          case RateLimiter.isDisabledFor now RateLimiter.Any rateLimiter of
            -- we're globally rate-limited, drop the envelope & loop
            Just _ -> loop rateLimiter
            Nothing -> case RateLimiter.filterEnvelope rateLimiter now envelope of
              -- all item categories are rate limited, drop the envelope & loop
              Nothing -> loop rateLimiter
              Just filteredEnvelope -> do
                newRateLimiter <- sendFn filteredEnvelope rateLimiter
                loop newRateLimiter

instance Sentry.Transport.Transport AsyncExecutor where
  send :: AsyncExecutor -> Patrol.Envelope -> IO Sentry.Transport.SendResponse
  send executor envelope = unlessShutdown executor.shutdownRef Sentry.Transport.SendFailed_Shutdown do
    -- Non-blocking send: drop if queue is full
    Unagi.tryWriteChan executor.taskQueue (SendEnvelope envelope) <&> \case
      False -> Sentry.Transport.SendFailed_QueueFull
      True -> Sentry.Transport.SendProcessed

  flush :: AsyncExecutor -> NominalDiffTime -> IO Sentry.Transport.FlushResponse
  flush executor timeout = unlessShutdown executor.shutdownRef Sentry.Transport.FlushFailed_Shutdown do
    -- Create synchronization MVar
    syncVar <- newEmptyMVar
    success <- Unagi.tryWriteChan executor.taskQueue (Flush syncVar)
    if success
      then do
        -- Wait for the task to release the sync var, otherwise time out.
        result <- UnliftIO.timeout (toMicroseconds timeout) do
          Sentry.Transport.FlushSucceeded <$ readMVar syncVar
        pure $ (fromMaybe $ Sentry.Transport.FlushFailed_TimedOut timeout) result
      else pure Sentry.Transport.FlushFailed_QueueFull

  shutdown :: AsyncExecutor -> NominalDiffTime -> IO Sentry.Transport.ShutdownResponse
  shutdown executor timeout = unlessShutdown executor.shutdownRef Sentry.Transport.ShutdownFailed_AlreadyShutdown do
    -- Prevent new tasks from being enqueued.
    atomically $ writeTVar executor.shutdownRef True
    -- Send shutdown task (best effort)
    void $ Unagi.tryWriteChan executor.taskQueue Shutdown
    -- Wait for the worker or cancel if it hasn't cleaned itself up in time.
    result <- UnliftIO.timeout (toMicroseconds timeout) do
      Sentry.Transport.ShutdownSucceeded <$ Async.wait executor.handle
    case result of
      Just success -> pure success
      Nothing -> do
        Async.cancel executor.handle
        pure $ Sentry.Transport.ShutdownFailed_TimedOut timeout

-- | Helper to check whether the executor has been shut down and should refuse
-- to perform a given action.
unlessShutdown :: TVar Bool -> a -> IO a -> IO a
unlessShutdown ref a ma = readTVarIO ref >>= \case
  True -> pure a
  False -> ma
{-# INLINE unlessShutdown #-}

-- | Helper to convert 'Data.Time.Clock.NominalDiffTime' (whose integer form
-- represents whole seconds, but which has picosecond precision) to an integer
-- representing whole microseconds.
toMicroseconds :: NominalDiffTime -> Int
toMicroseconds dt = floor $ dt * 1_000_000
