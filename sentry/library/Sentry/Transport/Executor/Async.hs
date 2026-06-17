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

import Control.Concurrent.Async (Async, async)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.Chan.Unagi.Bounded qualified as Unagi
import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, tryPutMVar)
import Control.Monad (void)
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (NominalDiffTime, getCurrentTime)
import GHC.Conc.Sync (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Patrol qualified
import Patrol.Type.ClientReport qualified as Patrol.ClientReport
import Patrol.Type.DataCategory (DataCategory)
import Sentry.ClientReport (ClientReports, DiscardReason)
import Sentry.ClientReport qualified as ClientReport
import Sentry.Transport (Transport (..))
import Sentry.Transport qualified as Sentry.Transport
import Sentry.Transport.Executor.RateLimiter (RateLimiter)
import Sentry.Transport.Executor.RateLimiter qualified as RateLimiter
import UnliftIO.Exception (catchAny)
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
  { taskQueue :: Unagi.InChan Task,
    shutdownRef :: TVar Bool,
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

-- | Create a new 'AsyncExecutor'.
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
new queueSize reports sendFn = do
  (taskQueue, outChan) <- Unagi.newChan queueSize
  shutdownRef <- newTVarIO False
  let clientReports = fmap (.accumulator) reports
  handle <- async $ mkWorker outChan reports sendFn
  pure AsyncExecutor{taskQueue, shutdownRef, handle, clientReports}

-- | Worker thread loop that processes tasks from the queue.
mkWorker ::
  Unagi.OutChan Task ->
  Maybe ClientReportConfig ->
  (Patrol.Envelope -> RateLimiter -> IO RateLimiter) ->
  IO ()
mkWorker outChan reports sendFn = loop RateLimiter.new
  where
    loop :: RateLimiter -> IO ()
    loop rateLimiter =
      Unagi.readChan outChan >>= \case
        -- Drain any pending client report before exiting (best-effort).
        Shutdown -> void $ drainReports rateLimiter
        -- Drain, then signal completion, carrying any rate limit learned from
        -- the drain's response into the rest of the loop.
        Flush syncVar -> do
          rateLimiter' <- drainReports rateLimiter
          tryPutMVar syncVar () *> loop rateLimiter'
        SendEnvelope envelope -> do
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
  send executor envelope = unlessShutdown executor.shutdownRef Sentry.Transport.SendFailed_Shutdown do
    -- Non-blocking send: drop if queue is full.
    Unagi.tryWriteChan executor.taskQueue (SendEnvelope envelope) >>= \case
      True -> pure Sentry.Transport.SendProcessed
      False -> do
        for_ executor.clientReports \cr ->
          ClientReport.recordEnvelopeDrop cr ClientReport.QueueOverflow envelope
        pure Sentry.Transport.SendFailed_QueueFull

  flush :: AsyncExecutor -> NominalDiffTime -> IO Sentry.Transport.FlushResponse
  flush executor timeout = unlessShutdown executor.shutdownRef Sentry.Transport.FlushFailed_Shutdown do
    syncVar <- newEmptyMVar
    success <- Unagi.tryWriteChan executor.taskQueue (Flush syncVar)
    if success
      then do
        result <- UnliftIO.timeout (toMicroseconds timeout) do
          Sentry.Transport.FlushSucceeded <$ readMVar syncVar
        pure $ (fromMaybe $ Sentry.Transport.FlushFailed_TimedOut timeout) result
      else pure Sentry.Transport.FlushFailed_QueueFull

  shutdown :: AsyncExecutor -> NominalDiffTime -> IO Sentry.Transport.ShutdownResponse
  shutdown executor timeout = unlessShutdown executor.shutdownRef Sentry.Transport.ShutdownFailed_AlreadyShutdown do
    -- Refuse new work, then signal the worker to drain and exit.
    atomically $ writeTVar executor.shutdownRef True
    result <- UnliftIO.timeout (toMicroseconds timeout) do
      Unagi.writeChan executor.taskQueue Shutdown
      Sentry.Transport.ShutdownSucceeded <$ Async.wait executor.handle
    case result of
      Just success -> pure success
      Nothing -> do
        Async.cancel executor.handle
        pure $ Sentry.Transport.ShutdownFailed_TimedOut timeout

  recordDiscards :: AsyncExecutor -> DiscardReason -> DataCategory -> Int -> IO ()
  recordDiscards executor reason category n =
    for_ executor.clientReports \reports ->
      ClientReport.record reports reason category n

-- | Helper to check whether the executor has been shut down and should refuse
-- to perform a given action.
unlessShutdown :: TVar Bool -> a -> IO a -> IO a
unlessShutdown ref a ma =
  readTVarIO ref >>= \case
    True -> pure a
    False -> ma
{-# INLINE unlessShutdown #-}

-- | Helper to convert 'Data.Time.Clock.NominalDiffTime' (whose integer form
-- represents whole seconds, but which has picosecond precision) to an integer
-- representing whole microseconds.
toMicroseconds :: NominalDiffTime -> Int
toMicroseconds dt = floor $ dt * 1_000_000
