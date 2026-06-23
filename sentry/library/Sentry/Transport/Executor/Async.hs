{-# LANGUAGE NumericUnderscores #-}

-- | Asynchronous executor with dedicated dispatcher thread.
--
-- This implementation mirrors the Rust SDK's @tokio_thread@ executor:
-- <https://github.com/getsentry/sentry-rust/blob/master/sentry/src/transports/tokio_thread.rs>
--
-- Uses a bounded queue with non-blocking sends (drops events when full) and a
-- dedicated dispatcher thread for processing.
--
-- The executor is generic over the actual sending mechanism - you provide
-- a send function that handles envelope delivery:
--
-- > sendFn :: Patrol.Envelope -> IO Delivery.Outcome
-- > transport <- Sentry.Transport.Executor.Async.new defaultQueueSize 1 Nothing sendFn
--
-- The send function is responsible only for transmitting the envelope and
-- returning an 'Delivery.Outcome', and it is up to the executor to apply
-- 'RateLimiter.updateFromResponse' after each successful send.
--
-- == Concurrency
--
-- The dispatcher fans out sends concurrently up to a configurable cap. At cap 1
-- (the default) behaviour is byte-for-byte identical to a serial loop. At cap
-- @k@, up to @k@ sends may be in flight simultaneously; ordering across sends is
-- __not__ preserved (fine for independent Sentry events).
--
-- The in-flight count doubles as the flush\/shutdown drain barrier: 'flush' and
-- 'shutdown' wait for all in-flight sends to complete before proceeding.
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
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, retry, writeTVar)
import Control.Monad (void, when)
import Data.Atomics (atomicModifyIORefCAS_)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (NominalDiffTime, getCurrentTime)
import Patrol qualified
import Patrol.Type.ClientReport qualified as Patrol.ClientReport
import Patrol.Type.DataCategory (DataCategory)
import Sentry.ClientReport (ClientReports, DiscardReason)
import Sentry.ClientReport qualified as ClientReport
import Sentry.Transport (Transport (..))
import Sentry.Transport qualified as Sentry.Transport
import Sentry.Transport.Delivery qualified as Delivery
import Sentry.Transport.Executor.RateLimiter (RateLimiter)
import Sentry.Transport.Executor.RateLimiter qualified as RateLimiter
import UnliftIO.Exception (catchAny, finally, mask_)
import UnliftIO.Timeout qualified as UnliftIO (timeout)

-- | Tasks that can be sent to the dispatcher thread.
type Task :: Type
data Task
  = -- | Send an envelope to Sentry.
    SendEnvelope Patrol.Envelope
  | -- | Flush pending envelopes, synchronizing completion with the caller.
    Flush (MVar ())
  | -- | Signal the dispatcher thread to shut down.
    Shutdown

-- | Asynchronous executor that can be used to construct a
-- 'Sentry.Transport.Transport' which processes 'Patrol.Type.Envelope.Envelope's
-- on a dedicated dispatcher thread.
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
    clientReports :: Maybe ClientReports,
    rateLimiter :: IORef RateLimiter,
    inFlight :: TVar Int,
    concurrency :: Int
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
-- by the dispatcher thread for each envelope. It is responsible only for
-- transmitting the envelope and returning a 'Delivery.Outcome'; the
-- executor owns the rate-limiter state and applies
-- 'RateLimiter.updateFromResponse' after each successful send.
--
-- Pass @Just@ a 'ClientReportConfig' to enable client-report piggybacking, or
-- 'Nothing' to disable client reports entirely.
--
-- The concurrency cap controls how many sends may be in flight simultaneously.
-- Pass @1@ for serial behaviour (byte-for-byte identical to a single-threaded
-- loop), or a larger value to fan out sends concurrently (useful with HTTP\/2
-- multiplexing).
--
-- Example:
--
-- > sendFn :: Patrol.Envelope -> IO Delivery.Outcome
-- > sendFn envelope = do
-- >   -- Transmit the envelope via HTTP, return the outcome.
-- >   myFancySendEnvelopeFn envelope
-- >
-- > executor <- Sentry.Transport.Executor.Async.new defaultQueueSize 1 Nothing sendFn
new ::
  -- | Queue size (use 'defaultQueueSize' unless you have a specific reason to tune it).
  Int ->
  -- | Concurrency cap: max in-flight sends (@1@ = serial).
  Int ->
  Maybe ClientReportConfig ->
  (Patrol.Envelope -> IO Delivery.Outcome) ->
  IO AsyncExecutor
new queueSize cap reports sendFn = do
  (taskQueue, outChan) <- Unagi.newChan queueSize
  shutdownRef <- newTVarIO False
  rateLimiter <- newIORef RateLimiter.new
  inFlight <- newTVarIO 0
  let clientReports = fmap (.accumulator) reports
  handle <- async $ mkWorker outChan reports sendFn rateLimiter inFlight cap
  pure AsyncExecutor{taskQueue, shutdownRef, handle, clientReports, rateLimiter, inFlight, concurrency = cap}

-- | Dispatcher thread that reads tasks from the queue and fans out sends
-- behind a bounded in-flight count.
mkWorker ::
  Unagi.OutChan Task ->
  Maybe ClientReportConfig ->
  (Patrol.Envelope -> IO Delivery.Outcome) ->
  IORef RateLimiter ->
  TVar Int ->
  Int ->
  IO ()
mkWorker outChan reports sendFn rlRef inFlight cap = loop
  where
    loop :: IO ()
    loop =
      Unagi.readChan outChan >>= \case
        -- Drain any pending client report before exiting (best-effort).
        Shutdown -> do
          awaitDrained
          drainReportsInline
        -- Wait for all in-flight sends, drain any pending client report,
        -- then signal the caller and continue.
        Flush syncVar -> do
          awaitDrained
          drainReportsInline
          void $ tryPutMVar syncVar ()
          loop
        -- Acquire an in-flight slot under mask_ (so a cancellation between
        -- acquire and fork cannot leak a slot), filter, piggyback, then fork
        -- the actual send. The forked thread releases the slot via finally.
        SendEnvelope envelope -> do
          mask_ $ do
            acquireSlot
            now <- getCurrentTime
            rl <- readIORef rlRef
            let filtered = RateLimiter.filterEnvelope rl now envelope
            -- Account for every item dropped by rate limiting, whether the
            -- whole envelope was filtered out or only some of its items.
            for_ (fmap (.accumulator) reports) \cr ->
              ClientReport.recordItemDrops cr ClientReport.RatelimitBackoff filtered.dropped
            case filtered.kept of
              -- Nothing to send; hand the slot back and fall through to loop.
              Nothing -> releaseSlot
              Just filteredEnvelope -> do
                -- Piggyback any pending client report onto the outgoing
                -- envelope. Serial on the dispatcher (one report per send).
                piggybacked <- case reports of
                  Nothing -> pure filteredEnvelope
                  Just config -> do
                    mReport <- ClientReport.takePending config.accumulator now False
                    pure $ maybe filteredEnvelope (`ClientReport.attach` filteredEnvelope) mReport
                _ <- async (sendAndAccount piggybacked `finally` releaseSlot)
                pure ()
          loop

    -- | Send an envelope and fold its outcome into the shared rate limiter.
    -- Catches all exceptions so a misbehaving sendFn never kills the dispatcher.
    sendAndAccount :: Patrol.Envelope -> IO ()
    sendAndAccount e =
      ( do
          outcome <- sendFn e
          now <- getCurrentTime
          -- Account for send/network failures (a 429 is accounted for by the
          -- rate limiter, not as a drop) against the actually-sent envelope.
          for_ (Delivery.discardReason outcome) \reason ->
            for_ (fmap (.accumulator) reports) \cr ->
              ClientReport.recordEnvelopeDrop cr reason e
          atomicModifyIORefCAS_ rlRef \cur ->
            RateLimiter.updateFromResponse cur now outcome
      )
        `catchAny` \_ ->
          for_ (fmap (.accumulator) reports) \cr ->
            ClientReport.recordEnvelopeDrop cr ClientReport.InternalSdkError e

    -- | Force-drain any pending client report, sending it inline on the
    -- dispatcher thread (after all in-flight sends have completed). Folds any
    -- rate limit learned from the response into the shared rate limiter.
    -- Best-effort: a failed send is swallowed.
    drainReportsInline :: IO ()
    drainReportsInline = case reports of
      Nothing -> pure ()
      Just config -> do
        now <- getCurrentTime
        ClientReport.takePending config.accumulator now True >>= \case
          Nothing -> pure ()
          Just report -> do
            let reportEnvelope = config.toEnvelope report
            mOutcome <-
              fmap Just (sendFn reportEnvelope) `catchAny` \_ -> do
                ClientReport.recordEnvelopeDrop config.accumulator ClientReport.InternalSdkError reportEnvelope
                pure Nothing
            case mOutcome of
              Nothing -> pure ()
              Just outcome -> do
                now' <- getCurrentTime
                -- A failed report delivery is itself charged (under 'Internal')
                -- so it surfaces in the next report; 429s are left to the rate
                -- limiter, not counted as a drop.
                for_ (Delivery.discardReason outcome) \reason ->
                  ClientReport.recordEnvelopeDrop config.accumulator reason reportEnvelope
                atomicModifyIORefCAS_ rlRef \cur ->
                  RateLimiter.updateFromResponse cur now' outcome

    -- | Acquire an in-flight slot, blocking until one is available.
    -- Interruptible under mask_ (STM retry can receive async exceptions).
    acquireSlot :: IO ()
    acquireSlot =
      atomically $
        readTVar inFlight >>= \n ->
          if n >= cap then retry else writeTVar inFlight (n + 1)

    -- | Release an in-flight slot.
    releaseSlot :: IO ()
    releaseSlot = atomically $ modifyTVar' inFlight (subtract 1)

    -- | Block until all in-flight sends have completed.
    awaitDrained :: IO ()
    awaitDrained =
      atomically $
        readTVar inFlight >>= \n ->
          when (n > 0) retry

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
    -- Refuse new work, then signal the dispatcher to drain and exit.
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
