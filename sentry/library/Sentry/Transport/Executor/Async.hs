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
-- > transport <- Sentry.Transport.Executor.Async.new def Nothing sendFn
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
-- 'shutdown' wait for all in-flight sends to complete before proceeding. A
-- timed-out 'shutdown' additionally aborts any sends still in flight so they do
-- not outlive the executor, and a 'flush' bounds its own drain by the caller's
-- timeout so a misbehaving send can never wedge the dispatcher permanently.
module Sentry.Transport.Executor.Async
  ( AsyncExecutor (..),
    ClientReportConfig (..),
    ExecutorOptions (..),
    RateLimiter,
    new,
    send,
    flush,
    shutdown,
    recordDiscards,
  )
where

import Control.Concurrent (ThreadId, killThread, myThreadId)
import Control.Concurrent.Async (Async, async, asyncWithUnmask)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.Chan.Unagi.Bounded qualified as Unagi
import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, tryPutMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, retry, writeTVar)
import Control.Monad (void, when)
import Data.Atomics (atomicModifyIORefCAS_)
import Data.Default (Default (def))
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
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
import UnliftIO.Exception (bracket_, catchAny, mask_, onException)
import UnliftIO.Timeout qualified as UnliftIO (timeout)

-- | Tasks that can be sent to the dispatcher thread.
type Task :: Type
data Task
  = -- | Send an envelope to Sentry.
    SendEnvelope Patrol.Envelope
  | -- | Flush pending envelopes, synchronizing completion with the caller. The
    -- caller's timeout rides along so the dispatcher can bound its own drain
    -- and never wedge permanently on a misbehaving send.
    Flush (MVar ()) NominalDiffTime
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
    -- | Count of occupied concurrency slots, as well as a semaphor that blocks
    -- until a slot is freed and a flush\/shutdown drain barrier.
    activeSlots :: TVar Int,
    -- | Thread ids of sends currently in flight, so a timed-out 'shutdown' can
    -- abort them.
    --
    -- Each forked send registers itself on entry and removes itself
    -- on completion.
    senders :: TVar (Set ThreadId)
  }

-- | Tuning options for an 'AsyncExecutor': how large the bounded task queue is
-- and how many envelopes may be sent concurrently.
--
-- Use 'def' for the defaults (queue size of 30, single-threaded concurrency)
-- and override with record syntax, e.g. @def{'concurrency' = 8}@ if desired.
type ExecutorOptions :: Type
data ExecutorOptions = ExecutorOptions
  { -- | Bounded task-queue capacity.
    queueSize :: Int,
    -- | Maximum number of in-flight sends.
    --
    -- @1@ = single-threaded, serial execution; @N@ = multi-threaded,
    -- concurrent execution (particularly useful with the HTTP\/2 backend).
    -- 
    -- __NOTE__: Values below @1@ are clamped to @1@.
    concurrency :: Int
  }

-- | Defaults: a 30-item queue (matching the Rust SDK's @tokio_thread@ executor)
-- and serialized dispatch with @'concurrency' = 1@.
instance Default ExecutorOptions where
  def = ExecutorOptions{queueSize = 30, concurrency = 1}

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
-- by the dispatcher thread for each envelope.
--
-- It is responsible only for transmitting the envelope, returning a
-- 'Delivery.Outcome', and applying 'RateLimiter.updateFromResponse' after
-- each successful send.
--
-- Example:
--
-- > sendFn :: Patrol.Envelope -> IO Delivery.Outcome
-- > sendFn envelope = do
-- >   -- Transmit the envelope via HTTP, return the outcome.
-- >   myFancySendEnvelopeFn envelope
-- >
-- > executor <- Sentry.Transport.Executor.Async.new def Nothing sendFn
new ::
  ExecutorOptions ->
  Maybe ClientReportConfig ->
  (Patrol.Envelope -> IO Delivery.Outcome) ->
  IO AsyncExecutor
new options reports sendFn = do
  -- A cap below 1 would make 'acquireSlot' retry forever on the first send,
  -- wedging the dispatcher; clamp it so the executor always makes progress.
  let concurrency = max 1 options.concurrency
  (taskQueue, outChan) <- Unagi.newChan options.queueSize
  shutdownRef <- newTVarIO False
  rateLimiter <- newIORef RateLimiter.new
  activeSlots <- newTVarIO 0
  senders <- newTVarIO Set.empty
  let clientReports = fmap (.accumulator) reports
  handle <- async $ mkWorker outChan reports sendFn rateLimiter activeSlots senders concurrency
  pure AsyncExecutor{taskQueue, shutdownRef, handle, clientReports, rateLimiter, activeSlots, senders}

-- | Dispatcher thread that reads tasks from the queue and fans out sends
-- behind a bounded in-flight count.
mkWorker ::
  Unagi.OutChan Task ->
  Maybe ClientReportConfig ->
  (Patrol.Envelope -> IO Delivery.Outcome) ->
  IORef RateLimiter ->
  TVar Int ->
  TVar (Set ThreadId) ->
  Int ->
  IO ()
mkWorker outChan reports sendFn rlRef activeSlots senders cap = loop
  where
    loop :: IO ()
    loop =
      Unagi.readChan outChan >>= \case
        -- Drain any pending client report before exiting (best-effort).
        Shutdown -> do
          awaitDrained
          drainReportsInline
        -- Wait for all in-flight sends, drain any pending client report, then
        -- signal the caller and continue.
        --
        -- The whole barrier+drain is bounded by the caller's timeout so a stuck
        -- in-flight send or a misbehaving drain send can never wedge the
        -- dispatcher permanently; on timeout we leave the syncVar unset so the
        -- caller observes 'FlushFailed_TimedOut'.
        Flush syncVar timeout -> do
          completed <- UnliftIO.timeout (toMicroseconds timeout) (awaitDrained *> drainReportsInline)
          for_ completed \() -> void (tryPutMVar syncVar ())
          loop
        -- Prepare the envelope (rate-limit filter + client-report piggyback)
        -- serially on the dispatcher, then acquire a slot and fork the actual
        -- send.
        --
        -- Preparation is guarded so a misbehaving filter/accumulator can
        -- never kill the dispatcher, and runs *before* the slot is taken so a
        -- failure cannot leak one.
        SendEnvelope envelope -> do
          mPrepared <-
            prepareEnvelope envelope `catchAny` \_ -> do
              for_ (fmap (.accumulator) reports) \cr ->
                ClientReport.recordEnvelopeDrop cr ClientReport.InternalSdkError envelope
              pure Nothing
          for_ mPrepared \piggybacked ->
            -- Mask the acquire->fork handoff so an async exception in that window
            -- can't strand a reserved slot.
            --
            -- The child runs unmasked and 'trackedSend' frees the slot when it
            -- finishes; if the fork itself fails the child never runs, so
            -- release the slot here instead.
            mask_ do
              acquireSlot
              void (asyncWithUnmask \unmask -> trackedSend (unmask (sendAndAccount piggybacked)))
                `onException` releaseSlot
          loop

    -- | Filter an envelope through the rate limiter and piggyback any pending
    -- client report. Runs serially on the dispatcher (one report per send).
    -- Returns 'Nothing' when rate limiting dropped the whole envelope.
    prepareEnvelope :: Patrol.Envelope -> IO (Maybe Patrol.Envelope)
    prepareEnvelope envelope = do
      now <- getCurrentTime
      rl <- readIORef rlRef
      let filtered = RateLimiter.filterEnvelope rl now envelope
      -- Account for every item dropped by rate limiting, whether the whole
      -- envelope was filtered out or only some of its items.
      for_ (fmap (.accumulator) reports) \cr ->
        ClientReport.recordItemDrops cr ClientReport.RatelimitBackoff filtered.dropped
      case filtered.kept of
        Nothing -> pure Nothing
        Just filteredEnvelope -> case reports of
          Nothing -> pure (Just filteredEnvelope)
          Just config -> do
            mReport <- ClientReport.takePending config.accumulator now False
            pure $ Just (maybe filteredEnvelope (`ClientReport.attach` filteredEnvelope) mReport)

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
        readTVar activeSlots >>= \n ->
          if n >= cap then retry else writeTVar activeSlots (n + 1)

    -- | Release an in-flight slot.
    releaseSlot :: IO ()
    releaseSlot = atomically $ modifyTVar' activeSlots (subtract 1)

    -- | Run a forked send as a tracked in-flight operation: register this
    -- thread in 'senders' (so a timed-out 'shutdown' can abort it) for the
    -- duration of @act@, then deregister and release its slot on completion,
    -- whether @act@ returns normally or throws.
    trackedSend :: IO () -> IO ()
    trackedSend act = do
      tid <- myThreadId
      bracket_
        (atomically (modifyTVar' senders (Set.insert tid)))
        (atomically (modifyTVar' senders (Set.delete tid)) *> releaseSlot)
        act

    -- | Block until all in-flight sends have completed.
    awaitDrained :: IO ()
    awaitDrained =
      atomically $
        readTVar activeSlots >>= \n ->
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
    success <- Unagi.tryWriteChan executor.taskQueue (Flush syncVar timeout)
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
        -- Best-effort: abort any sends still in flight so they do not outlive
        -- the executor after a timed-out shutdown. The dispatcher is already
        -- cancelled above, so no new sends can be forked past this point.
        senders <- readTVarIO executor.senders
        for_ senders killThread
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
