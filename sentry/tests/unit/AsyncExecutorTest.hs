module AsyncExecutorTest where

import Control.Concurrent (myThreadId, throwTo)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, tryPutMVar)
import Control.Concurrent.STM (check)
import Control.Concurrent.STM.TBMQueue qualified as TBM
import Control.Concurrent.STM.TQueue (TQueue, flushTQueue, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue)
import Control.Exception (MaskingState (Unmasked), bracket, finally, getMaskingState, mask_)
import Control.Exception qualified as Exception
import Control.Monad (replicateM_, void)
import Control.Monad.STM (atomically)
import Data.Foldable (for_)
import Data.Time.Clock (getCurrentTime)
import Patrol qualified
import Patrol.Type.DataCategory qualified as DataCategory
import Patrol.Type.Dsn qualified as Patrol.Dsn
import Patrol.Type.Envelope qualified as Patrol.Envelope
import Patrol.Type.Event qualified as Patrol.Event
import Patrol.Type.Headers qualified as Patrol.Headers
import Patrol.Type.Item qualified as Patrol.Item
import Patrol.Type.Items qualified as Patrol.Items
import Sentry.Client.Options qualified as Options
import Sentry.Client.Options.Dsn qualified as Dsn
import Sentry.ClientReport qualified as ClientReport
import Sentry.Init qualified as Init
import Sentry.Test qualified as Test
import Sentry.Transport qualified as Transport
import Sentry.Transport.Executor.Async qualified as AsyncExecutor
import Sentry.Transport.Executor.Async.Internal qualified as Internal
import Sentry.Transport.Executor.RateLimiter (RateLimiter)
import Sentry.Transport.Executor.RateLimiter qualified as RateLimiter
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import Test.Hspec
import UnliftIO.Exception (throwIO, toException)

spec_send :: Spec
spec_send = parallel $ describe "sending envelopes" do
  it "happens asynchronously" do
    q <- newTQueueIO
    -- construct a send function that blocks until `var` is filled
    var <- newEmptyMVar
    let sendFn env rl = readMVar var *> testSendFn q id env rl
    executor <- AsyncExecutor.new 1 Nothing sendFn
    -- verify that the send was processed
    res <- Transport.send executor testEnvelope
    res `shouldBe` Transport.SendProcessed
    -- unblock the send function
    putMVar var ()
    -- there is an envelope in the shared queue, the `sendFn` was called
    envelope <- atomically $ readTQueue q
    envelope `shouldBe` testEnvelope

  for_ [1, 3, AsyncExecutor.defaultQueueSize] \capacity ->
    it ("enforces exact queue capacity " <> show capacity) $ boundedLifecycle do
      entered <- newEmptyMVar
      release <- newEmptyMVar
      withExecutor capacity (\_ rl -> void (tryPutMVar entered ()) >> readMVar release >> pure rl) \executor -> do
        Transport.send executor testEnvelope `shouldReturn` Transport.SendProcessed
        readMVar entered
        replicateM_ capacity $ Transport.send executor testEnvelope `shouldReturn` Transport.SendProcessed
        Transport.send executor testEnvelope `shouldReturn` Transport.SendFailed_QueueFull
        Transport.flush executor 1 `shouldReturn` Transport.FlushFailed_QueueFull
        putMVar release ()
        Transport.shutdown executor 2 `shouldReturn` Transport.ShutdownSucceeded

  for_ [0, -1, minBound] \capacity ->
    it ("rejects invalid queue capacity " <> show capacity) do
      AsyncExecutor.new capacity Nothing (\_ -> pure) `shouldThrow` anyIOException

  it "is subject to filtering for valid envelope types" do
    q <- newTQueueIO
    let sendFn = testSendFn q id
    executor <- AsyncExecutor.new 2 Nothing sendFn
    -- verify that the send was processed
    sendRes <- Transport.send executor emptyEnvelope
    sendRes `shouldBe` Transport.SendProcessed
    -- flush the async executor's send queue
    void $ Transport.flush executor 1
    -- the test queue is empty, the envelope was dropped
    envelope <- atomically $ tryReadTQueue q
    envelope `shouldBe` Nothing

  it "respects rate limits" do
    q <- newTQueueIO
    -- construct a send function that applies a 1 minute rate limit after send
    now <- getCurrentTime
    let sendFn = testSendFn q (flip RateLimiter.updateFrom429 now)
    executor <- AsyncExecutor.new 1 Nothing sendFn
    -- verify that the send was processed
    res0 <- Transport.send executor testEnvelope
    res0 `shouldBe` Transport.SendProcessed
    -- there is an envelope in the shared queue, the `sendFn` was called
    envelope0 <- atomically $ readTQueue q
    envelope0 `shouldBe` testEnvelope
    -- verify that the send was processed
    res1 <- Transport.send executor testEnvelope
    res1 `shouldBe` Transport.SendProcessed
    -- flush the async executor's send queue
    void $ Transport.flush executor 1
    -- the queue is empty, the global rate limit was respected
    envelope1 <- atomically $ tryReadTQueue q
    envelope1 `shouldBe` Nothing

spec_flush :: Spec
spec_flush = parallel $ describe "flushing the executor" do
  it "times out when a send exceeds the given limit" do
    q <- newTQueueIO
    -- construct a send function that blocks until `var` is filled
    var <- newEmptyMVar
    let sendFn env rl = readMVar var *> testSendFn q id env rl
    executor <- AsyncExecutor.new 3 Nothing sendFn
    -- enqueues two messages to be sent despite the function blocking
    sendRes0 <- Transport.send executor testEnvelope
    sendRes0 `shouldBe` Transport.SendProcessed
    sendRes1 <- Transport.send executor testEnvelope
    sendRes1 `shouldBe` Transport.SendProcessed
    -- flush fails with a timeout error
    flushRes0 <- Transport.flush executor 0.001
    flushRes0 `shouldBe` (Transport.FlushFailed_TimedOut 0.001)
    -- unblock the send function
    putMVar var ()
    -- flush succeeds
    flushRes1 <- Transport.flush executor 1
    flushRes1 `shouldBe` Transport.FlushSucceeded
    -- both envelopes were sent succeessfully
    envelopes <- atomically $ flushTQueue q
    envelopes `shouldBe` [testEnvelope, testEnvelope]

spec_shutdown :: Spec
spec_shutdown = parallel $ describe "shutting the executor down" do
  it "rejects sends, flushes and repeat shutdowns once gracefully closed" do
    executor <- AsyncExecutor.new 1 Nothing (\_ rl -> pure rl)
    Transport.shutdown executor 1 `shouldReturn` Transport.ShutdownSucceeded
    Transport.send executor testEnvelope `shouldReturn` Transport.SendFailed_Shutdown
    Transport.flush executor 1 `shouldReturn` Transport.FlushFailed_Shutdown
    Transport.shutdown executor 1 `shouldReturn` Transport.ShutdownFailed_AlreadyShutdown

  it "immediately shuts the worker down when timeout is exceeded" $ boundedLifecycle do
    entered <- newEmptyMVar
    stopped <- newEmptyMVar
    never <- newEmptyMVar @()
    let sender _ rl = (putMVar entered () >> readMVar never >> pure rl) `finally` putMVar stopped ()
    withExecutor 1 sender \executor -> do
      Transport.send executor testEnvelope `shouldReturn` Transport.SendProcessed
      readMVar entered
      Transport.shutdown executor 0.001 `shouldReturn` Transport.ShutdownFailed_TimedOut 0.001
      readMVar stopped
      Transport.send executor testEnvelope `shouldReturn` Transport.SendFailed_Shutdown

  it "cancelling client close also terminates the executor worker" do
    completed <- timeout 5_000_000 do
      entered <- newEmptyMVar
      never <- newEmptyMVar @()
      stopped <- newEmptyMVar
      let sendFn _ rl = (putMVar entered () >> readMVar never >> pure rl) `finally` putMVar stopped ()
      executor <- AsyncExecutor.new 1 Nothing sendFn
      let opts =
            (Options.defaultClientOptions)
              { Options.dsn = Dsn.Explicit Test.TEST_DSN,
                Options.defaultIntegrations = False,
                Options.transport = Just (Options.PrebuiltTransport (Transport.SomeTransport executor)),
                Options.shutdownTimeout = 30
              }
      h <- Init.acquireClient opts
      Transport.send executor testEnvelope `shouldReturn` Transport.SendProcessed
      readMVar entered
      Async.withAsync (Init.close h) \closing -> do
        awaitClosed executor
        Async.cancel closing
        readMVar stopped
        Init.close h `shouldReturn` Transport.ShutdownFailed_Other "Client close interrupted"
    completed `shouldBe` Just ()

spec_resilience :: Spec
spec_resilience = parallel $ describe "worker resilience" do
  it "survives a sendFn that throws" do
    -- A throwing sendFn (e.g. an unhandled network exception) must not kill the
    -- worker thread and turn the executor into a black hole.
    let sendFn _ _ = throwIO (userError "boom") :: IO RateLimiter
    executor <- AsyncExecutor.new 3 Nothing sendFn
    -- The envelope is accepted; its send throws on the worker thread.
    res <- Transport.send executor testEnvelope
    res `shouldBe` Transport.SendProcessed
    -- The worker caught the exception, so flush still completes.
    flushRes <- Transport.flush executor 1
    flushRes `shouldBe` Transport.FlushSucceeded
    -- Subsequent sends are still accepted and shutdown remains graceful.
    res1 <- Transport.send executor testEnvelope
    res1 `shouldBe` Transport.SendProcessed
    shutdownRes <- Transport.shutdown executor 1
    shutdownRes `shouldBe` Transport.ShutdownSucceeded

spec_shutdownDrain :: Spec
spec_shutdownDrain = describe "shutting down with a saturated queue" do
  it "drains accepted envelopes after closing a full queue" $ boundedLifecycle do
    q <- newTQueueIO
    started <- newEmptyMVar
    block <- newEmptyMVar
    let sendFn env rl = do
          void $ tryPutMVar started ()
          readMVar block
          atomically $ writeTQueue q env
          pure rl
    withExecutor 1 sendFn \executor -> do
      Transport.send executor testEnvelope `shouldReturn` Transport.SendProcessed
      readMVar started
      Transport.send executor testEnvelope `shouldReturn` Transport.SendProcessed
      Async.withAsync (Transport.shutdown executor 5) \shutting -> do
        awaitClosed executor
        putMVar block ()
        Async.wait shutting `shouldReturn` Transport.ShutdownSucceeded
      atomically (flushTQueue q) `shouldReturn` [testEnvelope, testEnvelope]

spec_flushClientReports :: Spec
spec_flushClientReports = describe "flushing with client reports" do
  it "retains a rate limit learned while draining reports" do
    q <- newTQueueIO
    cr <- ClientReport.new
    -- A pending discard gives the forced drain something to send.
    ClientReport.record cr ClientReport.NetworkError DataCategory.Error 1
    let toEnvelope report =
          Patrol.Envelope.Envelope
            { Patrol.Envelope.headers = Patrol.Headers.empty,
              Patrol.Envelope.items = Patrol.Items.EnvelopeItems [Patrol.Item.ClientReport report]
            }
        reports = AsyncExecutor.ClientReportConfig{AsyncExecutor.accumulator = cr, AsyncExecutor.toEnvelope}
        -- Every send records the envelope and imposes a fresh global limit.
        sendFn env rl = do
          now <- getCurrentTime
          atomically $ writeTQueue q env
          pure $ RateLimiter.updateFrom429 rl now
    executor <- AsyncExecutor.new 3 (Just reports) sendFn
    -- The flush drains the report; sendFn returns a globally rate-limited state.
    Transport.flush executor 1 >>= (`shouldBe` Transport.FlushSucceeded)
    -- A subsequent send must be filtered out by the retained limit. If the
    -- drain's rate limiter were discarded, this envelope would be sent instead.
    Transport.send executor testEnvelope >>= (`shouldBe` Transport.SendProcessed)
    Transport.flush executor 1 >>= (`shouldBe` Transport.FlushSucceeded)
    envs <- atomically $ flushTQueue q
    envs `shouldSatisfy` notElem testEnvelope

-- | Stub function for sending an 'Patrol.Type.Envelope.Envelope', which adds
-- it to a queue that can be inspected from the test case.
testSendFn :: TQueue Patrol.Envelope -> (RateLimiter -> RateLimiter) -> Patrol.Envelope -> RateLimiter -> IO RateLimiter
testSendFn q fn env rl = do
  atomically $ writeTQueue q env
  pure $ fn rl

-- | An empty envelope; no headers means it should be filtered out.
emptyEnvelope :: Patrol.Envelope
emptyEnvelope =
  Patrol.Envelope.Envelope
    { Patrol.Envelope.items = Patrol.Items.EnvelopeItems [],
      Patrol.Envelope.headers = Patrol.Headers.empty
    }

-- | A valid 'Patrol.Type.Envelope.Envelope', derived from the 'testEvent' and
-- 'testDsn' mocks.
testEnvelope :: Patrol.Envelope
testEnvelope = Patrol.Envelope.fromEvent testDsn testEvent

-- | A valid 'Patrol.Type.Event.Event' mock.
testEvent :: Patrol.Event
testEvent = unsafePerformIO . Patrol.Event.fromSomeException . toException $ userError "boom"
{-# NOINLINE testEvent #-}

-- | A valid 'Patrol.Type.Dsn.Dsn' mock.
testDsn :: Patrol.Dsn
testDsn =
  Patrol.Dsn.Dsn
    { Patrol.Dsn.protocol = "a",
      Patrol.Dsn.publicKey = "b",
      Patrol.Dsn.secretKey = "",
      Patrol.Dsn.host = "c",
      Patrol.Dsn.port = Nothing,
      Patrol.Dsn.path = "/",
      Patrol.Dsn.projectId = "d"
    }

-- These tests own all worker resources and bound every scenario.
spec_admissionLifecycle :: Spec
spec_admissionLifecycle = describe "admission lifecycle" do
  it "acknowledges an admitted flush before graceful shutdown completes" $ boundedLifecycle do
    entered <- newEmptyMVar
    proceed <- newEmptyMVar
    withExecutor 4 (\_ rl -> putMVar entered () >> readMVar proceed >> pure rl) \executor -> do
      Transport.send executor testEnvelope `shouldReturn` Transport.SendProcessed
      readMVar entered
      Async.withAsync (Transport.flush executor 2) \flushing -> do
        awaitQueued executor
        Async.withAsync (Transport.shutdown executor 2) \closing -> do
          awaitClosed executor
          putMVar proceed ()
          Async.wait closing `shouldReturn` Transport.ShutdownSucceeded
          Async.wait flushing `shouldReturn` Transport.FlushSucceeded

  it "runs callbacks unmasked when constructed under masking" $ boundedLifecycle do
    observed <- newEmptyMVar
    executor <- mask_ $ AsyncExecutor.new 2 Nothing (\_ rl -> getMaskingState >>= putMVar observed >> pure rl)
    flip finally (void $ Transport.shutdown executor 0) do
      Transport.send executor testEnvelope `shouldReturn` Transport.SendProcessed
      readMVar observed `shouldReturn` Unmasked
      Transport.shutdown executor 1 `shouldReturn` Transport.ShutdownSucceeded

  it "reports asynchronous callback failure and rejects subsequent sends" $ boundedLifecycle do
    entered <- newEmptyMVar
    release <- newEmptyMVar
    withExecutor 4 (\_ _ -> putMVar entered () >> readMVar release >> Exception.throwIO Async.AsyncCancelled) \executor -> do
      Transport.send executor testEnvelope `shouldReturn` Transport.SendProcessed
      readMVar entered
      putMVar release ()
      Transport.shutdown executor 2 >>= (`shouldSatisfy` \case Transport.ShutdownFailed_Other _ -> True; _ -> False)
      Transport.send executor testEnvelope `shouldReturn` Transport.SendFailed_Shutdown

  it "wakes a concurrent flush when the worker is cancelled" $ boundedLifecycle do
    worker <- newEmptyMVar
    never <- newEmptyMVar @()
    withExecutor 4 (\_ rl -> myThreadId >>= putMVar worker >> readMVar never >> pure rl) \executor -> do
      Transport.send executor testEnvelope `shouldReturn` Transport.SendProcessed
      thread <- readMVar worker
      Async.withAsync (Transport.flush executor 2) \flushing -> do
        -- Waiting for admission (rather than racing a timeout) is what makes
        -- the outcome below deterministic: the flush is guaranteed to be
        -- queued, so cancelling the worker can only fail it with
        -- 'Transport.FlushFailed_Other', never 'Transport.FlushFailed_Shutdown'.
        awaitQueued executor
        throwTo thread Async.AsyncCancelled
        Async.wait flushing >>= (`shouldSatisfy` \case Transport.FlushFailed_Other _ -> True; _ -> False)
      Transport.send executor testEnvelope `shouldReturn` Transport.SendFailed_Shutdown

  it "reports worker failure while shutdown waits on a full queue" $ boundedLifecycle do
    worker <- newEmptyMVar
    never <- newEmptyMVar @()
    withExecutor 1 (\_ rl -> myThreadId >>= putMVar worker >> readMVar never >> pure rl) \executor -> do
      Transport.send executor testEnvelope `shouldReturn` Transport.SendProcessed
      thread <- readMVar worker
      Transport.send executor testEnvelope `shouldReturn` Transport.SendProcessed
      Async.withAsync (Transport.shutdown executor 2) \closing -> do
        awaitClosed executor
        throwTo thread Async.AsyncCancelled
        Async.wait closing >>= (`shouldSatisfy` \case Transport.ShutdownFailed_Other _ -> True; _ -> False)

  it "cancels a worker created by deferred client acquisition" $ boundedLifecycle do
    created <- newEmptyMVar
    observed <- newEmptyMVar
    stopped <- newEmptyMVar
    never <- newEmptyMVar @()
    let sendFn _ rl =
          (getMaskingState >>= putMVar observed >> readMVar never >> pure rl)
            `finally` putMVar stopped ()
        factory _ _ = do
          executor <- AsyncExecutor.new 2 Nothing sendFn
          putMVar created executor
          pure $ Transport.SomeTransport executor
        opts =
          (Options.defaultClientOptions)
            { Options.dsn = Dsn.Explicit Test.TEST_DSN,
              Options.defaultIntegrations = False,
              Options.transport = Just (Options.DeferredTransport factory),
              Options.shutdownTimeout = 0.001
            }
    bracket (Init.acquireClient opts) (void . Init.close) \clientHandle -> do
      executor <- readMVar created
      Transport.send executor testEnvelope `shouldReturn` Transport.SendProcessed
      readMVar observed `shouldReturn` Unmasked
      Init.close clientHandle `shouldReturn` Transport.ShutdownFailed_TimedOut 0.001
      -- The worker was cancelled out of its blocked send rather than left to
      -- run past the client that created it.
      readMVar stopped

  it "treats negative timeouts as immediate and saturates huge durations" $ boundedLifecycle do
    withExecutor 2 (\_ -> pure) \executor -> do
      Transport.flush executor (-1) `shouldReturn` Transport.FlushFailed_TimedOut (-1)
      Transport.flush executor (10 ^ (30 :: Int)) `shouldReturn` Transport.FlushSucceeded
      Transport.shutdown executor (-1) `shouldReturn` Transport.ShutdownFailed_TimedOut (-1)
      Transport.shutdown executor 1 `shouldReturn` Transport.ShutdownFailed_AlreadyShutdown

boundedLifecycle :: IO () -> IO ()
boundedLifecycle action = timeout 10_000_000 action `shouldReturn` Just ()

withExecutor :: Int -> (Patrol.Envelope -> RateLimiter -> IO RateLimiter) -> (AsyncExecutor.AsyncExecutor -> IO a) -> IO a
withExecutor size sendFn = bracket (AsyncExecutor.new size Nothing sendFn) (\executor -> void $ Transport.shutdown executor 0)

-- | Block until the executor's task queue closes.
--
-- The public 'Sentry.Transport.Executor.Async' module keeps the queue
-- private; this reaches into 'Sentry.Transport.Executor.Async.Internal' for
-- the passive STM wait, rather than polling the public boundary.
awaitClosed :: AsyncExecutor.AsyncExecutor -> IO ()
awaitClosed executor = atomically $ TBM.isClosedTBMQueue (Internal.taskQueue executor) >>= check

-- The worker is held inside its callback in callers of this helper, so the
-- queued marker cannot be consumed before we observe it.
awaitQueued :: AsyncExecutor.AsyncExecutor -> IO ()
awaitQueued executor = atomically $ TBM.isEmptyTBMQueue (Internal.taskQueue executor) >>= check . not
