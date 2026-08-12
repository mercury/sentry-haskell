module AsyncExecutorTest where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, tryPutMVar)
import Control.Concurrent.STM.TQueue (TQueue, flushTQueue, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue)
import Control.Monad (replicateM, void)
import Control.Monad.STM (atomically)
import Data.Time.Clock (getCurrentTime)
import Patrol qualified
import Patrol.Type.DataCategory qualified as DataCategory
import Patrol.Type.Dsn qualified as Patrol.Dsn
import Patrol.Type.Envelope qualified as Patrol.Envelope
import Patrol.Type.Event qualified as Patrol.Event
import Patrol.Type.Headers qualified as Patrol.Headers
import Patrol.Type.Item qualified as Patrol.Item
import Patrol.Type.Items qualified as Patrol.Items
import Sentry.ClientReport qualified as ClientReport
import Sentry.Transport qualified as Transport
import Sentry.Transport.Executor.Async qualified as AsyncExecutor
import Sentry.Transport.Executor.RateLimiter (RateLimiter)
import Sentry.Transport.Executor.RateLimiter qualified as RateLimiter
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import UnliftIO.Exception (displayException, fromException, throwIO, toException)

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

  it "drops envelopes when pending writes exceed capacity + 1" do
    q <- newTQueueIO
    -- construct a send function that blocks until `var` is filled
    var <- newEmptyMVar
    let sendFn env rl = readMVar var *> testSendFn q id env rl
        capacity = 1 :: Int
        attempts = capacity + 2
    executor <- AsyncExecutor.new capacity Nothing sendFn
    -- Saturate the executor faster than the worker can drain:
    --
    -- \* the worker is blocked in sendFn, so it dequeues at most one envelope
    -- \* at most capacity+1 envelopes can ever be pending
    -- \* out of capacity+2 rapid attempts, at least one is therefore
    --   guaranteed to hit a full queue and return SendFailed_QueueFull
    results <- replicateM attempts (Transport.send executor testEnvelope)
    -- The first write always succeeds (queue is empty when it runs).
    case results of
      [] -> expectationFailure "expected at least one send result"
      (r : _) -> r `shouldBe` Transport.SendProcessed
    -- At least one attempt was rejected with QueueFull.
    results `shouldSatisfy` elem Transport.SendFailed_QueueFull
    -- Unblock the worker so the executor can be torn down cleanly.
    putMVar var ()

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
  it "gracefully shuts the worker down" do
    let sendFn _ = pure
    executor <- AsyncExecutor.new 1 Nothing sendFn
    -- shutdown call succeeds
    res <- Transport.shutdown executor 1
    res `shouldBe` Transport.ShutdownSucceeded
    -- worker was shut down gracefully
    Async.poll executor.handle >>= \case
      Nothing -> expectationFailure "async worker should have completed"
      Just (Left exc) -> expectationFailure $ "async worker failed to shut down: " <> displayException exc
      Just (Right ()) -> pure ()

  it "immediately shuts the worker down when timeout is exceeded" do
    q <- newTQueueIO
    -- construct a send function that blocks until `var` is filled
    var <- newEmptyMVar
    let sendFn env rl = readMVar var *> testSendFn q id env rl
    executor <- AsyncExecutor.new 1 Nothing sendFn
    -- envelope is enqueued even though the send function is blocking
    sendRes <- Transport.send executor testEnvelope
    sendRes `shouldBe` Transport.SendProcessed
    -- shutdown call times out because of the empty mvar
    shutdownRes <- Transport.shutdown executor 0.001
    shutdownRes `shouldBe` (Transport.ShutdownFailed_TimedOut 0.001)
    -- the worker was terminated after the graceful shutdown failed
    Async.poll executor.handle >>= \case
      Nothing -> do
        Async.cancel executor.handle
        expectationFailure "async worker should have completed"
      Just (Left exc) -> fromException exc `shouldBe` Just Async.AsyncCancelled
      Just (Right ()) -> expectationFailure $ "async worker should have been canceled"
    -- the worker was killed before the blocking call was lifted, so the
    -- envelope should never have sent
    envelope <- atomically $ tryReadTQueue q
    envelope `shouldBe` Nothing

  it "stops processing envelopes" do
    let sendFn _ = pure
    executor <- AsyncExecutor.new 1 Nothing sendFn
    void $ Transport.shutdown executor 1
    res <- Transport.send executor testEnvelope
    res `shouldBe` Transport.SendFailed_Shutdown

  it "stops processing flush requests" do
    let sendFn _ = pure
    executor <- AsyncExecutor.new 1 Nothing sendFn
    void $ Transport.shutdown executor 1
    res <- Transport.flush executor 1
    res `shouldBe` Transport.FlushFailed_Shutdown

  it "does not shutdown more than once" do
    let sendFn _ = pure
    executor <- AsyncExecutor.new 1 Nothing sendFn
    void $ Transport.shutdown executor 1
    res <- Transport.shutdown executor 1
    res `shouldBe` Transport.ShutdownFailed_AlreadyShutdown

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
    -- The worker is still alive: the throw did not terminate it.
    Async.poll executor.handle >>= \case
      Nothing -> pure ()
      Just (Left exc) -> expectationFailure $ "worker died: " <> displayException exc
      Just (Right ()) -> expectationFailure "worker exited unexpectedly"
    -- Subsequent sends are still accepted and shutdown remains graceful.
    res1 <- Transport.send executor testEnvelope
    res1 `shouldBe` Transport.SendProcessed
    shutdownRes <- Transport.shutdown executor 1
    shutdownRes `shouldBe` Transport.ShutdownSucceeded

spec_shutdownDrain :: Spec
spec_shutdownDrain = describe "shutting down with a saturated queue" do
  it "delivers the shutdown signal even when the queue is full" do
    q <- newTQueueIO
    -- `started` signals the worker has dequeued the first envelope; `block`
    -- holds it inside sendFn so the queue can be saturated behind it.
    started <- newEmptyMVar
    block <- newEmptyMVar
    let sendFn env rl = do
          void $ tryPutMVar started ()
          readMVar block
          atomically $ writeTQueue q env
          pure rl
    executor <- AsyncExecutor.new 1 Nothing sendFn
    -- The worker dequeues env0 and blocks in sendFn.
    Transport.send executor testEnvelope >>= (`shouldBe` Transport.SendProcessed)
    readMVar started
    -- env1 now fills the size-1 queue while the worker is busy.
    Transport.send executor testEnvelope >>= (`shouldBe` Transport.SendProcessed)
    -- shutdown cannot enqueue Shutdown (queue full) until the worker drains,
    -- which we allow only after the call is in flight.
    shutting <- Async.async $ Transport.shutdown executor 5
    putMVar block ()
    res <- Async.wait shutting
    res `shouldBe` Transport.ShutdownSucceeded
    -- Both queued envelopes were delivered before the worker exited; a dropped
    -- Shutdown task would instead force-cancel the worker and time out.
    envs <- atomically $ flushTQueue q
    envs `shouldBe` [testEnvelope, testEnvelope]

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
