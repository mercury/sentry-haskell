module ClientReportTest where

import Control.Concurrent (forkOn, killThread, yield)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, finally, mask, throwIO, try)
import Control.Monad (replicateM_)
import Data.Foldable (traverse_)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Time.Clock (UTCTime (..), addUTCTime)
import Patrol.Type.ClientReport qualified as Patrol.ClientReport
import Patrol.Type.DataCategory (DataCategory (..))
import Patrol.Type.DiscardedEvent qualified as Patrol.DiscardedEvent
import Sentry.ClientReport (DiscardReason (..), piggybackInterval)
import Sentry.ClientReport qualified as ClientReport
import System.Timeout (timeout)
import Test.Concurrent (concurrentlyBounded)
import Test.Hspec

spec_clientReport :: Spec
spec_clientReport = describe "ClientReport" do
  describe "record / takePending" do
    it "returns Nothing when the accumulator is empty" do
      let now = UTCTime (toEnum 0) 0
      cr <- ClientReport.new now
      result <- ClientReport.takePending cr now True
      result `shouldBe` Nothing

    it "accumulates counts and returns them on force-flush" do
      let now = UTCTime (toEnum 0) 0
      cr <- ClientReport.new now
      ClientReport.record cr SampleRate Error 3
      ClientReport.record cr SampleRate Error 2
      result <- ClientReport.takePending cr now True
      case result of
        Nothing -> expectationFailure "expected a ClientReport"
        Just report -> do
          length report.discardedEvents `shouldBe` 1
          case report.discardedEvents of
            [] -> expectationFailure "expected at least one discarded event"
            (de : _) -> do
              Patrol.DiscardedEvent.reason de `shouldBe` "sample_rate"
              Patrol.DiscardedEvent.category de `shouldBe` Error
              Patrol.DiscardedEvent.quantity de `shouldBe` 5

    it "aggregates distinct (reason, category) pairs as separate entries" do
      let now = UTCTime (toEnum 0) 0
      cr <- ClientReport.new now
      ClientReport.record cr SampleRate Error 1
      ClientReport.record cr BeforeSend Error 2
      result <- ClientReport.takePending cr now True
      case result of
        Nothing -> expectationFailure "expected a ClientReport"
        Just report ->
          map (\item -> (item.reason, item.category, item.quantity)) report.discardedEvents
            `shouldBe` [("before_send", Error, 2), ("sample_rate", Error, 1)]

    it "resets the accumulator after takePending" do
      let now = UTCTime (toEnum 0) 0
      cr <- ClientReport.new now
      ClientReport.record cr SampleRate Error 1
      _ <- ClientReport.takePending cr now True
      result <- ClientReport.takePending cr now True
      result `shouldBe` Nothing

    it "ignores record calls with n <= 0" do
      let now = UTCTime (toEnum 0) 0
      cr <- ClientReport.new now
      ClientReport.record cr SampleRate Error 0
      ClientReport.record cr SampleRate Error (-1)
      result <- ClientReport.takePending cr now True
      result `shouldBe` Nothing

    it "round-trips every (reason, category) cell without collision" do
      let now = UTCTime (toEnum 0) 0
      cr <- ClientReport.new now
      -- Every category, in any order; reasons via the derived Bounded/Enum.
      let reasons = [minBound .. maxBound] :: [DiscardReason]
          categories =
            [ Default,
              Error,
              Transaction,
              Monitor,
              Span,
              LogItem,
              Security,
              Attachment,
              Session,
              Profile,
              ProfileChunk,
              Replay,
              Feedback,
              TraceMetric,
              Internal
            ]
          -- Assign a distinct, nonzero quantity to each of the 9×15 cells.
          cells = zip ([1 ..] :: [Int]) [(r, c) | r <- reasons, c <- categories]
      traverse_ (\(q, (r, c)) -> ClientReport.record cr r c q) cells
      result <- ClientReport.takePending cr now True
      case result of
        Nothing -> expectationFailure "expected a ClientReport"
        Just report -> do
          -- One entry per cell with its exact quantity: a collision in
          -- 'cellIndex' would merge two cells, dropping the count below the
          -- full grid size and doubling a quantity.
          let got =
                sort
                  [ (Patrol.DiscardedEvent.reason de, Patrol.DiscardedEvent.category de, Patrol.DiscardedEvent.quantity de)
                  | de <- report.discardedEvents
                  ]
              want =
                sort
                  [ (ClientReport.reasonText r, c, q)
                  | (q, (r, c)) <- cells
                  ]
          length report.discardedEvents `shouldBe` length cells
          got `shouldBe` want

  describe "concurrent recording and draining" do
    it "accounts for every increment exactly once across concurrent drains" do
      let now = UTCTime (toEnum 0) 0
          writes :: Int
          writes = 1_000
          cells :: [(DiscardReason, DataCategory, Int)]
          cells = [(SampleRate, Error, 1), (BeforeSend, Transaction, 3), (QueueOverflow, LogItem, 7)]
      cr <- ClientReport.new now
      totals <- newIORef Map.empty
      let collect = do
            result <- ClientReport.takePending cr now True
            traverse_ (\report -> traverse_ collectItem report.discardedEvents) result
          collectItem item = do
            item.quantity `shouldSatisfy` (> 0)
            atomicModifyIORef' totals \counts ->
              (Map.insertWith (+) (item.reason, item.category) item.quantity counts, ())
          writer = replicateM_ writes do
            traverse_ (\(reason, category, quantity) -> ClientReport.record cr reason category quantity) cells
            yield
          drainer = replicateM_ writes (collect >> yield)
      -- Pin workers so this also exercises simultaneous execution under +RTS -N.
      concurrentlyBounded (zipWith onCapability [0 ..] (replicate 4 writer <> replicate 4 drainer))
      -- Include increments which raced with the last drain and were left pending.
      collect
      readIORef totals
        `shouldReturn` Map.fromList
          [ ((ClientReport.reasonText reason, category), 4 * writes * quantity)
          | (reason, category, quantity) <- cells
          ]
      ClientReport.takePending cr now True `shouldReturn` Nothing

    it "observes a record published by another thread after an empty drain" do
      let now = UTCTime (toEnum 0) 0
      cr <- ClientReport.new now
      ClientReport.takePending cr now True `shouldReturn` Nothing
      within $ onCapability 1 $ ClientReport.record cr BeforeSend Error 3
      Just report <- within (ClientReport.takePending cr now True)
      map (.quantity) report.discardedEvents `shouldBe` [3]

    it "releases drain ownership when the interval check throws" do
      let now = UTCTime (toEnum 0) 0
      cr <- ClientReport.new now
      ClientReport.record cr BeforeSend Error 3
      ClientReport.takePending cr now (error "invalid force") `shouldThrow` anyErrorCall
      Just report <- within (ClientReport.takePending cr now True)
      map (.quantity) report.discardedEvents `shouldBe` [3]

  describe "interval" do
    it "returns Nothing when interval has not elapsed and force = False" do
      let now = UTCTime (toEnum 0) 0
      cr <- ClientReport.new now
      ClientReport.record cr SampleRate Error 1
      -- The initial reporting deadline is a full interval after creation.
      result <- ClientReport.takePending cr now False
      result `shouldBe` Nothing
      -- A skipped drain must not consume the indication that counts are pending.
      Just report <- ClientReport.takePending cr (addUTCTime piggybackInterval now) False
      map (.quantity) report.discardedEvents `shouldBe` [1]

    it "does not postpone a later report after repeated empty drains" do
      let now = UTCTime (toEnum 0) 0
          future = addUTCTime piggybackInterval now
      cr <- ClientReport.new now
      replicateM_ 3 do
        ClientReport.takePending cr future False `shouldReturn` Nothing
        ClientReport.takePending cr future True `shouldReturn` Nothing
      ClientReport.record cr BeforeSend Error 2
      Just report <- ClientReport.takePending cr future False
      map (.quantity) report.discardedEvents `shouldBe` [2]
      ClientReport.record cr BeforeSend Error 3
      ClientReport.takePending cr future False `shouldReturn` Nothing
      Just next <- ClientReport.takePending cr (addUTCTime piggybackInterval future) False
      map (.quantity) next.discardedEvents `shouldBe` [3]

    it "returns a report when force = True regardless of interval" do
      let now = UTCTime (toEnum 0) 0
      cr <- ClientReport.new now
      ClientReport.record cr SampleRate Error 1
      result <- ClientReport.takePending cr now True
      result `shouldSatisfy` \case
        Just _ -> True
        Nothing -> False

    it "does not move the last successful drain backwards" do
      let now = UTCTime (toEnum 0) 0
          future = addUTCTime piggybackInterval now
      cr <- ClientReport.new now
      ClientReport.record cr SampleRate Error 1
      _ <- ClientReport.takePending cr future True
      ClientReport.record cr SampleRate Error 2
      _ <- ClientReport.takePending cr now True
      ClientReport.record cr SampleRate Error 3
      ClientReport.takePending cr future False `shouldReturn` Nothing
      Just report <- ClientReport.takePending cr (addUTCTime piggybackInterval future) False
      map (.quantity) report.discardedEvents `shouldBe` [3]

    it "returns a report when piggybackInterval has elapsed" do
      let now = UTCTime (toEnum 0) 0
      cr <- ClientReport.new now
      ClientReport.record cr SampleRate Error 1
      -- Exactly the interval is sufficient.
      let future = addUTCTime piggybackInterval now
      result <- ClientReport.takePending cr future False
      result `shouldSatisfy` \case
        Just _ -> True
        Nothing -> False

    it "preserves the exact deadline across midnight after a successful drain" do
      let now = UTCTime (toEnum 0) 86_390
          firstDeadline = addUTCTime piggybackInterval now
          nextDeadline = addUTCTime piggybackInterval firstDeadline
          justBefore = addUTCTime (-0.000000000001)
      cr <- ClientReport.new now
      ClientReport.record cr SampleRate Error 1
      ClientReport.takePending cr (justBefore firstDeadline) False `shouldReturn` Nothing
      Just first <- ClientReport.takePending cr firstDeadline False
      map (.quantity) first.discardedEvents `shouldBe` [1]
      first.timestamp `shouldBe` Just firstDeadline
      ClientReport.record cr SampleRate Error 2
      ClientReport.takePending cr (justBefore nextDeadline) False `shouldReturn` Nothing
      Just next <- ClientReport.takePending cr nextDeadline False
      map (.quantity) next.discardedEvents `shouldBe` [2]
      next.timestamp `shouldBe` Just nextDeadline

-- | Run on a chosen capability, propagating failures and cleaning up on timeout.
onCapability :: Int -> IO () -> IO ()
onCapability capability action = mask \restore -> do
  done <- newEmptyMVar
  worker <- forkOn capability (try @SomeException (restore action) >>= putMVar done)
  restore (takeMVar done >>= either throwIO pure) `finally` killThread worker

-- | Bound synchronization waits so a leaked drain lock fails the test.
within :: IO a -> IO a
within action = timeout 5_000_000 action >>= maybe (throwIO $ userError "client-report test timed out") pure
