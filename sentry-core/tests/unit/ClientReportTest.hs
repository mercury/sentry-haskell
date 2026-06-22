module ClientReportTest where

import Data.Foldable (traverse_)
import Data.List (sort)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Patrol.Type.ClientReport qualified as Patrol.ClientReport
import Patrol.Type.DataCategory (DataCategory (..))
import Patrol.Type.DiscardedEvent qualified as Patrol.DiscardedEvent
import Sentry.ClientReport (DiscardReason (..), piggybackInterval)
import Sentry.ClientReport qualified as ClientReport
import Test.Hspec

spec_clientReport :: Spec
spec_clientReport = describe "ClientReport" do
  describe "record / takePending" do
    it "returns Nothing when the accumulator is empty" do
      now <- getCurrentTime
      cr <- ClientReport.new
      result <- ClientReport.takePending cr now True
      result `shouldBe` Nothing

    it "accumulates counts and returns them on force-flush" do
      now <- getCurrentTime
      cr <- ClientReport.new
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
      now <- getCurrentTime
      cr <- ClientReport.new
      ClientReport.record cr SampleRate Error 1
      ClientReport.record cr BeforeSend Error 2
      result <- ClientReport.takePending cr now True
      case result of
        Nothing -> expectationFailure "expected a ClientReport"
        Just report -> length report.discardedEvents `shouldBe` 2

    it "resets the accumulator after takePending" do
      now <- getCurrentTime
      cr <- ClientReport.new
      ClientReport.record cr SampleRate Error 1
      _ <- ClientReport.takePending cr now True
      result <- ClientReport.takePending cr now True
      result `shouldBe` Nothing

    it "ignores record calls with n <= 0" do
      now <- getCurrentTime
      cr <- ClientReport.new
      ClientReport.record cr SampleRate Error 0
      ClientReport.record cr SampleRate Error (-1)
      result <- ClientReport.takePending cr now True
      result `shouldBe` Nothing

    it "round-trips every (reason, category) cell without collision" do
      now <- getCurrentTime
      cr <- ClientReport.new
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
          cells = zip [1 ..] [(r, c) | r <- reasons, c <- categories]
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

  describe "interval" do
    it "returns Nothing when interval has not elapsed and force = False" do
      now <- getCurrentTime
      cr <- ClientReport.new
      ClientReport.record cr SampleRate Error 1
      -- 'new' seeds lastSent to now, so 0s have elapsed; interval not met
      result <- ClientReport.takePending cr now False
      result `shouldBe` Nothing

    it "returns a report when force = True regardless of interval" do
      now <- getCurrentTime
      cr <- ClientReport.new
      ClientReport.record cr SampleRate Error 1
      result <- ClientReport.takePending cr now True
      result `shouldSatisfy` \case
        Just _ -> True
        Nothing -> False

    it "returns a report when piggybackInterval has elapsed" do
      now <- getCurrentTime
      cr <- ClientReport.new
      ClientReport.record cr SampleRate Error 1
      -- advance time past the interval
      let future = addUTCTime (piggybackInterval + 1) now
      result <- ClientReport.takePending cr future False
      result `shouldSatisfy` \case
        Just _ -> True
        Nothing -> False
