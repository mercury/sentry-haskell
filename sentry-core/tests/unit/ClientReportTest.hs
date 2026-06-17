module ClientReportTest where

import Data.Text qualified as Text
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Patrol.Type.ClientReport qualified as Patrol.ClientReport
import Patrol.Type.DataCategory (DataCategory (..))
import Patrol.Type.DiscardedEvent qualified as Patrol.DiscardedEvent
import Sentry.ClientReport (DiscardReason (..), maxDistinctKeys, piggybackInterval)
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

  describe "keyspace cap" do
    it "folds overflow keys into (Custom \"other\", category) sentinel" do
      now <- getCurrentTime
      cr <- ClientReport.new
      -- fill up to the cap with distinct Custom reasons
      mapM_
        (\i -> ClientReport.record cr (Custom $ "reason-" <> Text.pack (show i)) Error 1)
        [1 .. maxDistinctKeys]
      -- this one should overflow into the sentinel
      ClientReport.record cr (Custom "overflow") Error 10
      result <- ClientReport.takePending cr now True
      case result of
        Nothing -> expectationFailure "expected a ClientReport"
        Just report -> do
          -- total quantity preserved: maxDistinctKeys + 10
          let total = sum $ map Patrol.DiscardedEvent.quantity report.discardedEvents
          total `shouldBe` maxDistinctKeys + 10
          -- number of distinct entries bounded at maxDistinctKeys
          length report.discardedEvents `shouldSatisfy` (<= maxDistinctKeys)

    it "always increments an existing key even when the map is full" do
      now <- getCurrentTime
      cr <- ClientReport.new
      -- fill the map to capacity
      mapM_
        (\i -> ClientReport.record cr (Custom $ "reason-" <> Text.pack (show i)) Error 1)
        [1 .. maxDistinctKeys]
      -- re-record an existing key; should increment, not overflow
      ClientReport.record cr (Custom "reason-1") Error 99
      result <- ClientReport.takePending cr now True
      case result of
        Nothing -> expectationFailure "expected a ClientReport"
        Just report -> do
          let total = sum $ map Patrol.DiscardedEvent.quantity report.discardedEvents
          total `shouldBe` maxDistinctKeys + 99
          length report.discardedEvents `shouldBe` maxDistinctKeys
