module RateLimiterTest where

import Control.Exception (SomeException (..))
import Data.ByteString (ByteString)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime, secondsToDiffTime)
import Data.Time.Clock.System (systemEpochDay)
import Patrol qualified
import Patrol.Type.ClientReport qualified as Patrol.ClientReport
import Patrol.Type.DataCategory qualified as DataCategory
import Patrol.Type.Envelope qualified as Patrol.Envelope
import Patrol.Type.Event qualified as Patrol.Event
import Patrol.Type.Headers qualified as Patrol.Headers
import Patrol.Type.Item qualified as Patrol.Item
import Patrol.Type.Items qualified as Patrol.Items
import Sentry.Transport.Executor.RateLimiter qualified as RateLimiter
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec

spec_updateFromRetryAfter :: Spec
spec_updateFromRetryAfter = describe "updateFromRetryAfter" do
  it "sets global rate limits" do
    let startTime = UTCTime systemEpochDay 0
        rl = RateLimiter.updateFromRetryAfter RateLimiter.new startTime "60"
    RateLimiter.isDisabledUntil rl Nothing
      `shouldBe` (Just $ 60 `addUTCTime` startTime)
    RateLimiter.isDisabledFor startTime Nothing rl
      `shouldBe` Just 60

  it "preserves fractional seconds" do
    let startTime = UTCTime systemEpochDay 0
        rl = RateLimiter.updateFromRetryAfter RateLimiter.new startTime "2.5"
    RateLimiter.isDisabledFor startTime Nothing rl
      `shouldBe` Just 2.5

  it "parses an IMF-fixdate as an absolute instant" do
    -- The result is independent of 'now'; 'startTime' here is only the
    -- reference point used to compute the remaining duration.
    let startTime = UTCTime systemEpochDay 0
        expiresAt = UTCTime (fromGregorian 2015 10 21) (secondsToDiffTime (7 * 3600 + 28 * 60))
        rl =
          RateLimiter.updateFromRetryAfter
            RateLimiter.new
            startTime
            "Wed, 21 Oct 2015 07:28:00 GMT"
    RateLimiter.isDisabledUntil rl Nothing
      `shouldBe` Just expiresAt

  it "falls back to 60 seconds for unparseable values" do
    let startTime = UTCTime systemEpochDay 0
        rl = RateLimiter.updateFromRetryAfter RateLimiter.new startTime "not-a-date"
    RateLimiter.isDisabledFor startTime Nothing rl
      `shouldBe` Just 60

spec_updateFromSentryHeader :: Spec
spec_updateFromSentryHeader = describe "updateFromSentryHeader" do
  it "updates per-category rate limits" do
    let startTime = UTCTime systemEpochDay 0
        header0 = "120:error:project:reason, 60:session:foo" :: ByteString
        rl0 = RateLimiter.updateFromSentryHeader RateLimiter.new startTime header0
    RateLimiter.isDisabledFor startTime (Just DataCategory.Error) rl0
      `shouldBe` Just 120
    RateLimiter.isDisabledFor startTime (Just DataCategory.Session) rl0
      `shouldBe` Just 60
    RateLimiter.isDisabledFor startTime (Just DataCategory.Transaction) rl0
      `shouldBe` Nothing
    RateLimiter.isDisabledFor startTime Nothing rl0
      `shouldBe` Nothing
    let header1 = "30::,\n120:invalid:invalid,\n4711:foo;bar;baz;security:project" :: ByteString
        rl1 = RateLimiter.updateFromSentryHeader rl0 startTime header1

    RateLimiter.isDisabledFor startTime Nothing rl1
      `shouldBe` Just 30
    RateLimiter.isDisabledFor startTime (Just DataCategory.Transaction) rl1
      `shouldBe` Just 30
    RateLimiter.isDisabledFor startTime (Just DataCategory.Error) rl1
      `shouldBe` Just 120
    RateLimiter.isDisabledFor startTime (Just DataCategory.Session) rl1
      `shouldBe` Just 60

  it "parses fractional durations" do
    let startTime = UTCTime systemEpochDay 0
        rl = RateLimiter.updateFromSentryHeader RateLimiter.new startTime "2.5:error:project"
    RateLimiter.isDisabledFor startTime (Just DataCategory.Error) rl
      `shouldBe` Just 2.5

  it "ignores trailing scope, reason, and namespace fields" do
    let startTime = UTCTime systemEpochDay 0
        header = "2700:error:organization:quota_exceeded:custom" :: ByteString
        rl = RateLimiter.updateFromSentryHeader RateLimiter.new startTime header
    RateLimiter.isDisabledFor startTime (Just DataCategory.Error) rl
      `shouldBe` Just 2700

  it "ignores whitespace after the comma separator" do
    let startTime = UTCTime systemEpochDay 0
        header = "60:error:project, 30:session:project" :: ByteString
        rl = RateLimiter.updateFromSentryHeader RateLimiter.new startTime header
    RateLimiter.isDisabledFor startTime (Just DataCategory.Error) rl
      `shouldBe` Just 60
    RateLimiter.isDisabledFor startTime (Just DataCategory.Session) rl
      `shouldBe` Just 30

  it "retains the maximum duration when a category repeats" do
    let startTime = UTCTime systemEpochDay 0
        header = "60:error:project,120:error:project" :: ByteString
        rl = RateLimiter.updateFromSentryHeader RateLimiter.new startTime header
    RateLimiter.isDisabledFor startTime (Just DataCategory.Error) rl
      `shouldBe` Just 120

  it "recognizes the log_item and trace_metric categories" do
    let startTime = UTCTime systemEpochDay 0
        header = "60:log_item:project,90:trace_metric:organization" :: ByteString
        rl = RateLimiter.updateFromSentryHeader RateLimiter.new startTime header
    RateLimiter.isDisabledFor startTime (Just DataCategory.LogItem) rl
      `shouldBe` Just 60
    RateLimiter.isDisabledFor startTime (Just DataCategory.TraceMetric) rl
      `shouldBe` Just 90

  it "skips a group with an unparseable duration without limiting it" do
    let startTime = UTCTime systemEpochDay 0
        header = "abc:error:project" :: ByteString
        rl = RateLimiter.updateFromSentryHeader RateLimiter.new startTime header
    RateLimiter.isDisabledFor startTime (Just DataCategory.Error) rl
      `shouldBe` Nothing
    RateLimiter.isDisabledFor startTime Nothing rl
      `shouldBe` Nothing

  it "skips a malformed group but keeps well-formed ones in the same header" do
    let startTime = UTCTime systemEpochDay 0
        header = "abc:error:project,60:session:project" :: ByteString
        rl = RateLimiter.updateFromSentryHeader RateLimiter.new startTime header
    RateLimiter.isDisabledFor startTime (Just DataCategory.Error) rl
      `shouldBe` Nothing
    RateLimiter.isDisabledFor startTime (Just DataCategory.Session) rl
      `shouldBe` Just 60

spec_updateFrom429 :: Spec
spec_updateFrom429 = describe "updateFrom429" do
  it "the HTTP 429 status code helper adds a 60 second delay" do
    let startTime = UTCTime systemEpochDay 0
        rl = RateLimiter.updateFrom429 RateLimiter.new startTime
    RateLimiter.isDisabledFor startTime Nothing rl
      `shouldBe` Just 60

spec_filterEnvelope :: Spec
spec_filterEnvelope = describe "filterEnvelope" do
  it "surfaces rate-limited items as dropped while keeping the rest" do
    let startTime = UTCTime systemEpochDay 0
        -- Limit the Error category only; the global limit stays clear, so the
        -- category-less client-report item survives while the event is dropped.
        rl = RateLimiter.updateFromSentryHeader RateLimiter.new startTime "60:error:project"
        envelope = mkEnvelope [eventItem, reportItem]
        filtered = RateLimiter.filterEnvelope rl startTime envelope
    filtered.dropped `shouldBe` [eventItem]
    fmap (.items) filtered.kept
      `shouldBe` Just (Patrol.Items.EnvelopeItems [reportItem])

  it "drops every item and keeps nothing when all are rate limited" do
    let startTime = UTCTime systemEpochDay 0
        rl = RateLimiter.updateFromSentryHeader RateLimiter.new startTime "60:error:project"
        filtered = RateLimiter.filterEnvelope rl startTime (mkEnvelope [eventItem])
    filtered.dropped `shouldBe` [eventItem]
    filtered.kept `shouldBe` Nothing

  it "keeps every item and drops nothing when no limit applies" do
    let startTime = UTCTime systemEpochDay 0
        filtered = RateLimiter.filterEnvelope RateLimiter.new startTime (mkEnvelope [eventItem])
    filtered.dropped `shouldBe` []
    fmap (.items) filtered.kept
      `shouldBe` Just (Patrol.Items.EnvelopeItems [eventItem])

-- | An envelope wrapping the given items with empty headers.
mkEnvelope :: [Patrol.Item] -> Patrol.Envelope
mkEnvelope items =
  Patrol.Envelope.Envelope
    { Patrol.Envelope.headers = Patrol.Headers.empty,
      Patrol.Envelope.items = Patrol.Items.EnvelopeItems items
    }

-- | An event item, which maps to the 'DataCategory.Error' rate-limit category.
eventItem :: Patrol.Item
eventItem = Patrol.Item.Event errorEvent

-- | A client-report item, which has no rate-limit category (global limit only).
reportItem :: Patrol.Item
reportItem =
  Patrol.Item.ClientReport
    Patrol.ClientReport.ClientReport
      { Patrol.ClientReport.timestamp = Nothing,
        Patrol.ClientReport.discardedEvents = []
      }

-- | A valid 'Patrol.Type.Event.Event' mock.
errorEvent :: Patrol.Event
errorEvent = unsafePerformIO $ Patrol.Event.fromSomeException $ SomeException $ userError "boom"
{-# NOINLINE errorEvent #-}
