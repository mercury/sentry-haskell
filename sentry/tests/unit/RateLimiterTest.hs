module RateLimiterTest where

import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime(..), addUTCTime)
import Data.Time.Clock.System (systemEpochDay)
import Sentry.Transport.Executor.RateLimiter qualified as RateLimiter
import Test.Hspec

spec_updateFromRetryAfter :: Spec
spec_updateFromRetryAfter = describe "updateFromRetryAfter" do
  it "sets global rate limits" do
    let startTime = UTCTime systemEpochDay 0
        rl = RateLimiter.updateFromRetryAfter RateLimiter.new startTime "60"
    RateLimiter.isDisabledUntil rl RateLimiter.Any
      `shouldBe` (Just $ 60 `addUTCTime` startTime)
    RateLimiter.isDisabledFor startTime RateLimiter.Any rl
      `shouldBe` Just 60

spec_updateFromSentryHeader :: Spec
spec_updateFromSentryHeader = describe "updateFromSentryHeader" do
  it "updates per-category rate limits" do
    let startTime = UTCTime systemEpochDay 0
        header0 = "120:error:project:reason, 60:session:foo" :: ByteString
        rl0 = RateLimiter.updateFromSentryHeader RateLimiter.new startTime header0
    RateLimiter.isDisabledFor startTime RateLimiter.Error rl0
      `shouldBe` Just 120
    RateLimiter.isDisabledFor startTime RateLimiter.Session rl0
      `shouldBe` Just 60
    RateLimiter.isDisabledFor startTime RateLimiter.Transaction rl0
      `shouldBe` Nothing
    RateLimiter.isDisabledFor startTime RateLimiter.Any rl0
      `shouldBe` Nothing
    let header1 = "30::,\n120:invalid:invalid,\n4711:foo;bar;baz;security:project" :: ByteString
        rl1 = RateLimiter.updateFromSentryHeader rl0 startTime header1
    
    RateLimiter.isDisabledFor startTime RateLimiter.Any rl1
      `shouldBe` Just 30
    RateLimiter.isDisabledFor startTime RateLimiter.Transaction rl1
      `shouldBe` Just 30
    RateLimiter.isDisabledFor startTime RateLimiter.Error rl1
      `shouldBe` Just 120
    RateLimiter.isDisabledFor startTime RateLimiter.Session rl1
      `shouldBe` Just 60

spec_updateFrom429 :: Spec
spec_updateFrom429 =  describe "updateFrom429" do
  it "the HTTP 429 status code helper adds a 60 second delay" do
    let startTime = UTCTime systemEpochDay 0
        rl = RateLimiter.updateFrom429 RateLimiter.new startTime
    RateLimiter.isDisabledFor startTime RateLimiter.Any rl
      `shouldBe` Just 60
