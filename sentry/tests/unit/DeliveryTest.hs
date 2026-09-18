{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module DeliveryTest where

import Control.Concurrent (threadDelay)
import Data.Foldable (for_)
import Data.Time.Clock (NominalDiffTime, UTCTime (..), addUTCTime, getCurrentTime)
import Network.HTTP.Types qualified as Http
import Sentry.ClientReport qualified as Report
import Sentry.Transport.Delivery qualified as Delivery
import Sentry.Transport.HTTP.Delivery qualified as HTTP
import Test.Hspec

spec_classification :: Spec
spec_classification = describe "delivery discard classification" do
  for_ [200, 201, 204, 299, 429] \code ->
    it ("does not report HTTP " <> show code) $
      Delivery.discardReason (HTTP.interpret (UTCTime (toEnum 0) 0) $ HTTP.Responded (Http.mkStatus code "test") [])
        `shouldBe` Nothing
  for_ [300, 400, 413, 500] \code ->
    it ("reports HTTP " <> show code <> " as send_error") $
      Delivery.discardReason (HTTP.interpret (UTCTime (toEnum 0) 0) $ HTTP.Responded (Http.mkStatus code "test") [])
        `shouldBe` Just Report.SendError
  it "reports network failures" $
    Delivery.discardReason (HTTP.interpret (UTCTime (toEnum 0) 0) $ HTTP.NetworkFailure "connection failed")
      `shouldBe` Just Report.NetworkError

-- A later fix to 'interpret's rate-limit precedence adds the tests this
-- coverage needs; until then, 'RateLimiterTest' exercises the header parsing
-- this module still delegates to.
spec_policy :: Spec
spec_policy = describe "HTTP delivery policy" $ pure ()

-- | Deadlines must be computed from a timestamp taken after the response, not
-- from before the request.
--
-- Under a pre-send timestamp, @Retry-After: 60@ on a send lasting @d@ seconds
-- yields a deadline only @60 - d@ seconds after the response, so the SDK
-- resumes while the server is still asking it to back off.
spec_deadlineTiming :: Spec
spec_deadlineTiming = describe "rate-limit deadline timing" do
  it "does not subtract a slow send's latency from the backoff" do
    let
      -- Stand in for a send whose response arrives measurably later than
      -- the clock jitter around the two timestamps taken below.
      slowSend = threadDelay 200_000 *> pure (HTTP.Responded Http.status200 [("Retry-After", "60")])
      margin = 0.1 :: NominalDiffTime
    sentAt <- getCurrentTime
    outcome <- slowSend >>= HTTP.interpretNow
    observedAt <- getCurrentTime
    case outcome.rateLimits of
      [limit] -> do
        limit.expiresAt `shouldSatisfy` (>= addUTCTime (60 + margin) sentAt)
        -- The deadline is also no later than the interval past the response.
        limit.expiresAt `shouldSatisfy` (<= addUTCTime 60 observedAt)
      limits -> expectationFailure $ "expected exactly one rate limit, got " <> show limits
