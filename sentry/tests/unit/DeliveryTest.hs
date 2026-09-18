{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module DeliveryTest where

import Control.Concurrent (threadDelay)
import Data.Foldable (for_)
import Data.Time.Clock (NominalDiffTime, UTCTime (..), addUTCTime, getCurrentTime)
import Network.HTTP.Types qualified as Http
import Patrol.Type.DataCategory qualified as DataCategory
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

spec_policy :: Spec
spec_policy = describe "HTTP delivery policy" do
  let now = UTCTime (toEnum 0) 0
      respond status headers = HTTP.interpret now (HTTP.Responded status headers)
      sentryLimits = ("X-Sentry-Rate-Limits", "120:error:project") :: Http.Header

  it "accepts a successful delivery" do
    (respond Http.status200 []).disposition `shouldBe` Delivery.Accepted

  -- Sentry's spec gives X-Sentry-Rate-Limits precedence over Retry-After, and
  -- says the header may appear on any response including a 200.
  it "prefers X-Sentry-Rate-Limits over Retry-After" do
    let outcome = respond Http.status200 [sentryLimits, ("Retry-After", "30")]
    outcome.rateLimits `shouldBe` [Delivery.categoryUntil DataCategory.Error (addUTCTime 120 now)]

  it "falls back to Retry-After when no category limits are announced" do
    (respond Http.status200 [("Retry-After", "30")]).rateLimits
      `shouldBe` [Delivery.allCategoriesUntil (addUTCTime 30 now)]

  for_ [400, 429, 500] \code ->
    it ("applies announced category limits on HTTP " <> show code) do
      let outcome = respond (Http.mkStatus code "test") [sentryLimits]
      outcome.rateLimits `shouldBe` [Delivery.categoryUntil DataCategory.Error (addUTCTime 120 now)]

  it "applies a 429's Retry-After rather than the sixty-second fallback" do
    let outcome = respond Http.tooManyRequests429 [("Retry-After", "300")]
    outcome.disposition `shouldBe` Delivery.Rejected Delivery.AccountedUpstream
    outcome.rateLimits `shouldBe` [Delivery.allCategoriesUntil (addUTCTime 300 now)]

  it "falls back to sixty seconds on a 429 carrying no usable headers" do
    (respond Http.tooManyRequests429 []).rateLimits
      `shouldBe` [Delivery.allCategoriesUntil (addUTCTime 60 now)]

  -- The group parser requires the scope field, so this header yields nothing.
  -- Keying the fallback on "no limits" rather than "no header" is what keeps
  -- such a response from producing no backoff at all.
  it "falls back to sixty seconds on a 429 whose header yields no limits" do
    (respond Http.tooManyRequests429 [("X-Sentry-Rate-Limits", "60:error")]).rateLimits
      `shouldBe` [Delivery.allCategoriesUntil (addUTCTime 60 now)]

  it "announces no limits when a non-429 response carries no headers" do
    (respond (Http.mkStatus 500 "test") []).rateLimits `shouldBe` []

  -- Matches sentry-rust's update_from_retry_after and sentry-javascript's
  -- parseRetryAfterHeader, both of which fall back to sixty seconds for a
  -- value neither parser understood, on any status.
  it "falls back to sixty seconds on an unparseable Retry-After, whatever the status" do
    (respond (Http.mkStatus 502 "bad gateway") [("Retry-After", "soon")]).rateLimits
      `shouldBe` [Delivery.allCategoriesUntil (addUTCTime 60 now)]

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
