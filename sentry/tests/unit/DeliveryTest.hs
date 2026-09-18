module DeliveryTest where

import Data.Foldable (for_)
import Data.Time.Clock (UTCTime (..), addUTCTime)
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
      Delivery.discardReason (HTTP.interpret $ HTTP.Responded (UTCTime (toEnum 0) 0) (Http.mkStatus code "test") [])
        `shouldBe` Nothing
  for_ [300, 400, 413, 500] \code ->
    it ("reports HTTP " <> show code <> " as send_error") $
      Delivery.discardReason (HTTP.interpret $ HTTP.Responded (UTCTime (toEnum 0) 0) (Http.mkStatus code "test") [])
        `shouldBe` Just Report.SendError
  it "reports network failures" $
    Delivery.discardReason (HTTP.interpret $ HTTP.NetworkFailure "connection failed")
      `shouldBe` Just Report.NetworkError

spec_policy :: Spec
spec_policy = describe "HTTP delivery policy" do
  let now = UTCTime (toEnum 0) 0
      respond status headers = HTTP.interpret (HTTP.Responded now status headers)
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

-- | Relative deadlines use header receipt time, independently of when the
-- response is subsequently interpreted.
spec_deadlineTiming :: Spec
spec_deadlineTiming = describe "rate-limit deadline timing" do
  for_ [[("Retry-After", "60")], [("X-Sentry-Rate-Limits", "60::project")], []] \headers ->
    it ("dates the limit from the response timestamp: " <> show headers) do
      let receivedAt = UTCTime (toEnum 0) 10
          response = HTTP.Responded receivedAt Http.tooManyRequests429 headers
      (HTTP.interpret response).rateLimits
        `shouldBe` [Delivery.allCategoriesUntil (addUTCTime 60 receivedAt)]
