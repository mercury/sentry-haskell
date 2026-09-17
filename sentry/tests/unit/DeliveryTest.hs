{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module DeliveryTest where

import Data.Foldable (for_)
import Data.Time.Clock (UTCTime (..))
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
