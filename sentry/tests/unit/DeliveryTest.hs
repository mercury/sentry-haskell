{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module DeliveryTest where

import Data.Foldable (for_)
import Network.HTTP.Types qualified as Http
import Sentry.ClientReport qualified as Report
import Sentry.Transport.Delivery qualified as Delivery
import Test.Hspec

spec_classification :: Spec
spec_classification = describe "delivery discard classification" do
  for_ [200, 201, 204, 299, 429] \code ->
    it ("does not report HTTP " <> show code) $
      Delivery.discardReason (Delivery.Responded (Http.mkStatus code "test") [])
        `shouldBe` Nothing
  for_ [300, 400, 413, 500] \code ->
    it ("reports HTTP " <> show code <> " as send_error") $
      Delivery.discardReason (Delivery.Responded (Http.mkStatus code "test") [])
        `shouldBe` Just Report.SendError
  it "reports network failures" $
    Delivery.discardReason (Delivery.NetworkFailure "connection failed")
      `shouldBe` Just Report.NetworkError
