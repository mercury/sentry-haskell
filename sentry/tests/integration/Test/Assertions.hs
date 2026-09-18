module Test.Assertions (assertOK, bounded) where

import Network.HTTP.Types qualified as Http
import Sentry.Transport.HTTP.Delivery qualified as Delivery
import System.Timeout (timeout)
import Test.Hspec (expectationFailure, shouldBe, shouldReturn)

-- | Assert that delivery received an HTTP 200 response.
assertOK :: Delivery.Outcome -> IO ()
assertOK = \case
  Delivery.Responded _ status _ -> status `shouldBe` Http.status200
  Delivery.NetworkFailure err -> expectationFailure (show err)

-- | Fail an integration test if it does not finish within fifteen seconds.
bounded :: IO () -> IO ()
bounded action = timeout 15_000_000 action `shouldReturn` Just ()
