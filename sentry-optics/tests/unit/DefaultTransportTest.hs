{-# LANGUAGE OverloadedLabels #-}

module DefaultTransportTest where

import Control.Exception (bracket)
import Data.Default (def)
import Data.Maybe (isJust)
import Sentry.Client (Client (..))
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Optics qualified as Sentry
import Sentry.Optics.Prelude
import Sentry.Test (withGlobalScope)
import Sentry.Test qualified as Test
import Test.Hspec

spec_defaultTransport :: Spec
spec_defaultTransport =
  describe "Sentry.Optics.init / withSentry (default transport)" do
    it "realizes a transport when ClientOptions.transport is left Nothing" do
      let opts = def{dsn = Just Test.TEST_DSN}
      withGlobalScope $
        bracket (Sentry.init opts) Sentry.close \client ->
          isJust client.transport `shouldBe` True

spec_opticsSurface :: Spec
spec_opticsSurface =
  describe "Sentry.Optics re-exports Sentry.Core.Optics" do
    it "emptyUser + optics operators compose end to end" do
      let user = Sentry.emptyUser & #email .~ "alice@example.com"
      (user ^. #email) `shouldBe` "alice@example.com"
