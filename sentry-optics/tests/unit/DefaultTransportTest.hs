{-# LANGUAGE OverloadedLabels #-}

module DefaultTransportTest where

import Control.Exception (bracket)
import Data.Default (def)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (isJust, isNothing)
import Sentry qualified as Plain
import Sentry.Client.Options (ClientOptions (..), TransportProvider (..))
import Sentry.Client.Options.Dsn qualified as Dsn
import Sentry.Optics qualified as Sentry
import Sentry.Optics.Prelude
import Sentry.Scope qualified as Scope
import Sentry.Test (withGlobalScope)
import Sentry.Test qualified as Test
import Sentry.Transport (SomeTransport (..))
import Test.Hspec

spec_defaultTransport :: Spec
spec_defaultTransport =
  describe "Sentry.Optics.init / withSentry (default transport)" do
    it "Disabled creates no default transport through either facade" do
      bracket (Sentry.acquireClient def{dsn = Dsn.Disabled}) Sentry.close \handle ->
        isNothing (Sentry.clientOf handle).transport `shouldBe` True
      bracket (Plain.acquireClient def{dsn = Dsn.Disabled}) Plain.close \handle ->
        isNothing (Plain.clientOf handle).transport `shouldBe` True

    it "realizes a transport when ClientOptions.transport is left Nothing" do
      let opts = def{dsn = Dsn.Explicit Test.TEST_DSN}
      withGlobalScope $
        bracket (Sentry.init opts) Sentry.close \handle ->
          isJust (Sentry.clientOf handle).transport `shouldBe` True

    it "acquires the default transport without installing a global binding" $ withGlobalScope do
      let opts = def{dsn = Dsn.Explicit Test.TEST_DSN}
      bracket (Sentry.acquireClient opts) Sentry.close \handle -> do
        isJust (Sentry.clientOf handle).transport `shouldBe` True
        isJust <$> Scope.lookupClient `shouldReturn` False
      isJust <$> Scope.lookupClient `shouldReturn` False

    it "supplies the default transport for scoped ownership" $ withGlobalScope do
      global <- Scope.getGlobal
      Sentry.withScopedClient def{dsn = Dsn.Explicit Test.TEST_DSN} do
        client <- Scope.resolveClient
        isJust client.transport `shouldBe` True
        snapshot <- Scope.readScopeRef global
        isJust snapshot.client `shouldBe` False
      isJust <$> Scope.lookupClient `shouldReturn` False

    it "preserves an explicitly configured scoped transport factory" $ withGlobalScope do
      called <- newIORef False
      transport <- Test.new
      let provider = DeferredTransport \_ _ -> do
            writeIORef called True
            pure (SomeTransport transport)
      Sentry.withScopedClient def{dsn = Dsn.Explicit Test.TEST_DSN, transport = Just provider} do
        readIORef called `shouldReturn` True
      isJust <$> Scope.lookupClient `shouldReturn` False

spec_opticsSurface :: Spec
spec_opticsSurface =
  describe "Sentry.Optics re-exports Sentry.Core.Optics" do
    it "emptyUser + optics operators compose end to end" do
      let user = Sentry.emptyUser & #email .~ "alice@example.com"
      (user ^. #email) `shouldBe` "alice@example.com"

spec_captureFacade :: Spec
spec_captureFacade = describe "capture facade" do
  it "exports capture outcomes and transport acceptance" do
    Sentry.withClient Sentry.NON_RECORDING_CLIENT $
      Sentry.captureEvent Sentry.emptyEvent `shouldReturn` Nothing
