module ClientResolutionTest where

import Patrol.Type.Level qualified as Patrol.Level
import Sentry qualified
import Sentry.Test (withoutGlobalClient)
import Sentry.Test qualified as Test
import Test.Hspec

spec_clientResolution :: Spec
spec_clientResolution = describe "client resolution (client-on-scope)" do
  it "shares the bound client across a nested withScope by default" do
    (_, transport) <- Test.withClient \_ ->
      Sentry.withScope \_ ->
        Sentry.captureMessage Patrol.Level.Info "nested current scope"
    events <- Test.fetchAndClearEvents transport
    length events `shouldBe` 1

  it "inherits the client across a nested withIsolationScope" do
    (_, transport) <- Test.withClient \_ ->
      Sentry.withIsolationScope \_ ->
        Sentry.captureMessage Patrol.Level.Info "nested isolation scope"
    events <- Test.fetchAndClearEvents transport
    length events `shouldBe` 1

  it "an inner withClient overrides the outer client for its extent" do
    transportA <- Test.new
    transportB <- Test.new
    let clientA = Test.mkClient transportA
        clientB = Test.mkClient transportB
    Sentry.withClient clientA do
      _ <- Sentry.withClient clientB $ Sentry.captureMessage Patrol.Level.Info "goes to B"
      _ <- Sentry.captureMessage Patrol.Level.Info "goes to A"
      pure ()
    eventsA <- Test.fetchAndClearEvents transportA
    eventsB <- Test.fetchAndClearEvents transportB
    length eventsA `shouldBe` 1
    length eventsB `shouldBe` 1

  it "resolves a non-recording client when nothing is bound (capture no-ops)" do
    result <- withoutGlobalClient $ Sentry.captureMessage Patrol.Level.Info "into the void"
    result `shouldBe` Nothing
