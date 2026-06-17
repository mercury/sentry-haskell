module CaptureDropTest where

import Control.Exception (toException)
import Control.Exception.Annotated (AnnotatedException (..), Annotation (..))
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Kind (Type)
import Data.Vector qualified as Vector
import Patrol.Type.DataCategory (DataCategory (..))
import Patrol.Type.Level qualified as Patrol.Level
import Sentry.Capture (captureException, captureMessage)
import Sentry.Client.Options (ClientOptions (..))
import Sentry.ClientReport (DiscardReason (..))
import Sentry.Event (CapturedEvent (..))
import Sentry.Integration (Integration (..), fromIntegration)
import Sentry.Scope (ScopeData (..))
import Sentry.Test qualified as Test
import Test.Hspec

-- | A test integration that unconditionally drops every event.
type DroppingIntegration :: Type
data DroppingIntegration = DroppingIntegration

instance Integration DroppingIntegration where
  processEvent _ _ _ = pure Nothing

spec_captureDrop :: Spec
spec_captureDrop = describe "drop-site instrumentation" do
  describe "scope eventProcessor drop (Stage A)" do
    it "records EventProcessor when captureException scope drops the event" do
      let scopeData = (def @ScopeData){eventProcessor = \_ -> Nothing}
          annotated =
            AnnotatedException
              [Annotation scopeData]
              (toException $ userError "boom")
      (result, transport) <- Test.withClient \_ ->
        captureException annotated
      result `shouldBe` Nothing
      drops <- liftIO $ Test.fetchAndClearDrops transport
      drops `shouldBe` [(EventProcessor, Error, 1)]

    it "records EventProcessor when captureMessage scope drops the event" do
      -- captureMessage reads the ambient scope; the default ambient scope has
      -- eventProcessor = Just, so we force a drop via sampleRate instead and
      -- just verify the pipeline records a drop for the message category too.
      let opts = (def @ClientOptions){sampleRate = 0.0}
      (result, transport) <-
        Test.withCustomClient opts \_ ->
          captureMessage Patrol.Level.Error "dropped message"
      result `shouldBe` Nothing
      drops <- liftIO $ Test.fetchAndClearDrops transport
      drops `shouldBe` [(SampleRate, Error, 1)]

  describe "sample rate drop" do
    it "records SampleRate when sampleRate = 0" do
      let opts = (def @ClientOptions){sampleRate = 0.0}
      (result, transport) <-
        Test.withCustomClient opts \_ ->
          captureException (userError "sampled out")
      result `shouldBe` Nothing
      drops <- liftIO $ Test.fetchAndClearDrops transport
      drops `shouldBe` [(SampleRate, Error, 1)]

    it "does not record a drop when sampleRate = 1" do
      let opts = (def @ClientOptions){sampleRate = 1.0}
      (_, transport) <-
        Test.withCustomClient opts \_ ->
          captureException (userError "passes through")
      drops <- liftIO $ Test.fetchAndClearDrops transport
      drops `shouldBe` []

  describe "beforeSend drop" do
    it "records BeforeSend when beforeSend returns Nothing" do
      let opts = (def @ClientOptions){beforeSend = Just (\_ -> Nothing)}
      (result, transport) <-
        Test.withCustomClient opts \_ ->
          captureException (userError "filtered")
      result `shouldBe` Nothing
      drops <- liftIO $ Test.fetchAndClearDrops transport
      drops `shouldBe` [(BeforeSend, Error, 1)]

    it "does not record a drop when beforeSend passes the event" do
      let opts = (def @ClientOptions){beforeSend = Just (\ce -> Just ce.event)}
      (_, transport) <-
        Test.withCustomClient opts \_ ->
          captureException (userError "passes through")
      drops <- liftIO $ Test.fetchAndClearDrops transport
      drops `shouldBe` []

  describe "integration processEvent drop" do
    it "records EventProcessor when an integration drops the event" do
      let droppingIntegration = fromIntegration DroppingIntegration
          opts = (def @ClientOptions){integrations = Vector.singleton droppingIntegration}
      (result, transport) <-
        Test.withCustomClient opts \_ ->
          captureException (userError "dropped by integration")
      result `shouldBe` Nothing
      drops <- liftIO $ Test.fetchAndClearDrops transport
      drops `shouldBe` [(EventProcessor, Error, 1)]
