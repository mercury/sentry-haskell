module CaptureDropTest where

import Control.Exception (toException)
import Control.Exception.Annotated (AnnotatedException (..), Annotation (..))
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Vector qualified as Vector
import Patrol qualified
import Patrol.Type.Breadcrumb qualified as Patrol.Breadcrumb
import Patrol.Type.Breadcrumbs qualified as Patrol.Breadcrumbs
import Patrol.Type.DataCategory (DataCategory (..))
import Patrol.Type.Event qualified as Patrol.Event
import Patrol.Type.Level qualified as Patrol.Level
import Patrol.Type.User qualified as Patrol.User
import Sentry.Capture (captureEvent, captureException, captureExceptionWith, captureMessage, captureUnhandledException)
import Sentry.Client.Options (ClientOptions (..))
import Sentry.ClientReport (DiscardReason (..))
import Sentry.Event qualified
import Sentry.Event.Captured (CapturedEvent (..))
import Sentry.Integration (Integration (..), fromIntegration)
import Sentry.Scope.IO qualified as Scope.IO
import Sentry.Scope.Operations (ScopeData (..))
import Sentry.Scope.Operations qualified as Scope
import Sentry.Scope.Update qualified as Scope.Update
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
    it "records EventProcessor when captureEvent scope drops the event" do
      (result, transport) <- Test.withClient \_ ->
        Scope.IO.withScope \scope -> do
          Scope.setEventProcessor scope (const Nothing)
          captureEvent Patrol.Event.empty
      result `shouldBe` Nothing
      events <- liftIO $ Test.fetchAndClearEvents transport
      events `shouldSatisfy` null
      drops <- liftIO $ Test.fetchAndClearDrops transport
      drops `shouldBe` [(EventProcessor, Error, 1)]

    it "applies scoped metadata and a processor once to captureEvent" do
      let scopeCrumb = crumb "scope"
          processorCrumb = crumb "processor"
          process ce =
            let crumbs = foldMap Patrol.Breadcrumbs.values ce.event.breadcrumbs
             in Just $ Sentry.Event.apply ce.event (Sentry.Event.setBreadcrumbs (Patrol.Breadcrumbs.Breadcrumbs (crumbs <> [processorCrumb])))
      (result, transport) <- Test.withClient \_ ->
        Scope.IO.withScope \scope -> do
          Scope.setTag scope "scope-tag" "present"
          Scope.setUser scope testUser
          Scope.Update.apply scope (Scope.Update.appendBreadcrumb scopeCrumb)
          Scope.setEventProcessor scope process
          captureEvent Patrol.Event.empty
      result `shouldSatisfy` (/= Nothing)
      events <- liftIO $ Test.fetchAndClearEvents transport
      case events of
        [event] -> do
          event.tags `shouldBe` Map.singleton "scope-tag" "present"
          event.user `shouldBe` Just testUser
          map (.message) (fromMaybe [] (Patrol.Breadcrumbs.values <$> event.breadcrumbs))
            `shouldBe` ["scope", "processor"]
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

    it "records EventProcessor when captureException scope drops the event" do
      let scopeData = (def @ScopeData){eventProcessor = \_ -> Nothing}
          annotated =
            AnnotatedException
              [Annotation scopeData]
              (toException $ userError "boom")
      (result, transport) <- Test.withClient \_ ->
        captureException annotated
      result `shouldBe` Nothing
      events <- liftIO $ Test.fetchAndClearEvents transport
      events `shouldSatisfy` null
      drops <- liftIO $ Test.fetchAndClearDrops transport
      drops `shouldBe` [(EventProcessor, Error, 1)]

    it "records EventProcessor when captureExceptionWith scope drops the event" do
      let scopeData = (def @ScopeData){eventProcessor = \_ -> Nothing}
          annotated =
            AnnotatedException
              [Annotation scopeData]
              (toException $ userError "boom")
      (result, transport) <- Test.withClient \_ ->
        captureExceptionWith def annotated
      result `shouldBe` Nothing
      drops <- liftIO $ Test.fetchAndClearDrops transport
      drops `shouldBe` [(EventProcessor, Error, 1)]

    it "records EventProcessor when captureUnhandledException scope drops the event" do
      let scopeData = (def @ScopeData){eventProcessor = \_ -> Nothing}
          annotated =
            AnnotatedException
              [Annotation scopeData]
              (toException $ userError "boom")
      (result, transport) <- Test.withClient \_ ->
        captureUnhandledException "warp.onException" annotated
      result `shouldBe` Nothing
      drops <- liftIO $ Test.fetchAndClearDrops transport
      drops `shouldBe` [(EventProcessor, Error, 1)]

    it "records EventProcessor when captureMessage scope drops the event" do
      (result, transport) <- Test.withClient \_ ->
        Scope.IO.withScope \scope -> do
          Scope.setEventProcessor scope (const Nothing)
          captureMessage Patrol.Level.Error "dropped message"
      result `shouldBe` Nothing
      events <- liftIO $ Test.fetchAndClearEvents transport
      events `shouldSatisfy` null
      drops <- liftIO $ Test.fetchAndClearDrops transport
      drops `shouldBe` [(EventProcessor, Error, 1)]

  describe "sample rate drop" do
    it "records SampleRate when sampleRate = 0" do
      let opts = (def @ClientOptions){sampleRate = Just 0.0}
      (result, transport) <-
        Test.withCustomClient opts \_ ->
          captureException (userError "sampled out")
      result `shouldBe` Nothing
      drops <- liftIO $ Test.fetchAndClearDrops transport
      drops `shouldBe` [(SampleRate, Error, 1)]

    it "does not record a drop when sampleRate = 1" do
      let opts = (def @ClientOptions){sampleRate = Just 1.0}
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

  describe "drop precedence (sampling runs last)" do
    it "attributes to BeforeSend, not SampleRate, when both would drop" do
      -- Sampling is the final gate, so a 'beforeSend' rejection is recorded
      -- even when 'sampleRate = 0' would also have dropped the event. This
      -- guards the spec-mandated filter order (processors -> beforeSend ->
      -- sampling) against regressing to sample-first.
      let opts =
            (def @ClientOptions)
              { sampleRate = Just 0.0,
                beforeSend = Just (\_ -> Nothing)
              }
      (result, transport) <-
        Test.withCustomClient opts \_ ->
          captureException (userError "rejected and sampled out")
      result `shouldBe` Nothing
      drops <- liftIO $ Test.fetchAndClearDrops transport
      drops `shouldBe` [(BeforeSend, Error, 1)]

    it "attributes to EventProcessor, not SampleRate, when an integration drops" do
      let droppingIntegration = fromIntegration DroppingIntegration
          opts =
            (def @ClientOptions)
              { sampleRate = Just 0.0,
                integrations = Vector.singleton droppingIntegration
              }
      (result, transport) <-
        Test.withCustomClient opts \_ ->
          captureException (userError "dropped by integration, also sampled out")
      result `shouldBe` Nothing
      drops <- liftIO $ Test.fetchAndClearDrops transport
      drops `shouldBe` [(EventProcessor, Error, 1)]

crumb :: Text -> Patrol.Breadcrumb
crumb msg = Patrol.Breadcrumb.empty{Patrol.Breadcrumb.message = msg}

testUser :: Patrol.User
testUser =
  Patrol.User.User
    { data_ = mempty,
      email = "alice@example.com",
      geo = Nothing,
      id = "user-1",
      ipAddress = "",
      name = "Alice",
      segment = "",
      username = "alice"
    }
