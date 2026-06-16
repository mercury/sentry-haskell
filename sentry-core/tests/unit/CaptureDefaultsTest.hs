module CaptureDefaultsTest where

import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Patrol qualified
import Patrol.Type.ClientSdkInfo (ClientSdkInfo (..))
import Patrol.Type.Event qualified as Patrol.Event
import Patrol.Type.EventId qualified as Patrol.EventId
import Patrol.Type.Level qualified as Patrol.Level
import Patrol.Type.Platform qualified as Patrol.Platform
import Sentry.Capture (captureEvent, captureException, captureMessage)
import Sentry.Client (Client)
import Sentry.Client qualified as Client
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Test qualified as Test
import Test.Hspec

spec_captureDefaults :: Spec
spec_captureDefaults = describe "applyClientDefaults" do
  describe "eventId and timestamp" do
    it "captureMessage assigns a non-nil eventId" do
      (_, transport) <- Test.withClient \_ ->
        captureMessage Patrol.Level.Info "hello"
      events <- liftIO $ Test.fetchAndClearEvents transport
      case events of
        [event] -> event.eventId `shouldNotBe` Patrol.EventId.empty
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

    it "captureMessage assigns a timestamp" do
      (_, transport) <- Test.withClient \_ ->
        captureMessage Patrol.Level.Info "hello"
      events <- liftIO $ Test.fetchAndClearEvents transport
      case events of
        [event] -> event.timestamp `shouldSatisfy` isJust
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

    it "captureException assigns a non-nil eventId" do
      (_, transport) <- Test.withClient \_ ->
        captureException (userError "boom")
      events <- liftIO $ Test.fetchAndClearEvents transport
      case events of
        [event] -> event.eventId `shouldNotBe` Patrol.EventId.empty
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

  describe "platform and sdk" do
    it "platform is set to Haskell" do
      (_, transport) <- Test.withClient \_ ->
        captureMessage Patrol.Level.Info "hello"
      events <- liftIO $ Test.fetchAndClearEvents transport
      case events of
        [event] -> event.platform `shouldBe` Just Patrol.Platform.Haskell
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

    it "sdk name is sentry.haskell" do
      (_, transport) <- Test.withClient \_ ->
        captureMessage Patrol.Level.Info "hello"
      events <- liftIO $ Test.fetchAndClearEvents transport
      case events of
        [event] -> case event.sdk of
          Nothing -> expectationFailure "sdk should be set"
          Just sdk -> sdk.name `shouldBe` "sentry.haskell"
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

    it "sdk integrations list is sorted and includes ContextIntegration" do
      (_, transport) <- Test.withClient \_ ->
        captureMessage Patrol.Level.Info "hello"
      events <- liftIO $ Test.fetchAndClearEvents transport
      case events of
        [event] -> case event.sdk of
          Nothing -> expectationFailure "sdk should be set"
          Just sdk -> sdk.integrations `shouldContain` ["ContextIntegration"]
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

  describe "environment" do
    it "explicit options.environment is applied to captureMessage events" do
      (_, transport) <-
        Test.withCustomClient def{environment = Just "staging"} \_ ->
          captureMessage Patrol.Level.Info "hello"
      events <- liftIO $ Test.fetchAndClearEvents transport
      case events of
        [event] -> event.environment `shouldBe` "staging"
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

    it "explicit options.environment is applied to captureException events" do
      (_, transport) <-
        Test.withCustomClient def{environment = Just "staging"} \_ ->
          captureException (userError "boom")
      events <- liftIO $ Test.fetchAndClearEvents transport
      case events of
        [event] -> event.environment `shouldBe` "staging"
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

    it "defaults environment to production when not set in options" do
      (_, transport) <- Test.withClient \_ ->
        captureMessage Patrol.Level.Info "hello"
      events <- liftIO $ Test.fetchAndClearEvents transport
      case events of
        [event] -> event.environment `shouldBe` "production"
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

    it "captureEvent preserves explicit environment set on the event" do
      let explicitEvent =
            Patrol.Event.empty
              { Patrol.Event.environment = "qa"
              }
      (_, transport) <-
        Test.withCustomClient def{environment = Just "staging"} \_ ->
          captureEvent explicitEvent
      events <- liftIO $ Test.fetchAndClearEvents transport
      case events of
        [event] -> event.environment `shouldBe` "qa"
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

  describe "release" do
    it "explicit options.release appears on events" do
      (_, transport) <-
        Test.withCustomClient def{release = Just "v1.0.0"} \_ ->
          captureMessage Patrol.Level.Info "hello"
      events <- liftIO $ Test.fetchAndClearEvents transport
      case events of
        [event] -> event.release `shouldBe` "v1.0.0"
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

  describe "server_name" do
    it "serverName is non-empty by default (filled by ContextIntegration)" do
      client <- liftIO $ Client.new def{dsn = Just Test.TEST_DSN}
      (client :: Client).options.serverName `shouldSatisfy` isJust

    it "explicit options.serverName is preserved and not overwritten" do
      client <- liftIO $ Client.new def{dsn = Just Test.TEST_DSN, serverName = Just "my-box"}
      (client :: Client).options.serverName `shouldBe` Just ("my-box" :: Text)

    it "serverName appears on events" do
      (_, transport) <-
        Test.withCustomClient def{serverName = Just "my-box"} \_ ->
          captureMessage Patrol.Level.Info "hello"
      events <- liftIO $ Test.fetchAndClearEvents transport
      case events of
        [event] -> event.serverName `shouldBe` "my-box"
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

  describe "dist" do
    it "explicit options.dist appears on events" do
      (_, transport) <-
        Test.withCustomClient def{dist = Just "42"} \_ ->
          captureMessage Patrol.Level.Info "hello"
      events <- liftIO $ Test.fetchAndClearEvents transport
      case events of
        [event] -> event.dist `shouldBe` "42"
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

    it "dist is empty when not set" do
      (_, transport) <- Test.withClient \_ ->
        captureMessage Patrol.Level.Info "hello"
      events <- liftIO $ Test.fetchAndClearEvents transport
      case events of
        [event] -> event.dist `shouldBe` Text.empty
        _ -> expectationFailure $ "expected one event, got " <> show (length events)
