module CaptureMechanismTest where

import Control.Exception (toException)
import Control.Exception.Annotated (AnnotatedException (..), Annotation (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.Default (def)
import Patrol.Type.Event qualified as Patrol.Event
import Patrol.Type.Exception qualified as Patrol.Exception
import Patrol.Type.Exceptions qualified as Patrol.Exceptions
import Patrol.Type.Level qualified as Patrol.Level
import Patrol.Type.Mechanism qualified as Patrol.Mechanism
import Sentry.Capture (CaptureOverrides (..), captureException, captureExceptionWith, captureMessage, captureUnhandledException)
import Sentry.Mechanism qualified as Mechanism
import Sentry.Scope (ScopeData (..))
import Sentry.Scope qualified as Scope
import Sentry.Scope.IO qualified as Scope.IO
import Sentry.Test qualified as Test
import Test.Hspec

-- | The 'Patrol.Mechanism' attached to the last exception value in an event,
-- if any.
lastMechanism :: Patrol.Event.Event -> Maybe Patrol.Mechanism.Mechanism
lastMechanism event = do
  exceptions <- event.exception
  case Patrol.Exceptions.values exceptions of
    [] -> Nothing
    excs -> Patrol.Exception.mechanism (last excs)

spec_captureMechanism :: Spec
spec_captureMechanism = describe "mechanism.handled" do
  describe "captureException" do
    it "attaches {type_ = \"generic\", handled = Just True}" do
      (_, transport) <- Test.withClient \_ ->
        captureException (userError "boom")
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] -> lastMechanism event `shouldBe` Just Mechanism.generic
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

  describe "captureUnhandledException" do
    it "attaches {type_ = ty, handled = Just False}" do
      (_, transport) <- Test.withClient \_ ->
        captureUnhandledException "warp.onException" (userError "escaped")
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] -> lastMechanism event `shouldBe` Just (Mechanism.unhandled "warp.onException")
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

    it "leaves the event level at Error" do
      (_, transport) <- Test.withClient \_ ->
        captureUnhandledException "warp.onException" (userError "escaped")
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] -> event.level `shouldBe` Just Patrol.Level.Error
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

  describe "captureExceptionWith" do
    it "attaches no mechanism by default" do
      (_, transport) <- Test.withClient \_ ->
        captureExceptionWith def (userError "no override")
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] -> lastMechanism event `shouldBe` Nothing
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

    it "normalizes an empty mechanism type_ to \"generic\"" do
      (_, transport) <- Test.withClient \_ ->
        captureExceptionWith def{mechanismOverride = Just Patrol.Mechanism.empty} (userError "empty type_")
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] -> (Patrol.Mechanism.type_ <$> lastMechanism event) `shouldBe` Just "generic"
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

    it "overrideLevel beats an ambient scope's setLevel" do
      (_, transport) <- Test.withClient \_ ->
        Scope.IO.withScope \scope -> do
          Scope.setLevel scope Patrol.Level.Error
          captureExceptionWith def{levelOverride = Just Patrol.Level.Warning} (userError "override wins")
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] -> event.level `shouldBe` Just Patrol.Level.Warning
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

    it "without overrideLevel, the ambient scope's setLevel still wins (regression)" do
      (_, transport) <- Test.withClient \_ ->
        Scope.IO.withScope \scope -> do
          Scope.setLevel scope Patrol.Level.Warning
          captureExceptionWith def (userError "no override")
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] -> event.level `shouldBe` Just Patrol.Level.Warning
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

    it "overrideLevel beats a throw-site ScopeData annotation" do
      let scopeData = (def @ScopeData){level = Just Patrol.Level.Error}
          annotated =
            AnnotatedException
              [Annotation scopeData]
              (toException $ userError "boom")
      (_, transport) <- Test.withClient \_ ->
        captureExceptionWith def{levelOverride = Just Patrol.Level.Warning} annotated
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] -> event.level `shouldBe` Just Patrol.Level.Warning
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

    it "applies both a mechanism and a level override together" do
      (_, transport) <- Test.withClient \_ ->
        captureExceptionWith
          def
            { mechanismOverride = Just (Mechanism.unhandled "custom.boundary"),
              levelOverride = Just Patrol.Level.Fatal
            }
          (userError "both")
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] -> do
          lastMechanism event `shouldBe` Just (Mechanism.unhandled "custom.boundary")
          event.level `shouldBe` Just Patrol.Level.Fatal
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

  describe "captureMessage" do
    it "attaches no mechanism (message events carry no exception)" do
      (_, transport) <- Test.withClient \_ ->
        captureMessage Patrol.Level.Info "hello"
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] -> event.exception `shouldSatisfy` (== Nothing)
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

  describe "singleton invariant" do
    it "exception.values has exactly one entry for a captureException event" do
      (_, transport) <- Test.withClient \_ ->
        captureException (userError "boom")
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] -> (length . Patrol.Exceptions.values <$> event.exception) `shouldBe` Just 1
        _ -> expectationFailure $ "expected one event, got " <> show (length events)

  describe "wire format" do
    it "serializes handled = false and a non-empty type onto the wire" do
      (_, transport) <- Test.withClient \_ ->
        captureUnhandledException "warp.onException" (userError "escaped")
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] -> case Aeson.toJSON event of
          Aeson.Object obj -> case Aeson.Types.parseMaybe extractMechanism obj of
            Just m ->
              m
                `shouldBe` Aeson.object
                  [ "type" Aeson..= Aeson.String "warp.onException",
                    "handled" Aeson..= Aeson.Bool False
                  ]
            Nothing -> expectationFailure "expected exception.values[0].mechanism to be present"
          _ -> expectationFailure "expected event to serialize to a JSON object"
        _ -> expectationFailure $ "expected one event, got " <> show (length events)
  where
    extractMechanism obj = do
      exception <- obj Aeson..: "exception"
      values <- exception Aeson..: "values"
      case values of
        (v : _) -> v Aeson..: "mechanism"
        [] -> fail "no exception values"
