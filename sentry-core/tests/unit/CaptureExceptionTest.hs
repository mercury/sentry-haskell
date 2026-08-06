module CaptureExceptionTest where

import Control.Exception (AsyncException (ThreadKilled), SomeException, toException)
import Control.Exception.Annotated (AnnotatedException (..), Annotation (..))
import Control.Exception.Safe qualified as Safe
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Patrol.Type.Event qualified as Patrol.Event
import Patrol.Type.Exception qualified as Patrol.Exception
import Patrol.Type.Exceptions qualified as Patrol.Exceptions
import Patrol.Type.Level qualified as Patrol.Level
import Patrol.Type.Mechanism qualified as Patrol.Mechanism
import Sentry.Capture (captureException)
import Sentry.Mechanism qualified as Mechanism
import Sentry.Scope (ScopeData (..))
import Sentry.Scope qualified as Scope
import Sentry.Scope.IO qualified as Scope.IO
import Sentry.Test qualified as Test
import Test.Hspec

spec_captureException :: Spec
spec_captureException = describe "captureException" do
  it "captures a plain exception with no annotation as an event" do
    (_, transport) <- Test.withClient \_ ->
      captureException (userError "boom")
    events <- Test.fetchAndClearEvents transport
    length events `shouldBe` 1

  it "uses ScopeData from AnnotatedException when present (authoritative)" do
    let scopeData = (def @ScopeData){level = Just Patrol.Level.Warning}
        annotated =
          AnnotatedException
            [Annotation scopeData]
            (toException $ userError "boom")
    (_, transport) <- Test.withClient \_ ->
      captureException annotated
    events <- Test.fetchAndClearEvents transport
    case events of
      [event] -> event.level `shouldBe` Just Patrol.Level.Warning
      _ -> expectationFailure $ "expected one event, got " <> show (length events)

  it "drops the event when scope's eventProcessor returns Nothing" do
    let scopeData = (def @ScopeData){eventProcessor = \_ -> Nothing}
        annotated =
          AnnotatedException
            [Annotation scopeData]
            (toException $ userError "boom")
    (result, transport) <- Test.withClient \_ ->
      captureException annotated
    result `shouldBe` Nothing
    events <- Test.fetchAndClearEvents transport
    events `shouldSatisfy` null

  it "uses ambient thread-local scope when no annotation is present" do
    (_, transport) <- Test.withClient \_ ->
      Scope.IO.withScope \scope -> do
        Scope.setLevel scope Patrol.Level.Warning
        captureException (userError "boom")
    events <- Test.fetchAndClearEvents transport
    case events of
      [event] -> event.level `shouldBe` Just Patrol.Level.Warning
      _ -> expectationFailure $ "expected one event, got " <> show (length events)

  it "preserves throw-site scope when an exception escapes withScope" do
    (_, transport) <- Test.withClient \_ -> do
      result <- Safe.try @_ @(AnnotatedException SomeException) $ Scope.IO.withScope \scope -> do
        Scope.setLevel scope Patrol.Level.Warning
        Safe.throwIO $ userError "escapes scope"
      case result of
        Right _ -> liftIO $ expectationFailure "expected an exception to be thrown"
        Left annotated -> void $ captureException annotated
    events <- Test.fetchAndClearEvents transport
    case events of
      [event] -> event.level `shouldBe` Just Patrol.Level.Warning
      _ -> expectationFailure $ "expected one event, got " <> show (length events)

  it "captures an async exception value as an event" do
    (_, transport) <- Test.withClient \_ ->
      captureException ThreadKilled
    events <- Test.fetchAndClearEvents transport
    length events `shouldBe` 1

  it "attaches the generic, handled mechanism (see CaptureMechanismTest for detail)" do
    (_, transport) <- Test.withClient \_ ->
      captureException (userError "boom")
    events <- Test.fetchAndClearEvents transport
    case events of
      [event] -> lastMechanism event `shouldBe` Just Mechanism.generic
      _ -> expectationFailure $ "expected one event, got " <> show (length events)

-- | The 'Patrol.Mechanism' attached to the last exception value in an event,
-- if any.
lastMechanism :: Patrol.Event.Event -> Maybe Patrol.Mechanism.Mechanism
lastMechanism event = do
  excs <- event.exception
  case Patrol.Exceptions.values excs of
    [] -> Nothing
    xs -> Patrol.Exception.mechanism (last xs)
