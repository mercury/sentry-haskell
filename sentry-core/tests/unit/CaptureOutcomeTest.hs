module CaptureOutcomeTest where

import Control.Exception (toException)
import Control.Exception.Annotated (AnnotatedException (..), Annotation (..))
import Data.Default (def)
import Data.Foldable (for_)
import Data.Kind (Type)
import Patrol.Type.Event qualified as Event
import Patrol.Type.EventId qualified as EventId
import Patrol.Type.Frame qualified as Frame
import Patrol.Type.Level qualified as Level
import Sentry.Client qualified as Client
import Sentry.Client.Options.Dsn qualified as Dsn
import Sentry.Core
import Sentry.Event qualified
import Sentry.Scope.Operations qualified as Scope
import Sentry.Test qualified as Test
import Sentry.Transport (SendResponse (..))
import Sentry.Transport qualified as Transport
import StacktraceTest (exceptionFrames, threadFrames)
import Test.Hspec

-- | Custom transport preserving existing instance compatibility.
type Responding :: Type
data Responding = Responding SendResponse Test.TestTransport

instance Transport.Transport Responding where
  send (Responding response recorded) envelope = response <$ Transport.send recorded envelope
  recordDiscards (Responding _ recorded) = Transport.recordDiscards recorded

spec_captureOutcome :: Spec
spec_captureOutcome = describe "capture outcomes" do
  for_ [SendProcessed, SendFailed_Other, SendFailed_Shutdown, SendFailed_QueueFull] \response ->
    it ("returns the final ID only for " <> show response <> " without duplicate drops") do
      recorded <- Test.new
      finalId <- EventId.random
      client <- Client.new def{dsn = Dsn.Explicit Test.TEST_DSN, beforeSend = Just (\ce -> Just (Sentry.Event.apply ce.event (Sentry.Event.setEventId finalId))), transport = Just (PrebuiltTransport (Transport.SomeTransport (Responding response recorded)))}
      withClient client do
        result <- captureEvent Event.empty
        events <- Test.fetchAndClearEvents recorded
        case events of
          [event] -> do
            event.eventId `shouldBe` finalId
            result `shouldBe` if response == SendProcessed then Just event.eventId else Nothing
          _ -> expectationFailure "expected one event"
      Test.fetchAndClearDrops recorded `shouldReturn` []
  it "reports disabled clients after scope processing" do
    withClient NON_RECORDING_CLIENT do
      captureEvent Event.empty `shouldReturn` Nothing
      withScope \scope -> do
        Scope.setEventProcessor scope (const Nothing)
        captureEvent Event.empty `shouldReturn` Nothing
  it "uses every capture verb and honors annotated scope drops" do
    let annotated = AnnotatedException [Annotation ((def :: ScopeData){eventProcessor = const Nothing})] (toException (userError "annotated"))
    (results, recorded) <- Test.withClient \_ ->
      sequence
        [ captureEvent Event.empty,
          captureMessage Level.Warning "message",
          captureException (userError "handled"),
          captureExceptionWith def{levelOverride = Just Level.Fatal} (userError "override"),
          captureUnhandledException "boundary" (userError "unhandled"),
          captureException annotated
        ]
    length (filter (/= Nothing) results) `shouldBe` 5
    last results `shouldBe` Nothing
    events <- Test.fetchAndClearEvents recorded
    fmap (.level) (take 1 (drop 3 events)) `shouldBe` [Just Level.Fatal]

spec_captureOutcomeCallStacks :: Spec
spec_captureOutcomeCallStacks = describe "capture wrapper call stacks" do
  it "keeps SDK wrapper frames out of message and exception captures" do
    (_, recorded) <- Test.withClient \_ -> do
      _ <- captureMessage Level.Info "stack"
      _ <- captureException (userError "stack")
      _ <- captureExceptionWith def (userError "stack")
      captureUnhandledException "boundary" (userError "stack")
    events <- Test.fetchAndClearEvents recorded
    let frames = concatMap (\event -> threadFrames event <> exceptionFrames event) events
    frames `shouldSatisfy` (not . null)
    frames `shouldSatisfy` all (\frame -> Frame.module_ frame /= "Sentry.Capture")
