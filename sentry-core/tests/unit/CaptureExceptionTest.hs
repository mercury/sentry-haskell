module CaptureExceptionTest where

import Control.Exception (AsyncException (ThreadKilled), SomeException, toException)
import Control.Exception.Annotated (AnnotatedException (..), Annotation (..))
import Control.Exception.Safe qualified as Safe
import Data.Default (def)
import Patrol.Type.Event qualified as Patrol.Event
import Patrol.Type.Level qualified as Patrol.Level
import Sentry.Capture (captureException)
import Sentry.Client (Client)
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Scope (ScopeData (..))
import Sentry.Scope qualified as Scope
import Sentry.Scope.IO qualified as Scope.IO
import Sentry.Test qualified as Test
import Sentry.Transport (SomeTransport (..))
import Test.Hspec
import Witch qualified

mkClient :: Test.TestTransport -> Client
mkClient transport =
  Witch.from @ClientOptions @Client
    (def @ClientOptions)
      { dsn = Just Test.TEST_DSN,
        transport = Just (SomeTransport transport)
      }

spec_captureException :: Spec
spec_captureException = describe "captureException" do
  it "captures a plain exception with no annotation as an event" do
    transport <- Test.new
    let client = mkClient transport
    _ <- captureException client (userError "boom")
    events <- Test.fetchAndClearEvents transport
    length events `shouldBe` 1

  it "uses ScopeData from AnnotatedException when present (authoritative)" do
    transport <- Test.new
    let client = mkClient transport
        scopeData = (def @ScopeData){level = Just Patrol.Level.Warning}
        annotated =
          AnnotatedException
            [Annotation scopeData]
            (toException $ userError "boom")
    _ <- captureException client annotated
    events <- Test.fetchAndClearEvents transport
    case events of
      [event] -> event.level `shouldBe` Just Patrol.Level.Warning
      _ -> expectationFailure $ "expected one event, got " <> show (length events)

  it "drops the event when scope's eventProcessor returns Nothing" do
    transport <- Test.new
    let client = mkClient transport
        scopeData = (def @ScopeData){eventProcessor = const Nothing}
        annotated =
          AnnotatedException
            [Annotation scopeData]
            (toException $ userError "boom")
    result <- captureException client annotated
    result `shouldBe` Nothing
    events <- Test.fetchAndClearEvents transport
    events `shouldSatisfy` null

  it "uses ambient thread-local scope when no annotation is present" do
    transport <- Test.new
    let client = mkClient transport
    _ <- Scope.IO.withScope \scope -> do
      Scope.setLevel scope Patrol.Level.Warning
      captureException client (userError "boom")
    events <- Test.fetchAndClearEvents transport
    case events of
      [event] -> event.level `shouldBe` Just Patrol.Level.Warning
      _ -> expectationFailure $ "expected one event, got " <> show (length events)

  it "preserves throw-site scope when an exception escapes withScope" do
    transport <- Test.new
    let client = mkClient transport
    result <- Safe.try @_ @(AnnotatedException SomeException) $ Scope.IO.withScope \scope -> do
      Scope.setLevel scope Patrol.Level.Warning
      Safe.throwIO $ userError "escapes scope"
    case result of
      Right _ -> expectationFailure "expected an exception to be thrown"
      Left annotated -> do
        _ <- captureException client annotated
        events <- Test.fetchAndClearEvents transport
        case events of
          [event] -> event.level `shouldBe` Just Patrol.Level.Warning
          _ -> expectationFailure $ "expected one event, got " <> show (length events)

  it "captures an async exception value as an event" do
    transport <- Test.new
    let client = mkClient transport
    _ <- captureException client ThreadKilled
    events <- Test.fetchAndClearEvents transport
    length events `shouldBe` 1
