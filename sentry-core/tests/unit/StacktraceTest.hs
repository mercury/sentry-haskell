module StacktraceTest where

import Control.Exception (SomeException)
import Control.Exception.Annotated qualified as Annotated
import Data.Default (def)
import Data.HashSet qualified as HashSet
import Data.Kind (Type)
import Data.List (find, nub)
import Data.Maybe (isJust, isNothing)
import Patrol.Type.Event qualified as Patrol.Event
import Patrol.Type.Exception qualified as Patrol.Exception
import Patrol.Type.Exceptions qualified as Patrol.Exceptions
import Patrol.Type.Frame qualified as Patrol.Frame
import Patrol.Type.Level qualified as Patrol.Level
import Patrol.Type.Stacktrace qualified as Patrol.Stacktrace
import Patrol.Type.Thread qualified as Patrol.Thread
import Patrol.Type.Threads qualified as Patrol.Threads
import Sentry.Capture (captureException, captureMessage)
import Sentry.Client qualified as Client
import Sentry.Integration.Stacktrace (ProcessStacktraceIntegration (..))
import Sentry.Internal (ClientOptions (..))
import Sentry.Test qualified as Test
import Test.Hspec

spec_stacktrace :: Spec
spec_stacktrace = describe "stacktrace integrations" do
  describe "captureException" do
    it "attaches frames from the HasCallStack floor for a plain exception" do
      (_, transport) <- Test.withClient \_ ->
        captureException (userError "plain exception")
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] -> exceptionFrames event `shouldSatisfy` (not . null)
        _ -> expectationFailure $ "expected 1 event, got " <> show (length events)

    it "attaches frames from annotated-exception CallStack annotations" do
      -- Annotated.throw adds a CallStack annotation automatically.
      result <-
        Annotated.try @SomeException $
          Annotated.throw (userError "annotated exception")
      let exc = case result of
            Left e -> e
            Right _ -> error "expected exception"
      (_, transport) <- Test.withClient \_ ->
        captureException exc
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] -> exceptionFrames event `shouldSatisfy` (not . null)
        _ -> expectationFailure $ "expected 1 event, got " <> show (length events)

    it "produces no duplicate frames (deduplication)" do
      -- Both AttachAnnotatedException and AttachCallStack sources may
      -- contribute overlapping frames; dedup should collapse them.
      result <-
        Annotated.try @SomeException $
          Annotated.throw (userError "dedup exception")
      let exc = case result of
            Left e -> e
            Right _ -> error "expected exception"
      (_, transport) <- Test.withClient \_ ->
        captureException exc
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] ->
          let frames = exceptionFrames event
              keys = map frameKey frames
           in keys `shouldBe` nub keys
        _ -> expectationFailure $ "expected 1 event, got " <> show (length events)

  describe "captureMessage" do
    it "attaches frames from the HasCallStack floor to a thread entry" do
      (_, transport) <- Test.withClient \_ ->
        captureMessage Patrol.Level.Info "test message"
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] -> do
          threadFrames event `shouldSatisfy` (not . null)
          -- Should have a 'current = True' thread entry.
          let mCurrentThread =
                find (\t -> t.current == Just True)
                  . maybe [] Patrol.Threads.values
                  $ event.threads
          mCurrentThread `shouldSatisfy` isJust
        _ -> expectationFailure $ "expected 1 event, got " <> show (length events)

  describe "in_app classification" do
    it "marks at least one frame in_app=True for a plain captureMessage" do
      (_, transport) <- Test.withClient \_ ->
        captureMessage Patrol.Level.Info "classify message"
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] ->
          threadFrames event `shouldSatisfy` any (\f -> f.inApp == Just True)
        _ -> expectationFailure $ "expected 1 event, got " <> show (length events)

    it "marks at least one frame in_app=True for a plain exception" do
      (_, transport) <- Test.withClient \_ ->
        captureException (userError "classify")
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] ->
          exceptionFrames event `shouldSatisfy` any (\f -> f.inApp == Just True)
        _ -> expectationFailure $ "expected 1 event, got " <> show (length events)

    it "marks wellKnownNotInApp frames (base, sentry-core) as in_app=False" do
      (_, transport) <- Test.withClient \_ ->
        captureException (userError "denylist test")
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] ->
          -- Frames from `base` or `sentry-core` must never be in-app.
          filter (\f -> f.package == "base" || f.package == "sentry-core") (exceptionFrames event)
            `shouldSatisfy` all (\f -> f.inApp == Just False)
        _ -> expectationFailure $ "expected 1 event, got " <> show (length events)

    it "excludes frames whose module_ matches inAppExclude" do
      let opts = def{inAppExclude = HashSet.fromList ["StacktraceTest"]}
      (_, transport) <- Test.withCustomClient opts \_ ->
        captureException (userError "exclude test")
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] ->
          filter (\f -> f.module_ == "StacktraceTest") (exceptionFrames event)
            `shouldSatisfy` all (\f -> f.inApp == Just False)
        _ -> expectationFailure $ "expected 1 event, got " <> show (length events)

    it "inAppInclude takes precedence over inAppExclude for the same prefix" do
      let opts =
            def
              { inAppInclude = HashSet.fromList ["StacktraceTest"],
                inAppExclude = HashSet.fromList ["StacktraceTest"]
              }
      (_, transport) <- Test.withCustomClient opts \_ ->
        captureException (userError "precedence test")
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] ->
          filter (\f -> f.module_ == "StacktraceTest") (exceptionFrames event)
            `shouldSatisfy` all (\f -> f.inApp == Just True)
        _ -> expectationFailure $ "expected 1 event, got " <> show (length events)

    it "falls back to marking unclassified frames in_app=True when no frame would otherwise be in-app" do
      -- Exclude only SDK frames so they become inApp=False.  The test module's
      -- own frames have no matching rule and remain inApp=Nothing.  Since no
      -- frame is inApp=True after classification, the any_in_app fallback
      -- promotes those Nothing frames to True.
      let opts = def{inAppExclude = HashSet.fromList ["Sentry"]}
      (_, transport) <- Test.withCustomClient opts \_ ->
        captureException (userError "fallback test")
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] ->
          -- The fallback must have fired: at least one frame is in_app.
          exceptionFrames event `shouldSatisfy` any (\f -> f.inApp == Just True)
        _ -> expectationFailure $ "expected 1 event, got " <> show (length events)

  describe "disableIntegration" do
    it "removing ProcessStacktraceIntegration leaves frames unclassified" do
      let opts = Client.disableIntegration (type ProcessStacktraceIntegration) def
      (_, transport) <- Test.withCustomClient opts \_ ->
        captureException (userError "no classify")
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] ->
          -- Frames should exist (attach integrations still run) but none
          -- should have inApp set since ProcessStacktrace was removed.
          exceptionFrames event `shouldSatisfy` all (\f -> f.inApp == Nothing)
        _ -> expectationFailure $ "expected 1 event, got " <> show (length events)

    it "disableIntegration removes exactly that integration from the client" do
      let optsWithout = Client.disableIntegration (type ProcessStacktraceIntegration) def
      clientWithout <- Client.new optsWithout{dsn = Just Test.TEST_DSN}
      clientWith <- Client.new def{dsn = Just Test.TEST_DSN}
      Client.getIntegration (type ProcessStacktraceIntegration) clientWithout
        `shouldSatisfy` isNothing
      Client.getIntegration (type ProcessStacktraceIntegration) clientWith
        `shouldSatisfy` isJust

-- Helpers

-- | Flatten all frames from all exception values in an event.
exceptionFrames :: Patrol.Event.Event -> [Patrol.Frame.Frame]
exceptionFrames event =
  foldMap stackFrames (foldMap Patrol.Exceptions.values event.exception)
  where
    stackFrames e = foldMap Patrol.Stacktrace.frames e.stacktrace

-- | Flatten all frames from all thread entries in an event.
threadFrames :: Patrol.Event.Event -> [Patrol.Frame.Frame]
threadFrames event =
  foldMap stackFrames (foldMap Patrol.Threads.values event.threads)
  where
    stackFrames t = foldMap Patrol.Stacktrace.frames t.stacktrace

-- | Deduplication key matching 'Sentry.Stacktrace.dedupeFrames'.
type FrameKey :: Type
type FrameKey = (String, String, String, Maybe Int, Maybe Int)

frameKey :: Patrol.Frame.Frame -> FrameKey
frameKey f =
  ( show f.package,
    show f.module_,
    show f.function,
    f.lineno,
    f.colno
  )
