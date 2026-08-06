module StacktraceTest where

import Control.Exception (SomeException)
import Control.Exception.Annotated qualified as Annotated
import Data.Default (def)
import Data.HashSet qualified as HashSet
import Data.Kind (Type)
import Data.List (find, nub, unsnoc)
import Data.Maybe (isJust, isNothing)
import Patrol.Type.Event qualified as Patrol.Event
import Patrol.Type.Exception qualified as Patrol.Exception
import Patrol.Type.Exceptions qualified as Patrol.Exceptions
import Patrol.Type.Frame qualified as Patrol.Frame
import Patrol.Type.Level qualified as Patrol.Level
import Patrol.Type.Stacktrace qualified as Patrol.Stacktrace
import Patrol.Type.Thread qualified as Patrol.Thread
import Patrol.Type.Threads qualified as Patrol.Threads
import Sentry.Capture (captureException, captureMessage, captureMessage_)
import Sentry.Client qualified as Client
import Sentry.Integration.Stacktrace (ProcessStacktraceIntegration (..))
import Sentry.Internal (ClientOptions (..))
import Sentry.Stacktrace (classifyInApp, packageName)
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

    it "preserves the exception's mechanism through frame-attachment merges" do
      -- 'Sentry.Stacktrace.mergeCallStackIntoException' record-updates only
      -- the 'stacktrace' field of the /last/ exception value; this pins that
      -- it doesn't clobber a 'mechanism' set by 'Sentry.Event.attachMechanism'
      -- on that same entry.
      result <-
        Annotated.try @SomeException $
          Annotated.throw (userError "mechanism survives merge")
      let exc = case result of
            Left e -> e
            Right _ -> error "expected exception"
      (_, transport) <- Test.withClient \_ ->
        captureException exc
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] -> do
          exceptionFrames event `shouldSatisfy` (not . null)
          (event.exception >>= (\e -> Patrol.Exception.mechanism =<< lastMay (Patrol.Exceptions.values e)))
            `shouldSatisfy` isJust
        _ -> expectationFailure $ "expected 1 event, got " <> show (length events)

    it "does not add a Sentry.Capture frame (withFrozenCallStack guard)" do
      -- 'captureException' delegates to an internal helper; without
      -- 'GHC.Stack.withFrozenCallStack' that delegation would push an extra
      -- "Sentry.Capture" frame onto 'captureCallStack'. Same for the three
      -- other public entry points that share the same helper.
      (_, transport) <- Test.withClient \_ ->
        captureException (userError "frozen call stack")
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] -> exceptionFrames event `shouldSatisfy` all (\f -> f.module_ /= "Sentry.Capture")
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

    it "does not add a Sentry.Capture frame via captureMessage_ (withFrozenCallStack guard)" do
      -- Same withFrozenCallStack guard as captureException/captureUnhandledException:
      -- captureMessage_ delegates to captureMessage, and without freezing the
      -- call stack that delegation pushes an extra "Sentry.Capture" frame.
      (_, transport) <- Test.withClient \_ ->
        captureMessage_ Patrol.Level.Info "frozen call stack"
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] -> threadFrames event `shouldSatisfy` all (\f -> f.module_ /= "Sentry.Capture")
        _ -> expectationFailure $ "expected 1 event, got " <> show (length events)

  describe "packageName" do
    it "strips a bare version" do
      packageName "text-2.1.1" `shouldBe` "text"

    it "strips a version and an '-inplace' suffix" do
      packageName "sentry-core-0.0.0-inplace" `shouldBe` "sentry-core"

    it "leaves a hyphenated name with no version untouched" do
      packageName "sentry-core" `shouldBe` "sentry-core"

    it "leaves an empty package untouched" do
      packageName "" `shouldBe` ""

  describe "in_app classification" do
    it "marks at least one frame in_app=True for a plain captureMessage" do
      -- Explicit inAppInclude for this test module, rather than relying on
      -- 'wellKnownNotInApp' leaving it unclassified: this test suite's own
      -- frames are compiled as part of the *sentry-core* package itself
      -- (e.g. package "sentry-core-0.0.0-inplace-unit"), so once the
      -- built-in "sentry-core" denylist entry matches correctly (see
      -- 'packageName'), those frames are legitimately excluded too, and the
      -- any-in-app fallback (which only promotes still-'Nothing' frames)
      -- can't rescue an explicit False. A real application, living in its
      -- own package, wouldn't hit this collision.
      let opts = def{inAppInclude = HashSet.fromList ["StacktraceTest"]}
      (_, transport) <- Test.withCustomClient opts \_ ->
        captureMessage Patrol.Level.Info "classify message"
      events <- Test.fetchAndClearEvents transport
      case events of
        [event] ->
          threadFrames event `shouldSatisfy` any (\f -> f.inApp == Just True)
        _ -> expectationFailure $ "expected 1 event, got " <> show (length events)

    it "marks at least one frame in_app=True for a plain exception" do
      -- See the captureMessage case above for why inAppInclude is explicit here.
      let opts = def{inAppInclude = HashSet.fromList ["StacktraceTest"]}
      (_, transport) <- Test.withCustomClient opts \_ ->
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
        [event] -> do
          -- Match on the bare package name (see 'packageName'): a real build
          -- reports `f.package` as a full unit-id like
          -- "sentry-core-0.0.0-inplace", which an exact `== "sentry-core"`
          -- check would never match, letting this assertion pass vacuously
          -- over an empty list without exercising the denylist at all.
          let denylisted =
                filter
                  (\f -> packageName f.package `elem` ["base", "sentry-core"])
                  (exceptionFrames event)
          -- Guard against the vacuous-pass failure mode above: this must
          -- actually find frames to classify, not just conclude `all p []`.
          denylisted `shouldSatisfy` (not . null)
          denylisted `shouldSatisfy` all (\f -> f.inApp == Just False)
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
      -- Exercises 'classifyInApp' directly with synthetic frames, rather than
      -- through a real captureException call in *this* test suite: every
      -- frame captured from inside sentry-core's own test-suite belongs to
      -- the sentry-core package itself (e.g. "sentry-core-0.0.0-inplace-unit"),
      -- so the built-in "sentry-core" denylist entry now correctly excludes
      -- it too (see 'packageName') — leaving no frame genuinely unclassified
      -- to exercise the fallback against. A real application, in its own
      -- package, wouldn't hit this self-hosting collision.
      let excludedFrame =
            Patrol.Frame.empty
              { Patrol.Frame.module_ = "Sentry.Capture",
                Patrol.Frame.package = "sentry-core-0.0.0-inplace"
              }
          unclassifiedFrame =
            Patrol.Frame.empty
              { Patrol.Frame.module_ = "MyApp.Handler",
                Patrol.Frame.package = "myapp-0.1.0.0-inplace"
              }
          event = eventWithExceptionFrames [excludedFrame, unclassifiedFrame]
          classified = classifyInApp HashSet.empty HashSet.empty event
      -- The denylisted frame stays excluded...
      filter (\f -> f.module_ == "Sentry.Capture") (exceptionFrames classified)
        `shouldSatisfy` all (\f -> f.inApp == Just False)
      -- ...and since no frame was otherwise in_app=True, the fallback
      -- promotes the unclassified frame to True.
      filter (\f -> f.module_ == "MyApp.Handler") (exceptionFrames classified)
        `shouldSatisfy` all (\f -> f.inApp == Just True)

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

-- | The last element of a list, if any.
lastMay :: [a] -> Maybe a
lastMay = fmap snd . unsnoc

-- | Build a minimal 'Patrol.Event.Event' with a single exception value whose
-- stacktrace is exactly the given frames — for exercising 'classifyInApp'
-- directly, without a real capture call.
eventWithExceptionFrames :: [Patrol.Frame.Frame] -> Patrol.Event.Event
eventWithExceptionFrames frames =
  Patrol.Event.empty
    { Patrol.Event.exception =
        Just
          Patrol.Exceptions.empty
            { Patrol.Exceptions.values =
                [ Patrol.Exception.empty
                    { Patrol.Exception.stacktrace =
                        Just Patrol.Stacktrace.empty{Patrol.Stacktrace.frames = frames}
                    }
                ]
            }
    }

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
