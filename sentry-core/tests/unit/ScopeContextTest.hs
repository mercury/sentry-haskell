module ScopeContextTest where

import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Foldable (toList, traverse_)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import OpenTelemetry.Context (Context)
import OpenTelemetry.Context qualified as Context
import Patrol qualified
import Patrol.Type.Breadcrumb qualified as Patrol.Breadcrumb
import Sentry.Client (Client (..), pattern NON_RECORDING_CLIENT)
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Scope (Scope, ScopeData (..))
import Sentry.Scope qualified as Scope
import Sentry.Scope.IO qualified as Scope.IO
import Sentry.Scope.Internal qualified as Internal
import Sentry.Test qualified as Test
import Test.Hspec

-- | The three scope handles installed on a hand-built 'Context', plus the
-- 'Context' itself. Every test starts from a fresh 'Context.empty' and
-- layers exactly the scopes it needs via this helper's variants below, so
-- tests never share state and never fall through to the real
-- 'Internal.processGlobal' singleton (see 'withFreshGlobal').
type Layers :: Type
data Layers = Layers
  { context :: Context,
    global :: Scope,
    isolation :: Maybe Scope,
    current :: Maybe Scope
  }

-- | Insert a freshly created, empty 'Scope.Global' scope into the given
-- 'Context'. Every helper below goes through this rather than leaving the
-- global layer absent, because 'Sentry.Scope.readScopeAt' (like
-- 'Sentry.Scope.readAmbientScope') falls back to the real process-wide
-- 'Internal.processGlobal' singleton when no override is present on the
-- 'Context' — a hand-built 'Context.empty' would otherwise silently read
-- shared, cross-test state.
withFreshGlobal :: IO (Context, Scope)
withFreshGlobal = do
  g <- Scope.create Scope.Global
  pure (Internal.insertGlobal g Context.empty, g)

-- | A 'Context' with only a fresh global scope installed.
globalOnly :: IO Layers
globalOnly = do
  (ctx, g) <- withFreshGlobal
  pure Layers{context = ctx, global = g, isolation = Nothing, current = Nothing}

-- | A 'Context' with a fresh global and isolation scope, no current scope.
isolationOnly :: IO Layers
isolationOnly = do
  (ctx0, g) <- withFreshGlobal
  i <- Scope.create Scope.Isolation
  pure
    Layers
      { context = Scope.insertIsolation i ctx0,
        global = g,
        isolation = Just i,
        current = Nothing
      }

-- | A 'Context' with a fresh global and current scope, no isolation scope.
currentOnly :: IO Layers
currentOnly = do
  (ctx0, g) <- withFreshGlobal
  c <- Scope.create Scope.Current
  pure
    Layers
      { context = Scope.insertCurrent c ctx0,
        global = g,
        isolation = Nothing,
        current = Just c
      }

-- | A 'Context' with fresh global, isolation, and current scopes all
-- installed — the common case once 'Sentry.Scope.IO.withIsolationScope' has
-- run, since it forks both layers together.
allLayers :: IO Layers
allLayers = do
  (ctx0, g) <- withFreshGlobal
  i <- Scope.create Scope.Isolation
  c <- Scope.create Scope.Current
  pure
    Layers
      { context = Scope.insertCurrent c (Scope.insertIsolation i ctx0),
        global = g,
        isolation = Just i,
        current = Just c
      }

-- | Minimal breadcrumb with a distinguishable message.
crumb :: Text -> Patrol.Breadcrumb
crumb msg = Patrol.Breadcrumb.empty{Patrol.Breadcrumb.message = msg}

-- | 'Scope.resolveClientAt' monomorphized to 'IO', so call sites don't need
-- their own type annotation to pin down the ambiguous 'MonadIO' instance.
resolveClientAtIO :: Context -> IO Client
resolveClientAtIO = Scope.resolveClientAt

spec_readScopeAt :: Spec
spec_readScopeAt = describe "readScopeAt" do
  it "merges global, isolation, and current layers" do
    layers <- allLayers
    Scope.setTag layers.global "layer" "global"
    traverse_ (\s -> Scope.setTag s "layer-i" "isolation") layers.isolation
    traverse_ (\s -> Scope.setTag s "layer-c" "current") layers.current
    merged <- Scope.readScopeAt layers.context
    merged.tags
      `shouldBe` Map.fromList
        [ ("layer", "global"),
          ("layer-i", "isolation"),
          ("layer-c", "current")
        ]

  it "treats an absent isolation and current layer as mempty" do
    layers <- globalOnly
    Scope.setTag layers.global "only" "global"
    merged <- Scope.readScopeAt layers.context
    merged.tags `shouldBe` Map.fromList [("only", "global")]
    merged.breadcrumbs `shouldBe` mempty

spec_addBreadcrumbAt :: Spec
spec_addBreadcrumbAt = describe "addBreadcrumbAt" do
  it "lands on isolation even when a current scope is present" do
    layers <- allLayers
    Scope.addBreadcrumbAt layers.context (crumb "isolation-bound")
    isolationData <- maybe (fail "expected an isolation scope") Scope.readScopeRef layers.isolation
    currentData <- maybe (fail "expected a current scope") Scope.readScopeRef layers.current
    map (.message) (toList isolationData.breadcrumbs) `shouldBe` ["isolation-bound"]
    currentData.breadcrumbs `shouldBe` mempty

  it "no-ops when no isolation scope is active, even with a current scope present" do
    layers <- currentOnly
    Scope.addBreadcrumbAt layers.context (crumb "dropped")
    currentData <- maybe (fail "expected a current scope") Scope.readScopeRef layers.current
    currentData.breadcrumbs `shouldBe` mempty

  it "no-ops when neither isolation nor current scope is active" do
    layers <- globalOnly
    -- Just asserting this doesn't throw; there is no scope left to inspect.
    Scope.addBreadcrumbAt layers.context (crumb "nowhere")
    globalData <- Scope.readScopeRef layers.global
    globalData.breadcrumbs `shouldBe` mempty

  it "respects maxBreadcrumbs" do
    layers <- isolationOnly
    isolation <- maybe (fail "expected an isolation scope") pure layers.isolation
    transport <- Test.new
    let client = Test.mkCustomClient transport def{maxBreadcrumbs = 3}
    Scope.bindClient (Just client) isolation
    traverse_
      (Scope.addBreadcrumbAt layers.context . crumb)
      ["1", "2", "3", "4", "5"]
    isolationData <- Scope.readScopeRef isolation
    map (.message) (toList isolationData.breadcrumbs) `shouldBe` ["3", "4", "5"]

  it "respects beforeBreadcrumb" do
    layers <- isolationOnly
    isolation <- maybe (fail "expected an isolation scope") pure layers.isolation
    transport <- Test.new
    let client = Test.mkCustomClient transport def{beforeBreadcrumb = Just (const Nothing)}
    Scope.bindClient (Just client) isolation
    Scope.addBreadcrumbAt layers.context (crumb "dropped")
    isolationData <- Scope.readScopeRef isolation
    isolationData.breadcrumbs `shouldBe` mempty

spec_setTagAt :: Spec
spec_setTagAt = describe "setTagAt" do
  it "lands on current when both current and isolation are present" do
    layers <- allLayers
    Scope.setTagAt layers.context "env" "prod"
    currentData <- maybe (fail "expected a current scope") Scope.readScopeRef layers.current
    isolationData <- maybe (fail "expected an isolation scope") Scope.readScopeRef layers.isolation
    currentData.tags `shouldBe` Map.fromList [("env", "prod")]
    isolationData.tags `shouldBe` mempty

  it "falls back to isolation when only isolation is present" do
    layers <- isolationOnly
    Scope.setTagAt layers.context "env" "staging"
    isolationData <- maybe (fail "expected an isolation scope") Scope.readScopeRef layers.isolation
    isolationData.tags `shouldBe` Map.fromList [("env", "staging")]

  it "no-ops when neither current nor isolation is present" do
    layers <- globalOnly
    Scope.setTagAt layers.context "env" "nowhere"
    globalData <- Scope.readScopeRef layers.global
    globalData.tags `shouldBe` mempty

spec_resolveClientAt :: Spec
spec_resolveClientAt = describe "resolveClientAt" do
  it "honors a client bound on the isolation scope" do
    layers <- isolationOnly
    isolation <- maybe (fail "expected an isolation scope") pure layers.isolation
    transport <- Test.new
    let client = Test.mkClient transport
    Scope.bindClient (Just client) isolation
    resolved <- resolveClientAtIO layers.context
    resolved.options.dsn `shouldBe` Just Test.TEST_DSN

  it "falls back to NON_RECORDING_CLIENT when no layer has a client bound" do
    layers <- allLayers
    resolved <- resolveClientAtIO layers.context
    case resolved of
      NON_RECORDING_CLIENT -> pure ()
      _ -> expectationFailure "expected NON_RECORDING_CLIENT"

-- | The strongest guard for this refactor: existing ambient-API behavior must
-- be untouched. If this file needs edits after wiring @Scope.hs@'s ambient
-- functions through the @*At@ family, the refactor was not behavior-preserving.
spec_ambientUnaffected :: Spec
spec_ambientUnaffected = describe "ambient addBreadcrumb (regression guard)" do
  it "still lands on the ambient isolation scope" do
    (scopeData, _) <- Test.withClient \_ ->
      Scope.IO.withIsolationScope \scope -> do
        Scope.addBreadcrumb (crumb "ambient")
        liftIO $ Scope.readScopeRef scope
    map (.message) (toList scopeData.breadcrumbs) `shouldBe` ["ambient"]
