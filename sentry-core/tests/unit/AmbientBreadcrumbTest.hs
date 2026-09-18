module AmbientBreadcrumbTest where

import Control.Exception (bracket)
import Data.Default (def)
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Context.ThreadLocal qualified as ThreadLocal
import Patrol.Type.Breadcrumb qualified as Breadcrumb
import Patrol.Type.Breadcrumbs qualified as Breadcrumbs
import Patrol.Type.Event (Event (..))
import Patrol.Type.Level qualified as Level
import Sentry.Capture qualified as Capture
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Client.Options.Dsn qualified as Dsn
import Sentry.Core qualified as Sentry
import Sentry.Init qualified as Init
import Sentry.Scope.IO qualified as Scope.IO
import Sentry.Scope.Operations qualified as Scope
import Sentry.Test qualified as Test
import Sentry.Transport (SomeTransport (..))
import Test.Fixtures (crumb)
import Test.Hspec
import Witch qualified

-- Test-only restoration of the entire context isolates lazy state between cases.
freshContext :: IO a -> IO a
freshContext action = bracket ThreadLocal.getContext (\old -> ThreadLocal.adjustContext (const old)) \_ -> do
  ThreadLocal.adjustContext (Scope.removeIsolation . Scope.removeCurrent)
  Test.withGlobalScope action

messages :: IO [String]
messages = map (Witch.from . (.message)) . toList . (.breadcrumbs) <$> Scope.readMergedScope

spec_ambientBreadcrumbs :: Spec
spec_ambientBreadcrumbs = describe "lazy ambient breadcrumbs" do
  it "captures breadcrumbs after ordinary initialization with defaults, filtering and limits" $ freshContext do
    transport <- Test.new
    let opts =
          def
            { dsn = Dsn.Explicit Test.TEST_DSN,
              transport = Just (Witch.from (SomeTransport transport)),
              maxBreadcrumbs = 2,
              beforeBreadcrumb = Just (\c -> if c.message == "drop" then Nothing else Just c)
            }
    Init.withSentry opts \_ -> do
      Scope.addBreadcrumb (crumb "old")
      Scope.addBreadcrumbs [crumb "one", crumb "drop", crumb "two"]
      Capture.captureMessage_ Level.Info "captured"
    events <- Test.fetchAndClearEvents transport
    case events of
      [event] -> case event.breadcrumbs of
        Just crumbs -> do
          map (.message) crumbs.values `shouldBe` ["one", "two"]
          map (.timestamp) crumbs.values `shouldSatisfy` all (not . isNothing)
          map (.type_) crumbs.values `shouldSatisfy` all (not . isNothing)
        Nothing -> expectationFailure "missing breadcrumbs"
      _ -> expectationFailure "expected one event"

  it "does not establish state without a recording client or for an empty batch" $ freshContext do
    Scope.addBreadcrumb (crumb "uninitialized")
    ctx <- ThreadLocal.getContext
    Scope.lookupIsolation ctx `shouldSatisfy` isNothing
    Init.withSentry def \_ -> Scope.addBreadcrumbs [crumb "disabled"]
    ctx' <- ThreadLocal.getContext
    Scope.lookupIsolation ctx' `shouldSatisfy` isNothing
    transport <- Test.new
    global <- Scope.getGlobal
    Test.mkClient transport >>= \client -> Scope.bindClient (Just client) global
    Scope.addBreadcrumbs []
    ctx'' <- ThreadLocal.getContext
    Scope.lookupIsolation ctx'' `shouldSatisfy` isNothing

  it "preserves current and unrelated keys, persists across current brackets, and isolates siblings" $ freshContext do
    transport <- Test.new
    global <- Scope.getGlobal
    Test.mkClient transport >>= \client -> Scope.bindClient (Just client) global
    key <- Context.newKey "ambient-unrelated"
    ThreadLocal.adjustContext (Context.insert key True)
    Scope.IO.withScope \current -> do
      Scope.setTag current "current" "retained"
      Scope.addBreadcrumb (crumb "parent")
      ctx <- ThreadLocal.getContext
      Context.lookup key ctx `shouldBe` Just True
      currentData <- Scope.readScopeRef current
      merged <- Scope.readMergedScope
      merged.tags `shouldBe` currentData.tags
    messages >>= (`shouldBe` ["parent"])
    Scope.IO.withIsolationScope \_ -> do
      Scope.addBreadcrumb (crumb "request-one")
      messages >>= (`shouldBe` ["parent", "request-one"])
    Scope.IO.withIsolationScope \_ -> messages >>= (`shouldBe` ["parent"])
    messages >>= (`shouldBe` ["parent"])
    -- Lazy scopes must not pin the client that caused their creation.
    Scope.bindClient Nothing global
    resolved <- Scope.lookupClient
    isNothing resolved `shouldBe` True

  it "keeps explicit contexts independent and skips disabled ambient writes" $ freshContext do
    explicitContext <- ThreadLocal.getContext
    transport <- Test.new
    global <- Scope.getGlobal
    Test.mkClient transport >>= \client -> Scope.bindClient (Just client) global
    Scope.addBreadcrumbAt explicitContext (crumb "no-scope")
    current <- ThreadLocal.getContext
    Scope.lookupIsolation current `shouldSatisfy` isNothing
    Scope.addBreadcrumb (crumb "ambient")
    Scope.addBreadcrumbAt explicitContext (crumb "still-no-scope")
    messages >>= (`shouldBe` ["ambient"])
    Scope.bindClient Nothing global
    Scope.addBreadcrumb (crumb "existing")
    messages >>= (`shouldBe` ["ambient"])

spec_ambientMetadata :: Spec
spec_ambientMetadata = describe "ambient metadata targeting" do
  it "skips invalid arguments and creates no scopes while disabled" $ freshContext do
    Sentry.setUser (error "user" :: Sentry.UserUpdate)
    Sentry.setTag (error "key") (error "value")
    Sentry.setTransaction (error "transaction")
    Sentry.addBreadcrumb (error "crumb" :: Sentry.BreadcrumbUpdate)
    Sentry.addBreadcrumbs (error "crumbs")
    Sentry.clearBreadcrumbs
    _ <- Sentry.readMergedScope
    ctx <- ThreadLocal.getContext
    isNothing (Scope.lookupIsolation ctx) `shouldBe` True
    isNothing (Scope.lookupCurrent ctx) `shouldBe` True

  it "getters preserve unrelated keys and disabled writes preserve explicit metadata" $ freshContext do
    key <- Context.newKey "unrelated"
    ThreadLocal.adjustContext (Context.insert key (42 :: Int))
    isolation <- Sentry.getIsolationScope
    current <- Sentry.getCurrentScope
    Scope.setTag isolation "explicit" "kept"
    Scope.setTransaction current "kept"
    Sentry.clearTags
    Sentry.unsetTransaction
    Sentry.modifyUser (error "update" :: Sentry.UserUpdate)
    Sentry.setOptionalUser (error "optional user")
    again <- Sentry.getIsolationScope
    result <- Sentry.readScopeRef again
    result.tags `shouldBe` Map.singleton "explicit" "kept"
    result.user `shouldBe` Nothing
    local <- Sentry.readScopeRef current
    local.transaction `shouldBe` Just "kept"
    ctx <- ThreadLocal.getContext
    Context.lookup key ctx `shouldBe` Just 42

  it "retains isolation writes across current exits and restores nested isolation" $ freshContext do
    transport <- Test.new
    client <- Test.mkClient transport
    global <- Scope.getGlobal
    Scope.bindClient (Just client) global
    Sentry.withScope \_ -> do
      Sentry.setTag "request" "outer"
      Sentry.setTransaction "local"
      current <- Sentry.getCurrentScope >>= Sentry.readScopeRef
      current.transaction `shouldBe` Just "local"
      current.tags `shouldBe` mempty
    outer <- Sentry.readMergedScope
    outer.tags `shouldBe` Map.singleton "request" "outer"
    outer.transaction `shouldBe` Nothing
    Sentry.withIsolationScope \_ -> do
      Sentry.setTag "request" "inner"
      Sentry.withIsolationScope \_ -> Sentry.setTag "request" "nested"
      inner <- Sentry.readMergedScope
      inner.tags `shouldBe` Map.singleton "request" "inner"
    restored <- Sentry.readMergedScope
    restored.tags `shouldBe` outer.tags
    current <- Sentry.getCurrentScope
    Scope.bindClient (Just Sentry.NON_RECORDING_CLIENT) current
    Sentry.setTag "request" "disabled"
    Sentry.addBreadcrumbs (error "shadowed")
    final <- Sentry.readMergedScope
    final.tags `shouldBe` outer.tags

  it "skips breadcrumb hooks on a non-recording client with an existing scope" $ freshContext do
    let opts = def{dsn = Dsn.Disabled, beforeBreadcrumb = Just (error "disabled hook")}
    Init.withSentry opts \_ -> do
      isolation <- Sentry.getIsolationScope
      Sentry.addBreadcrumb (crumb "ignored")
      Sentry.addBreadcrumbs [crumb "ignored"]
      result <- Sentry.readScopeRef isolation
      result.breadcrumbs `shouldBe` mempty
