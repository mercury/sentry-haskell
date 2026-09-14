module AmbientBreadcrumbTest where

import Control.Exception (bracket)
import Data.Default (def)
import Data.Foldable (toList)
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
import Sentry.Init qualified as Init
import Sentry.Scope.IO qualified as Scope.IO
import Sentry.Scope.Operations qualified as Scope
import Sentry.Test qualified as Test
import Sentry.Transport (SomeTransport (..))
import Test.Hspec
import Witch qualified

-- Test-only restoration of the entire context isolates lazy state between cases.
freshContext :: IO a -> IO a
freshContext action = bracket ThreadLocal.getContext (\old -> ThreadLocal.adjustContext (const old)) \_ -> do
  ThreadLocal.adjustContext (Scope.removeIsolation . Scope.removeCurrent)
  Test.withGlobalScope action

crumb :: String -> Breadcrumb.Breadcrumb
crumb msg = Breadcrumb.empty{Breadcrumb.message = Witch.from msg}

messages :: IO [String]
messages = map (Witch.from . (.message)) . toList . (.breadcrumbs) <$> Scope.readAmbientScope

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
      merged <- Scope.readAmbientScope
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

  it "keeps explicit contexts independent and existing non-recording scopes usable" $ freshContext do
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
    messages >>= (`shouldBe` ["ambient", "existing"])
