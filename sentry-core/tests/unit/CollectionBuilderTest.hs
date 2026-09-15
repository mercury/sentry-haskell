module CollectionBuilderTest where

import Control.Exception (evaluate)
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Patrol.Type.Context qualified as Context
import Patrol.Type.Stacktrace qualified as Stacktrace
import Sentry.Breadcrumb qualified as B
import Sentry.BrowserContext qualified as Browser
import Sentry.Event qualified as E
import Sentry.Exception qualified as X
import Sentry.Mechanism qualified as M
import Sentry.Scope qualified as Scope
import Sentry.Scope.Internal (ScopeData (..))
import Sentry.Scope.Update qualified as S
import Sentry.Update qualified as U
import Test.Hspec

spec_collections :: Spec
spec_collections = describe "collection builders" do
  it "constructs exceptions from lists and edits fields while preserving metadata" do
    let original = U.run [X.setType "old", X.setValue "message", X.setModule "module", X.setThreadId "thread", X.setMechanism M.generic, X.setStacktrace Stacktrace.empty] X.empty
        result = U.run (X.lastException [X.setType "new", X.setValue "safe"]) (X.singleton original)
    map (.type_) result.values `shouldBe` ["new"]
    map (.value) result.values `shouldBe` ["safe"]
    map (.module_) result.values `shouldBe` ["module"]
    map (.threadId) result.values `shouldBe` ["thread"]
    map (.mechanism) result.values `shouldBe` [Just M.generic]
    map (.stacktrace) result.values `shouldBe` [Just Stacktrace.empty]
    (U.run X.unsetStacktrace original).stacktrace `shouldBe` Nothing
    (U.run (X.lastException X.empty) result).values `shouldBe` [X.empty]
  it "forces assigned fields and nested records without forcing leaves" do
    mapM_ (\builder -> evaluate (U.run (builder (error "field")) X.empty) `shouldThrow` anyErrorCall) [X.setType, X.setValue, X.setModule, X.setThreadId]
    evaluate (U.run (X.setStacktrace (error "stack")) X.empty) `shouldThrow` anyErrorCall
    evaluate (U.run (X.setMechanism (error "mechanism" :: M.Mechanism)) X.empty) `shouldThrow` anyErrorCall
    evaluate (length (X.singleton X.empty{X.value = error "lazy leaf"}).values) `shouldReturn` 1
  it "creates, edits, replaces and removes nested mechanisms" do
    let first = U.run (X.modifyMechanism (M.setType "custom")) X.empty
        second = U.run (X.modifyExistingMechanism [M.setHandled False]) first
    fmap (.type_) second.mechanism `shouldBe` Just "custom"
    fmap (.handled) second.mechanism `shouldBe` Just (Just False)
    (U.run (X.setMechanism M.generic) second).mechanism `shouldBe` Just M.generic
    (U.run X.unsetMechanism second).mechanism `shouldBe` Nothing
    U.run (X.modifyExistingMechanism (error "unused" :: M.MechanismUpdate)) X.empty `shouldBe` X.empty
  it "orders insertions and selects existing exceptions independently" do
    let chain = U.run [X.appendException (X.setValue "b"), X.prependException (X.setValue "a"), X.appendException (X.setValue "c")] X.emptyChain
        edited = U.run [X.firstException (X.setType "first"), X.lastException (X.setType "last"), X.eachException (X.with (\x -> X.setValue (x.value <> "!")))] chain
    map (.value) edited.values `shouldBe` ["a!", "b!", "c!"]
    map (.type_) edited.values `shouldBe` ["first", "", "last"]
    U.run (X.setValues [X.setValue "a", X.setValue "b"]) X.emptyChain `shouldBe` X.Exceptions [U.run (X.setValue "a") X.empty, U.run (X.setValue "b") X.empty]
  it "skips all empty selections" do
    mapM_ (\select -> U.run (select (error "unused" :: X.ExceptionUpdate)) X.emptyChain `shouldBe` X.emptyChain) [X.firstException, X.lastException, X.eachException]
    mapM_ (\select -> U.run (select (error "unused" :: B.BreadcrumbUpdate)) B.emptyCollection `shouldBe` B.emptyCollection) [B.firstBreadcrumb, B.lastBreadcrumb, B.eachBreadcrumb]
  it "forces inserted children and later selected results during application" do
    evaluate (U.run (X.setValues [X.empty, error "child"]) X.emptyChain) `shouldThrow` anyErrorCall
    evaluate (U.run (X.appendException (error "child" :: X.Exception)) X.emptyChain) `shouldThrow` anyErrorCall
    let chain = X.Exceptions [X.empty, U.run (X.setValue "bad") X.empty]
        invalid = X.with (\x -> if x.value == "bad" then error "later" else mempty :: X.ExceptionUpdate)
    evaluate (U.run (X.eachException invalid) chain) `shouldThrow` anyErrorCall
    evaluate (U.run (X.lastException invalid) chain) `shouldThrow` anyErrorCall
    evaluate (length (U.run (X.firstException (X.setType "ok")) (X.Exceptions [X.empty, error "unselected"])).values) `shouldReturn` 2
  it "distinguishes absent and present-empty Event payloads" do
    (E.apply E.empty (E.modifyExceptionChain X.clearValues)).exception `shouldBe` Just X.emptyChain
    (E.apply E.empty (E.modifyExistingExceptionChain X.clearValues)).exception `shouldBe` Nothing
    (E.apply E.empty (E.modifyExistingExceptionChain (error "unused" :: X.ExceptionsUpdate))).exception `shouldBe` Nothing
    let event = E.apply E.empty (E.setExceptionChain [X.appendException (X.setValue "one")])
    (E.apply event E.unsetExceptionChain).exception `shouldBe` Nothing
    (E.apply E.empty (E.setExceptionChain X.emptyChain)).exception `shouldBe` Just X.emptyChain
    (E.apply E.empty (E.modifyBreadcrumbs B.clearValues)).breadcrumbs `shouldBe` Just B.emptyCollection
    (E.apply E.empty (E.modifyExistingBreadcrumbs (error "unused" :: B.BreadcrumbsUpdate))).breadcrumbs `shouldBe` Nothing
    (E.apply (E.apply E.empty (E.setBreadcrumbs B.emptyCollection)) E.unsetBreadcrumbs).breadcrumbs `shouldBe` Nothing
  it "provides breadcrumb selection parity for wrappers, Event, and local Scope" do
    let collection = U.run [B.appendBreadcrumb (B.setMessage "b"), B.prependBreadcrumb (B.setMessage "a"), B.appendBreadcrumb (B.setMessage "c")] B.emptyCollection
        edits = [B.firstBreadcrumb (B.setCategory "first"), B.lastBreadcrumb (B.setCategory "last"), B.eachBreadcrumb (B.with (\b -> B.setMessage (b.message <> "!")))]
        expected = U.run edits collection
        scope = U.run (S.setBreadcrumbs collection <> S.modifyBreadcrumbs edits) (mempty :: ScopeData)
        event = E.apply E.empty [E.setBreadcrumbs collection, E.modifyExistingBreadcrumbs edits]
    map (.message) expected.values `shouldBe` ["a!", "b!", "c!"]
    map (.category) expected.values `shouldBe` ["first", "", "last"]
    toList scope.breadcrumbs `shouldBe` expected.values
    event.breadcrumbs `shouldBe` Just expected
    toList (U.run (S.eachBreadcrumb (error "unused" :: B.BreadcrumbUpdate)) (mempty :: ScopeData)).breadcrumbs `shouldBe` []
    let parent = U.run (S.appendBreadcrumb (B.setMessage "parent")) (mempty :: ScopeData)
        local = U.run (S.prependBreadcrumb (B.setMessage "local") <> S.firstBreadcrumb (B.setCategory "first") <> S.lastBreadcrumb (B.setCategory "last")) (mempty :: ScopeData)
    map (.message) (toList (parent <> local).breadcrumbs) `shouldBe` ["parent", "local"]
    map (.category) (toList parent.breadcrumbs) `shouldBe` [""]
  it "forces later breadcrumb edits and insertions" do
    evaluate (U.run (B.setValues [B.empty, error "later"]) B.emptyCollection) `shouldThrow` anyErrorCall
    let collection = B.Breadcrumbs [B.empty, U.run (B.setMessage "bad") B.empty]
        invalid = B.with (\b -> if b.message == "bad" then error "later" else mempty :: B.BreadcrumbUpdate)
    evaluate (E.apply E.empty (E.setBreadcrumbs (B.eachBreadcrumb invalid <> B.clearValues))) `shouldReturn` E.apply E.empty (E.setBreadcrumbs B.emptyCollection)
    evaluate (U.run (B.eachBreadcrumb invalid) collection) `shouldThrow` anyErrorCall
    evaluate (U.run (S.setBreadcrumbs collection <> S.eachBreadcrumb invalid) (mempty :: ScopeData)) `shouldThrow` anyErrorCall

spec_insertIfAbsent :: Spec
spec_insertIfAbsent = describe "insert-if-absent builders" do
  it "inserts missing tags and keeps the first default; setters still overwrite" do
    let event = E.apply E.empty [E.setTagIfAbsent "key" "first", E.setTagIfAbsent "key" "second", E.setTagIfAbsent "other" "value"]
        scope = U.run [Scope.setTagIfAbsent "key" "first", Scope.setTagIfAbsent "key" "second", Scope.setTagIfAbsent "other" "value"] (U.empty :: ScopeData)
    event.tags `shouldBe` Map.fromList [("key", "first"), ("other", "value")]
    scope.tags `shouldBe` event.tags
    (E.apply event (E.setTag "key" "last")).tags Map.! "key" `shouldBe` "last"
    (U.run (Scope.setTag "key" "last") scope).tags Map.! "key" `shouldBe` "last"
  it "preserves empty tags and contexts without evaluating unused values" do
    let payload = Context.Other Map.empty
        event = E.apply E.empty [E.setTag "key" "", E.setContext "custom" payload]
        scope = U.run [Scope.setTag "key" "", Scope.setContext "custom" payload] (U.empty :: ScopeData)
        event' = E.apply event [E.setTagIfAbsent "key" (error "unused"), E.setContextIfAbsent "custom" (error "unused")]
        scope' = U.run [Scope.setTagIfAbsent "key" (error "unused"), Scope.setContextIfAbsent "custom" (error "unused")] scope
    event'.tags `shouldBe` event.tags
    event'.contexts `shouldBe` event.contexts
    scope'.tags `shouldBe` scope.tags
    scope'.contexts `shouldBe` scope.contexts
  it "inserts contexts, preserves typed variants, and composes with removal" do
    let typed = Context.Browser Browser.empty
        custom = Context.Other Map.empty
        event = E.apply E.empty (E.setContextIfAbsent "browser" typed <> E.setContextIfAbsent "browser" custom)
        scope = U.run (Scope.setContextIfAbsent "browser" typed <> Scope.setContextIfAbsent "browser" custom) (U.empty :: ScopeData)
    event.contexts `shouldBe` Map.singleton "browser" typed
    scope.contexts `shouldBe` event.contexts
    (E.apply event [E.removeContext "browser", E.setContextIfAbsent "browser" custom]).contexts `shouldBe` Map.singleton "browser" custom
    (U.run [Scope.removeContext "browser", Scope.setContextIfAbsent "browser" custom] scope).contexts `shouldBe` Map.singleton "browser" custom
  it "forces inserted values and keys but leaves nested JSON lazy" do
    mapM_
      (\upd -> evaluate (E.apply E.empty upd) `shouldThrow` anyErrorCall)
      [E.setTagIfAbsent "key" (error "value"), E.setContextIfAbsent "key" (error "value"), E.setTagIfAbsent (error "key") "value", E.setContextIfAbsent (error "key") (Context.Other Map.empty)]
    mapM_
      (\upd -> evaluate (U.run upd (U.empty :: ScopeData)) `shouldThrow` anyErrorCall)
      [Scope.setTagIfAbsent "key" (error "value"), Scope.setContextIfAbsent "key" (error "value"), Scope.setTagIfAbsent (error "key") "value", Scope.setContextIfAbsent (error "key") (Context.Other Map.empty)]
    let payload = Context.Other (Map.singleton "lazy" (error "JSON"))
    evaluate (Map.size (E.apply E.empty (E.setContextIfAbsent "custom" payload)).contexts) `shouldReturn` 1
    evaluate (Map.size (U.run (Scope.setContextIfAbsent "custom" payload) (U.empty :: ScopeData)).contexts) `shouldReturn` 1
  it "checks scope defaults locally even when a lower layer has the key" do
    let lower = U.run [Scope.setTag "key" "lower", Scope.setContext "custom" (Context.Other Map.empty)] (U.empty :: ScopeData)
        local = U.run [Scope.setTagIfAbsent "key" "local", Scope.setContextIfAbsent "custom" (Context.Browser Browser.empty)] (U.empty :: ScopeData)
    (lower <> local).tags Map.! "key" `shouldBe` "local"
    (lower <> local).contexts `shouldBe` local.contexts
