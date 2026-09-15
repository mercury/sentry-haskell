module ScopeMutationTest where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Patrol qualified
import Patrol.Type.Breadcrumb qualified as Patrol.Breadcrumb
import Patrol.Type.Context qualified as Patrol.Context
import Patrol.Type.Event qualified as Patrol.Event
import Patrol.Type.Level qualified as Patrol.Level
import Patrol.Type.RuntimeContext qualified as Patrol.RuntimeContext
import Patrol.Type.User qualified as Patrol.User
import Sentry.Event (fromMessage)
import Sentry.Event qualified
import Sentry.Event.Captured (CapturedEvent (..))
import Sentry.Scope.Operations (ScopeData (..))
import Sentry.Scope.Operations qualified as Scope
import Sentry.Scope.Update qualified as Update
import Sentry.Update (Update (..), runUpdate)
import Test.Hspec
import Witch qualified

testUser :: Patrol.User
testUser =
  Patrol.User.User
    { data_ = mempty,
      email = "alice@example.com",
      geo = Nothing,
      id = "user-1",
      ipAddress = "",
      name = "Alice",
      segment = "",
      username = "alice"
    }

testContext :: Patrol.Context
testContext = Patrol.Context.Other (Map.singleton "name" (Aeson.toJSON ("Firefox" :: String)))

spec_setterFamily :: Spec
spec_setterFamily = describe "Sentry.Scope setter family" do
  describe "scalar fields" do
    it "setLevel sets level; unsetLevel clears it" do
      scope <- Scope.create Scope.Current
      Scope.setLevel scope Patrol.Level.Warning
      d <- Scope.readScopeRef scope
      d.level `shouldBe` Just Patrol.Level.Warning
      Scope.unsetLevel scope
      d' <- Scope.readScopeRef scope
      d'.level `shouldBe` Nothing

    it "setUser sets user; unsetUser clears it" do
      scope <- Scope.create Scope.Current
      Scope.setUser scope testUser
      d <- Scope.readScopeRef scope
      d.user `shouldBe` Just testUser
      Scope.unsetUser scope
      d' <- Scope.readScopeRef scope
      d'.user `shouldBe` Nothing

    it "setFingerprint sets fingerprint; unsetFingerprint clears it" do
      scope <- Scope.create Scope.Current
      Scope.setFingerprint scope ["a", "b"]
      d <- Scope.readScopeRef scope
      d.fingerprint `shouldBe` Just ["a", "b"]
      Scope.unsetFingerprint scope
      d' <- Scope.readScopeRef scope
      d'.fingerprint `shouldBe` Nothing

    it "setTransaction sets transaction; unsetTransaction clears it" do
      scope <- Scope.create Scope.Current
      Scope.setTransaction scope "checkout"
      d <- Scope.readScopeRef scope
      d.transaction `shouldBe` Just "checkout"
      Scope.unsetTransaction scope
      d' <- Scope.readScopeRef scope
      d'.transaction `shouldBe` Nothing

  describe "tags" do
    it "setTag inserts; removeTag deletes; clearTags wipes" do
      scope <- Scope.create Scope.Current
      Scope.setTag scope "env" "prod"
      Scope.setTag scope "tier" "free"
      d <- Scope.readScopeRef scope
      d.tags `shouldBe` Map.fromList [("env", "prod"), ("tier", "free")]
      Scope.removeTag scope "tier"
      d' <- Scope.readScopeRef scope
      d'.tags `shouldBe` Map.singleton "env" "prod"
      Scope.clearTags scope
      d'' <- Scope.readScopeRef scope
      d''.tags `shouldBe` Map.empty

  describe "extras" do
    it "setExtra inserts; removeExtra deletes; clearExtras wipes" do
      scope <- Scope.create Scope.Current
      Scope.setExtra scope "k1" (Aeson.toJSON ("v1" :: String))
      Scope.setExtra scope "k2" (Aeson.toJSON (42 :: Int))
      d <- Scope.readScopeRef scope
      Map.keys d.extras `shouldBe` ["k1", "k2"]
      Scope.removeExtra scope "k1"
      d' <- Scope.readScopeRef scope
      Map.keys d'.extras `shouldBe` ["k2"]
      Scope.clearExtras scope
      d'' <- Scope.readScopeRef scope
      d''.extras `shouldBe` Map.empty

  describe "contexts" do
    it "setContext inserts; removeContext deletes; clearContexts wipes" do
      scope <- Scope.create Scope.Current
      Scope.setContext scope "browser" testContext
      d <- Scope.readScopeRef scope
      Map.keys d.contexts `shouldBe` ["browser"]
      Scope.removeContext scope "browser"
      d' <- Scope.readScopeRef scope
      d'.contexts `shouldBe` Map.empty
      Scope.setContext scope "browser" testContext
      Scope.clearContexts scope
      d'' <- Scope.readScopeRef scope
      d''.contexts `shouldBe` Map.empty

    it "setRuntimeContext replaces a custom context at runtime" do
      scope <- Scope.create Scope.Current
      let rc = Patrol.RuntimeContext.empty{Patrol.RuntimeContext.name = "ghc"}
      Scope.setContext scope "runtime" testContext
      Scope.setRuntimeContext scope rc
      d <- Scope.readScopeRef scope
      Map.lookup "runtime" d.contexts `shouldBe` Just (Patrol.Context.Runtime rc)

    it "setContextValues sets an Other context from key/value pairs" do
      scope <- Scope.create Scope.Current
      Scope.setContextValues scope "custom" [("k", Aeson.toJSON ("v" :: String))]
      d <- Scope.readScopeRef scope
      Map.lookup "custom" d.contexts
        `shouldBe` Just (Patrol.Context.Other (Map.singleton "k" (Aeson.toJSON ("v" :: String))))

    it "setContextValues replaces previous fields and preserves other contexts" do
      scope <- Scope.create Scope.Current
      Scope.setContext scope "browser" testContext
      Scope.setContextValues scope "checkout" [("cart_id", Aeson.String "cart-123"), ("items", Aeson.Number 3)]
      Scope.setContextValues scope "checkout" [("items", Aeson.Number 1)]
      d <- Scope.readScopeRef scope
      d.contexts
        `shouldBe` Map.fromList
          [ ("browser", testContext),
            ("checkout", Patrol.Context.Other (Map.singleton "items" (Aeson.Number 1)))
          ]

    it "setContextValues uses the last value for duplicate keys" do
      scope <- Scope.create Scope.Current
      Scope.setContextValues scope "checkout" [("items", Aeson.Number 3), ("items", Aeson.Number 1)]
      d <- Scope.readScopeRef scope
      Map.lookup "checkout" d.contexts
        `shouldBe` Just (Patrol.Context.Other (Map.singleton "items" (Aeson.Number 1)))

    it "an empty context shadows inheritance and is serialized; removal reveals inheritance" do
      isolation <- Scope.create Scope.Isolation
      current <- Scope.create Scope.Current
      Scope.setContext isolation "custom" testContext
      Scope.setContextValues current "custom" []
      inherited <- Scope.readScopeRef isolation
      empty <- Scope.readScopeRef current
      Map.lookup "custom" (inherited <> empty).contexts
        `shouldBe` Just (Patrol.Context.Other Map.empty)
      let ce = Witch.from (fromMessage Patrol.Level.Info "hi") :: CapturedEvent
      case Aeson.toJSON <$> Scope.applyToEvent (inherited <> empty) ce of
        Just (Aeson.Object event) ->
          KeyMap.lookup "contexts" event
            `shouldBe` Just (Aeson.object ["custom" Aeson..= Aeson.object []])
        other -> expectationFailure ("Expected an event object, got " <> show other)
      Scope.removeContext current "custom"
      removed <- Scope.readScopeRef current
      Map.lookup "custom" removed.contexts `shouldBe` Nothing
      Map.lookup "custom" (inherited <> removed).contexts `shouldBe` Just testContext

spec_scopeUpdate :: Spec
spec_scopeUpdate = describe "Sentry.Scope.Update" do
  it "Update.apply applies a single smart-constructor update" do
    scope <- Scope.create Scope.Current
    Update.apply scope (Update.setLevel Patrol.Level.Warning)
    d <- Scope.readScopeRef scope
    d.level `shouldBe` Just Patrol.Level.Warning

  it "Monoid composition: later updates win over earlier on scalar fields" do
    scope <- Scope.create Scope.Current
    Update.apply scope (Update.setLevel Patrol.Level.Debug <> Update.setLevel Patrol.Level.Error)
    d <- Scope.readScopeRef scope
    d.level `shouldBe` Just Patrol.Level.Error

  it "Monoid composition merges multiple field updates" do
    scope <- Scope.create Scope.Current
    Update.apply scope $
      Update.setLevel Patrol.Level.Warning
        <> Update.setTag "env" "prod"
        <> Update.setUser testUser
    d <- Scope.readScopeRef scope
    d.level `shouldBe` Just Patrol.Level.Warning
    d.tags `shouldBe` Map.singleton "env" "prod"
    d.user `shouldBe` Just testUser

  it "the Update constructor and runUpdate round-trip a modification" do
    let upd = Update \s -> s{level = Just Patrol.Level.Error}
        result = runUpdate upd mempty
    result.level `shouldBe` Just Patrol.Level.Error

  it "mempty :: ScopeUpdate is the identity (no-op)" do
    scope <- Scope.create Scope.Current
    Scope.setLevel scope Patrol.Level.Error
    -- Annotated because 'Update.apply' accepts anything convertible to a
    -- 'ScopeUpdate', so a bare 'mempty' has nothing to pin its type to.
    Update.apply scope (mempty :: Update.ScopeUpdate)
    d <- Scope.readScopeRef scope
    d.level `shouldBe` Just Patrol.Level.Error

  it "removeTag in an Update clears the tag" do
    scope <- Scope.create Scope.Current
    Scope.setTag scope "env" "prod"
    scope `Update.apply` (Update.removeTag "env")
    d <- Scope.readScopeRef scope
    d.tags `shouldBe` Map.empty

  it "clearTags in an Update wipes the tags map" do
    scope <- Scope.create Scope.Current
    Scope.setTag scope "env" "prod"
    Scope.setTag scope "tier" "free"
    scope `Update.apply` Update.clearTags
    d <- Scope.readScopeRef scope
    d.tags `shouldBe` Map.empty

spec_scopeUpdateCoverage :: Spec
spec_scopeUpdateCoverage = describe "Sentry.Scope.Update extended coverage" do
  describe "breadcrumbs" do
    let crumb m = Patrol.Breadcrumb.empty{Patrol.Breadcrumb.message = m}

    it "addBreadcrumb appends in order" do
      let d = runUpdate (Update.appendBreadcrumb (crumb "a") <> Update.appendBreadcrumb (crumb "b")) mempty
      map (.message) (toList d.breadcrumbs) `shouldBe` ["a", "b"]

    it "addBreadcrumbs appends a batch in order" do
      let d = runUpdate (Update.appendBreadcrumbs [crumb "a", crumb "b", crumb "c"]) mempty
      map (.message) (toList d.breadcrumbs) `shouldBe` ["a", "b", "c"]

    it "clearBreadcrumbs empties the sequence" do
      let d = runUpdate (Update.appendBreadcrumbs [crumb "a", crumb "b"] <> Update.clearBreadcrumbs) mempty
      toList d.breadcrumbs `shouldBe` []

    it "trimBreadcrumbs keeps the most recent n" do
      let d = runUpdate (Update.appendBreadcrumbs [crumb "a", crumb "b", crumb "c"] <> Update.trimBreadcrumbs 2) mempty
      map (.message) (toList d.breadcrumbs) `shouldBe` ["b", "c"]

  describe "event processors" do
    let ce = Witch.from (fromMessage Patrol.Level.Info "hi") :: CapturedEvent
        setFatal captured = Just (Sentry.Event.apply captured.event (Sentry.Event.setLevel Patrol.Level.Fatal))
        -- The processor returns an update, so run it against the event it saw.
        levelAfter d = (.level) <$> d.eventProcessor ce

    it "setEventProcessor replaces the processor" do
      let d = runUpdate (Update.setEventProcessor setFatal) mempty
      levelAfter d `shouldBe` Just (Just Patrol.Level.Fatal)

    it "unsetEventProcessor restores the pass-through" do
      let d = runUpdate (Update.setEventProcessor (const Nothing) <> Update.unsetEventProcessor) mempty
      levelAfter d `shouldBe` Just (Just Patrol.Level.Info)

    it "addEventProcessor chains after the existing processor" do
      let d = runUpdate (Update.addEventProcessor setFatal) mempty
      levelAfter d `shouldBe` Just (Just Patrol.Level.Fatal)

    it "addEventProcessor is skipped once an earlier processor drops the event" do
      let d = runUpdate (Update.setEventProcessor (const Nothing) <> Update.addEventProcessor setFatal) mempty
      levelAfter d `shouldBe` Nothing

spec_contextRemovalInheritance :: Spec
spec_contextRemovalInheritance = describe "local context field removal" do
  it "leaves an absent local context absent and preserves its inherited payload" do
    isolation <- Scope.create Scope.Isolation
    current <- Scope.create Scope.Current
    Scope.setContext isolation "custom" testContext
    Scope.removeContextValue current "custom" "secret"
    inherited <- Scope.readScopeRef isolation
    local <- Scope.readScopeRef current
    Map.lookup "custom" local.contexts `shouldBe` Nothing
    let captured = Witch.into @CapturedEvent (fromMessage Patrol.Level.Info "hi")
    fmap (Map.lookup "custom" . (.contexts)) (Scope.applyToEvent (inherited <> local) captured)
      `shouldBe` Just (Just testContext)

  it "keeps an empty custom context when its last field is removed" do
    scope <- Scope.create Scope.Current
    Scope.setContextValue scope "custom" "last" Aeson.Null
    Scope.removeContextValue scope "custom" "last"
    snapshot <- Scope.readScopeRef scope
    Map.lookup "custom" snapshot.contexts `shouldBe` Just (Patrol.Context.Other Map.empty)

  it "does not change typed contexts" do
    scope <- Scope.create Scope.Current
    Scope.setRuntimeContext scope Patrol.RuntimeContext.empty
    originalData <- Scope.readScopeRef scope
    Scope.removeContextValue scope "runtime" "name"
    editedData <- Scope.readScopeRef scope
    editedData.contexts `shouldBe` originalData.contexts
