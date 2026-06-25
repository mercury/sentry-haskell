module ScopeMutationTest where

import Data.Aeson qualified as Aeson
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Patrol qualified
import Patrol.Type.Breadcrumb qualified as Patrol.Breadcrumb
import Patrol.Type.Context qualified as Patrol.Context
import Patrol.Type.Event qualified as Patrol.Event
import Patrol.Type.Level qualified as Patrol.Level
import Patrol.Type.User qualified as Patrol.User
import Sentry.Event (CapturedEvent (..), fromMessage)
import Sentry.Scope (ScopeData (..))
import Sentry.Scope qualified as Scope
import Sentry.Scope.Update qualified as Update
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
      Scope.setFingerprint scope (Vector.fromList ["a", "b"])
      d <- Scope.readScopeRef scope
      d.fingerprint `shouldBe` Just (Vector.fromList ["a", "b"])
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

  it "the ScopeUpdate constructor and runScopeUpdate round-trip a modification" do
    let upd = Update.ScopeUpdate \s -> s{level = Just Patrol.Level.Error}
        result = Update.runScopeUpdate upd mempty
    result.level `shouldBe` Just Patrol.Level.Error

  it "mempty :: ScopeUpdate is the identity (no-op)" do
    scope <- Scope.create Scope.Current
    Scope.setLevel scope Patrol.Level.Error
    Update.apply scope mempty
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
      let d = Update.runScopeUpdate (Update.addBreadcrumb (crumb "a") <> Update.addBreadcrumb (crumb "b")) mempty
      map (.message) (toList d.breadcrumbs) `shouldBe` ["a", "b"]

    it "addBreadcrumbs appends a batch in order" do
      let d = Update.runScopeUpdate (Update.addBreadcrumbs [crumb "a", crumb "b", crumb "c"]) mempty
      map (.message) (toList d.breadcrumbs) `shouldBe` ["a", "b", "c"]

    it "clearBreadcrumbs empties the sequence" do
      let d = Update.runScopeUpdate (Update.addBreadcrumbs [crumb "a", crumb "b"] <> Update.clearBreadcrumbs) mempty
      toList d.breadcrumbs `shouldBe` []

    it "trimBreadcrumbs keeps the most recent n" do
      let d = Update.runScopeUpdate (Update.addBreadcrumbs [crumb "a", crumb "b", crumb "c"] <> Update.trimBreadcrumbs 2) mempty
      map (.message) (toList d.breadcrumbs) `shouldBe` ["b", "c"]

  describe "event processors" do
    let ce = Witch.from (fromMessage Patrol.Level.Info "hi") :: CapturedEvent
        setFatal c = Just c.event{Patrol.Event.level = Just Patrol.Level.Fatal}

    it "setEventProcessor replaces the processor" do
      let d = Update.runScopeUpdate (Update.setEventProcessor setFatal) mempty
      ((.level) <$> d.eventProcessor ce) `shouldBe` Just (Just Patrol.Level.Fatal)

    it "unsetEventProcessor restores the pass-through" do
      let d = Update.runScopeUpdate (Update.setEventProcessor (const Nothing) <> Update.unsetEventProcessor) mempty
      ((.level) <$> d.eventProcessor ce) `shouldBe` Just (Just Patrol.Level.Info)

    it "addEventProcessor chains after the existing processor" do
      let d = Update.runScopeUpdate (Update.addEventProcessor setFatal) mempty
      ((.level) <$> d.eventProcessor ce) `shouldBe` Just (Just Patrol.Level.Fatal)

    it "addEventProcessor is skipped once an earlier processor drops the event" do
      let d = Update.runScopeUpdate (Update.setEventProcessor (const Nothing) <> Update.addEventProcessor setFatal) mempty
      ((.level) <$> d.eventProcessor ce) `shouldBe` Nothing
