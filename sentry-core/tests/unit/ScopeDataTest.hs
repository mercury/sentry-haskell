module ScopeDataTest where

import Data.Aeson qualified as Aeson
import Data.Default (def)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Vector qualified as Vector
import Patrol qualified
import Patrol.Type.Context qualified as Patrol.Context
import Patrol.Type.Event qualified as Patrol.Event
import Patrol.Type.Level qualified as Patrol.Level
import Patrol.Type.User qualified as Patrol.User
import Sentry.Scope (ScopeData (..), ScopeType (..))
import Test.Hspec

spec_ScopeData_Semigroup :: Spec
spec_ScopeData_Semigroup = describe "ScopeData Semigroup" do
  it "merged type_ is always Merged" do
    let old = def{type_ = Just Global}
        new = def{type_ = Just Current}
    (old <> new).type_ `shouldBe` Just Merged

  it "right-biased for level (new wins)" do
    let old = def{level = Just Patrol.Level.Warning}
        new = def{level = Just Patrol.Level.Error}
    (old <> new).level `shouldBe` Just Patrol.Level.Error

  it "falls back to old level when new is Nothing" do
    let old = def{level = Just Patrol.Level.Warning}
    (old <> def).level `shouldBe` Just Patrol.Level.Warning

  it "right-biased for fingerprint (new wins)" do
    let old = def{fingerprint = Just (Vector.fromList ["old"])}
        new = def{fingerprint = Just (Vector.fromList ["new"])}
    (old <> new).fingerprint `shouldBe` Just (Vector.fromList ["new"])

  it "falls back to old fingerprint when new is Nothing" do
    let old = def{fingerprint = Just (Vector.fromList ["old"])}
    (old <> def).fingerprint `shouldBe` Just (Vector.fromList ["old"])

  it "right-biased for transaction (new wins)" do
    let old = def{transaction = Just "old-txn"}
        new = def{transaction = Just "new-txn"}
    (old <> new).transaction `shouldBe` Just "new-txn"

  it "falls back to old transaction when new is Nothing" do
    let old = def{transaction = Just "old-txn"}
    (old <> def).transaction `shouldBe` Just "old-txn"

  it "right-biased for user (new wins)" do
    let old = def{user = Just testUser1}
        new = def{user = Just testUser2}
    (old <> new).user `shouldBe` Just testUser2

  it "falls back to old user when new is Nothing" do
    let old = def{user = Just testUser1}
    (old <> def).user `shouldBe` Just testUser1

  it "concatenates breadcrumbs (old ++ new)" do
    let old = def{breadcrumbs = Seq.fromList ["a", "b"]}
        new = def{breadcrumbs = Seq.fromList ["c"]}
    (old <> new).breadcrumbs `shouldBe` Seq.fromList ["a", "b", "c"]

  it "unions tags with right bias" do
    let old = def{tags = Map.fromList [("env", "staging"), ("shared", "old")]}
        new = def{tags = Map.fromList [("shared", "new"), ("extra", "val")]}
        merged = old <> new
    merged.tags `shouldBe` Map.fromList [("env", "staging"), ("shared", "new"), ("extra", "val")]

  it "unions extras with right bias" do
    let old = def{extras = Map.singleton "a" (Aeson.toJSON ("1" :: String))}
        new = def{extras = Map.singleton "a" (Aeson.toJSON ("2" :: String))}
    (old <> new).extras `shouldBe` Map.singleton "a" (Aeson.toJSON ("2" :: String))

  it "unions contexts with right bias" do
    let old = def{contexts = Map.singleton "browser" (Patrol.Context.Other mempty)}
        new = def{contexts = Map.singleton "browser" (Patrol.Context.Other (Map.singleton "name" (Aeson.toJSON ("Firefox" :: String))))}
    (old <> new).contexts `shouldBe` new.contexts

  it "chains eventProcessors left-to-right" do
    let old = def{eventProcessor = const Nothing}
        new = def{eventProcessor = Just}
    -- old processor drops the event, so the chain should produce Nothing
    (old <> new).eventProcessor Patrol.Event.empty `shouldBe` Nothing

  it "chains eventProcessors — both pass-through preserves event" do
    let old = def{eventProcessor = \e -> Just e{Patrol.Event.level = Just Patrol.Level.Warning}}
        new = def{eventProcessor = \e -> Just e{Patrol.Event.level = Just Patrol.Level.Error}}
        merged = old <> new
    case merged.eventProcessor Patrol.Event.empty of
      Nothing -> expectationFailure "expected event to pass through"
      Just event -> event.level `shouldBe` Just Patrol.Level.Error

spec_ScopeData_Monoid :: Spec
spec_ScopeData_Monoid = describe "ScopeData Monoid" do
  it "mempty is identity on the left" do
    let scope = def{level = Just Patrol.Level.Error, tags = Map.singleton "k" "v"}
        merged = mempty <> scope
    merged.level `shouldBe` Just Patrol.Level.Error
    merged.tags `shouldBe` Map.singleton "k" "v"

  it "mempty is identity on the right" do
    let scope = def{level = Just Patrol.Level.Error, tags = Map.singleton "k" "v"}
        merged = scope <> mempty
    merged.level `shouldBe` Just Patrol.Level.Error
    merged.tags `shouldBe` Map.singleton "k" "v"

  it "mempty <> mempty has all-empty fields" do
    let merged = mempty <> (mempty :: ScopeData)
    merged.level `shouldBe` Nothing
    merged.fingerprint `shouldBe` Nothing
    merged.transaction `shouldBe` Nothing
    merged.breadcrumbs `shouldBe` mempty
    merged.user `shouldBe` Nothing
    merged.extras `shouldBe` mempty
    merged.tags `shouldBe` mempty
    merged.contexts `shouldBe` mempty

  it "associativity: (a <> b) <> c == a <> (b <> c) for scalar fields" do
    let a = def{level = Just Patrol.Level.Debug, transaction = Just "a"}
        b = def{level = Just Patrol.Level.Warning}
        c = def{transaction = Just "c"}
        lhs = (a <> b) <> c
        rhs = a <> (b <> c)
    lhs.level `shouldBe` rhs.level
    lhs.transaction `shouldBe` rhs.transaction

  it "associativity: (a <> b) <> c == a <> (b <> c) for collection fields" do
    let a = def{breadcrumbs = Seq.fromList ["1"], tags = Map.singleton "x" "a"}
        b = def{breadcrumbs = Seq.fromList ["2"], tags = Map.singleton "x" "b"}
        c = def{breadcrumbs = Seq.fromList ["3"], tags = Map.singleton "y" "c"}
        lhs = (a <> b) <> c
        rhs = a <> (b <> c)
    lhs.breadcrumbs `shouldBe` rhs.breadcrumbs
    lhs.tags `shouldBe` rhs.tags

-- Helpers

testUser1 :: Patrol.User
testUser1 =
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

testUser2 :: Patrol.User
testUser2 =
  Patrol.User.User
    { data_ = mempty,
      email = "bob@example.com",
      geo = Nothing,
      id = "user-2",
      ipAddress = "",
      name = "Bob",
      segment = "",
      username = "bob"
    }
