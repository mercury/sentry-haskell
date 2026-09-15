module ScopeDataTest where

import Data.Aeson qualified as Aeson
import Data.Default (def)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Patrol qualified
import Patrol.Type.Breadcrumb qualified as Patrol.Breadcrumb
import Patrol.Type.Context qualified as Patrol.Context
import Patrol.Type.Event qualified as Patrol.Event
import Patrol.Type.Level qualified as Patrol.Level
import Patrol.Type.User qualified as Patrol.User
import Sentry.Capture qualified as Capture
import Sentry.Event qualified
import Sentry.Event.Captured (CapturedEvent (..))
import Sentry.Scope.IO qualified as Scope.IO
import Sentry.Scope.Operations (ScopeData (..), ScopeType (..))
import Sentry.Scope.Operations qualified as Scope
import Sentry.Test qualified as Test
import Sentry.User qualified
import Test.Hspec
import Witch qualified

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
    let old = def{fingerprint = Just ["old"]}
        new = def{fingerprint = Just ["new"]}
    (old <> new).fingerprint `shouldBe` Just ["new"]

  it "falls back to old fingerprint when new is Nothing" do
    let old = def{fingerprint = Just ["old"]}
    (old <> def).fingerprint `shouldBe` Just ["old"]

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
    let old = def{breadcrumbs = Seq.fromList [crumb "a", crumb "b"]}
        new = def{breadcrumbs = Seq.fromList [crumb "c"]}
    (old <> new).breadcrumbs `shouldBe` Seq.fromList [crumb "a", crumb "b", crumb "c"]

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
    let old = def{eventProcessor = \_ -> Nothing}
        new = def{eventProcessor = \ce -> Just ce.event}
    -- old processor drops the event, so the chain should produce Nothing
    case (old <> new).eventProcessor (Witch.into @CapturedEvent Patrol.Event.empty) of
      Nothing -> pure ()
      Just _ -> expectationFailure "expected the chain to drop the event"

  it "passes the preceding processor result to the next layer" do
    let old = def{eventProcessor = \ce -> Just (Sentry.Event.apply ce.event (Sentry.Event.setLevel Patrol.Level.Warning))}
        new = def{eventProcessor = \ce -> if ce.event.level == Just Patrol.Level.Warning then Just ce.event{Patrol.Event.level = Just Patrol.Level.Error} else Nothing}
        merged = old <> new
    case merged.eventProcessor (Witch.into @CapturedEvent Patrol.Event.empty) of
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
    let a = def{breadcrumbs = Seq.fromList [crumb "1"], tags = Map.singleton "x" "a"}
        b = def{breadcrumbs = Seq.fromList [crumb "2"], tags = Map.singleton "x" "b"}
        c = def{breadcrumbs = Seq.fromList [crumb "3"], tags = Map.singleton "y" "c"}
        lhs = (a <> b) <> c
        rhs = a <> (b <> c)
    lhs.breadcrumbs `shouldBe` rhs.breadcrumbs
    lhs.tags `shouldBe` rhs.tags

-- Helpers

-- | Minimal breadcrumb with a distinguishable message.
crumb :: Text -> Patrol.Breadcrumb
crumb msg = Patrol.Breadcrumb.empty{Patrol.Breadcrumb.message = msg}

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

spec_eventPrecedence :: Spec
spec_eventPrecedence = describe "scope to Event precedence" do
  it "preserves present Event identity and transaction but uses scope collision keys and level" do
    let scope =
          def
            { user = Just Sentry.User.empty{Sentry.User.id = "scope"},
              transaction = Just "scope",
              level = Just Patrol.Level.Warning,
              tags = Map.singleton "key" "scope",
              extras = Map.singleton "key" Aeson.Null,
              contexts = Map.singleton "custom" (Patrol.Context.Other mempty)
            }
        event =
          Patrol.Event.empty
            { Patrol.Event.user = Just Sentry.User.empty,
              Patrol.Event.transaction = "event",
              Patrol.Event.level = Just Patrol.Level.Info,
              Patrol.Event.tags = Map.singleton "key" "event",
              Patrol.Event.extra = Map.singleton "key" (Aeson.Bool True),
              Patrol.Event.contexts = Map.singleton "custom" (Patrol.Context.Other (Map.singleton "old" Aeson.Null))
            }
    case Scope.applyToEvent scope (Witch.into @CapturedEvent event) of
      Nothing -> expectationFailure "unexpected drop"
      Just result -> do
        result.user `shouldBe` event.user
        result.transaction `shouldBe` "event"
        result.level `shouldBe` scope.level
        result.tags `shouldBe` scope.tags
        result.extra `shouldBe` scope.extras
        result.contexts `shouldBe` scope.contexts
    let fallback = Scope.applyToEvent scope (Witch.into @CapturedEvent Patrol.Event.empty)
    fmap (.user) fallback `shouldBe` Just scope.user
    fmap (.transaction) fallback `shouldBe` Just "scope"
    let emptyAssignment = scope{transaction = Just ""}
    fmap (.transaction) (Scope.applyToEvent emptyAssignment (Witch.into @CapturedEvent Patrol.Event.empty)) `shouldBe` Just ""

  it "processors observe merged values and enrich the delivered Event" do
    (_, transport) <- Test.withClient \_ -> Scope.IO.withScope \scope -> do
      Scope.setTag scope "collision" "scope"
      Scope.setEventProcessor scope \ce ->
        if Map.lookup "collision" ce.event.tags == Just "scope"
          then Just ce.event{Patrol.Event.tags = Map.insert "processed" "yes" ce.event.tags}
          else Nothing
      Capture.captureEvent_ Patrol.Event.empty{Patrol.Event.tags = Map.singleton "collision" "event"}
      local <- Scope.readScopeRef scope
      Map.lookup "processed" local.tags `shouldBe` Nothing
    events <- Test.fetchAndClearEvents transport
    map (.tags) events `shouldBe` [Map.fromList [("collision", "scope"), ("processed", "yes")]]
