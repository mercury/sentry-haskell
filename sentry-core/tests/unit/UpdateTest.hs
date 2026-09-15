module UpdateTest where

import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Exception (evaluate, finally, throwIO)
import Control.Monad (replicateM_)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Default (def)
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Time.Clock (UTCTime)
import Sentry.AppContext qualified
import Sentry.BrowserContext qualified
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Context qualified
import Sentry.Core qualified as Sentry
import Sentry.DeviceContext qualified
import Sentry.Event qualified
import Sentry.Event.Captured (CapturedEvent (..))
import Sentry.Geo qualified
import Sentry.OsContext qualified
import Sentry.Request qualified
import Sentry.RuntimeContext qualified
import Sentry.Scope qualified as Builders
import Sentry.Scope.Operations qualified as Scope
import Sentry.Scope.Update qualified as Update
import Sentry.Test qualified as Test
import Sentry.TraceContext qualified
import Sentry.Update qualified
import Sentry.User qualified
import System.Timeout (timeout)
import Test.Hspec
import Witch qualified

-- | The update that changes nothing.
--
-- Named because a bare 'mempty' is ambiguous at these call sites:
-- 'Update.setUser' and 'Update.modifyUser' accept anything convertible to a
-- 'Sentry.UserUpdate', so there is nothing to pin the type to. Real code
-- assigns something.
noChange :: Sentry.UserUpdate
noChange = mempty

-- | Run a 'Sentry.UserUpdate' against an empty record. Spelled out here so the
-- assertions below stay readable; in real code this is 'Witch.from'.
asUser :: Sentry.UserUpdate -> Sentry.User
asUser = Witch.from

-- | As 'asUser', for geo.
asGeo :: Sentry.GeoUpdate -> Sentry.Geo
asGeo = Witch.from

-- | As 'asUser', for events.
asEvent :: Sentry.EventUpdate -> Sentry.Event
asEvent = Witch.from

-- | The user an update leaves behind when applied to an empty scope.
userAfter :: Update.ScopeUpdate -> Maybe Sentry.User
userAfter action = (Sentry.Update.run action (mempty @Sentry.ScopeData)).user

-- | A scope that already carries a user, so 'Update.modifyUser' has something
-- to act on.
seeded :: Update.ScopeUpdate
seeded = Update.setUser [Sentry.User.setId "123", Sentry.User.setEmail "old"]

spec_composition :: Spec
spec_composition = describe "UserUpdate composition" do
  it "applies the left operand first, so later assignments win" do
    asUser (Sentry.User.setId "first" <> Sentry.User.setId "last")
      `shouldBe` Sentry.User.empty{Sentry.User.id = "last"}
    asGeo (Sentry.Geo.setCity "Boston" <> Sentry.Geo.setCity "Detroit")
      `shouldBe` Sentry.Geo.empty{Sentry.Geo.city = "Detroit"}

  it "is a monoid: mempty is an identity and composition associates" do
    let a = Sentry.User.setId "1"
        b = Sentry.User.setName "b"
        c = Sentry.User.setEmail "c@example.com"
    asUser (mempty <> a) `shouldBe` asUser a
    asUser (a <> mempty) `shouldBe` asUser a
    asUser ((a <> b) <> c) `shouldBe` asUser (a <> (b <> c))
    asUser mempty `shouldBe` Sentry.User.empty

  it "folds a collection of updates" do
    asUser (mconcat [Sentry.User.setId "1", Sentry.User.setName "Alice"])
      `shouldBe` Sentry.User.empty{Sentry.User.id = "1", Sentry.User.name = "Alice"}
    asUser (foldMap Sentry.User.setName ["a", "b", "last"])
      `shouldBe` Sentry.User.empty{Sentry.User.name = "last"}
    asUser (mconcat []) `shouldBe` Sentry.User.empty

  it "assigns every text field" do
    asUser
      ( Sentry.User.setId "1"
          <> Sentry.User.setName "Alice"
          <> Sentry.User.setEmail "alice@example.com"
          <> Sentry.User.setIpAddress "127.0.0.1"
          <> Sentry.User.setSegment "beta"
          <> Sentry.User.setUsername "alice"
      )
      `shouldBe` Sentry.User.empty
        { Sentry.User.id = "1",
          Sentry.User.name = "Alice",
          Sentry.User.email = "alice@example.com",
          Sentry.User.ipAddress = "127.0.0.1",
          Sentry.User.segment = "beta",
          Sentry.User.username = "alice"
        }

spec_listForm :: Spec
spec_listForm = describe "List form" do
  it "is interchangeable with a <> chain" do
    let fields = [Sentry.User.setId "1", Sentry.User.setName "Alice"]
    Sentry.Update.run fields Sentry.User.empty
      `shouldBe` asUser (Sentry.User.setId "1" <> Sentry.User.setName "Alice")
    userAfter (Update.setUser fields) `shouldBe` userAfter (Update.setUser (mconcat fields))

  it "treats an empty list as the update that changes nothing" do
    Sentry.Update.run ([] :: [Sentry.UserUpdate]) Sentry.User.empty
      `shouldBe` Sentry.User.empty
    userAfter (seeded <> Update.modifyUser ([] :: [Sentry.UserUpdate]))
      `shouldBe` userAfter seeded

  it "nests: a list of updates stands in for a sub-record" do
    asUser (Sentry.User.setGeo [Sentry.Geo.setCity "Detroit", Sentry.Geo.setCountryCode "US"])
      `shouldBe` asUser
        (Sentry.User.setGeo (Sentry.Geo.setCity "Detroit" <> Sentry.Geo.setCountryCode "US"))

spec_conversions :: Spec
spec_conversions = describe "Witch conversions" do
  it "runs an update against the empty record" do
    (Witch.from (Sentry.User.setId "42") :: Sentry.User)
      `shouldBe` Sentry.User.empty{Sentry.User.id = "42"}

  it "runs a list of updates against the empty record" do
    (Witch.from [Sentry.User.setId "42"] :: Sentry.User)
      `shouldBe` Sentry.User.empty{Sentry.User.id = "42"}

  it "treats a record as the constant update, discarding what preceded it" do
    let alice = Sentry.User.empty{Sentry.User.name = "Alice"}
        constant = Witch.from alice :: Sentry.UserUpdate
    asUser (Sentry.User.setId "ignored" <> constant) `shouldBe` alice
    asUser (constant <> Sentry.User.setId "kept")
      `shouldBe` alice{Sentry.User.id = "kept"}

  it "collapses a list of updates into one update" do
    let collapsed = Witch.from [Sentry.User.setId "1", Sentry.User.setName "Alice"] :: Sentry.UserUpdate
    asUser collapsed `shouldBe` asUser (Sentry.User.setId "1" <> Sentry.User.setName "Alice")

  it "runs against a base other than empty" do
    Sentry.Update.run (Sentry.User.setName "Alice") Sentry.User.empty{Sentry.User.id = "42"}
      `shouldBe` Sentry.User.empty{Sentry.User.id = "42", Sentry.User.name = "Alice"}

spec_nestedGeo :: Spec
spec_nestedGeo = describe "Nested geo" do
  it "sets, updates and unsets" do
    let withGeo = Sentry.User.setGeo (Sentry.Geo.setCity "Boston" <> Sentry.Geo.setRegion "MA")
    asUser withGeo
      `shouldBe` Sentry.User.empty
        { Sentry.User.geo =
            Just Sentry.Geo.empty{Sentry.Geo.city = "Boston", Sentry.Geo.region = "MA"}
        }
    asUser (withGeo <> Sentry.User.modifyGeo (Sentry.Geo.setCity "Cambridge"))
      `shouldBe` Sentry.User.empty
        { Sentry.User.geo =
            Just Sentry.Geo.empty{Sentry.Geo.city = "Cambridge", Sentry.Geo.region = "MA"}
        }
    asUser (withGeo <> Sentry.User.unsetGeo) `shouldBe` Sentry.User.empty
    asUser (withGeo <> Sentry.User.setGeo Sentry.Geo.empty)
      `shouldBe` Sentry.User.empty{Sentry.User.geo = Just Sentry.Geo.empty}

  it "leaves an absent geo alone without applying the update" do
    asUser (Sentry.User.modifyExistingGeo (Sentry.Geo.setCity "Boston")) `shouldBe` Sentry.User.empty
    asUser (Sentry.User.modifyExistingGeo (error "must not run" :: Sentry.GeoUpdate))
      `shouldBe` Sentry.User.empty

spec_keyedData :: Spec
spec_keyedData = describe "Keyed user data" do
  it "inserts, overwrites, removes and clears in order" do
    let fields =
          Sentry.User.setData "a" Aeson.Null
            <> Sentry.User.setData "a" (Aeson.Bool True)
            <> Sentry.User.setData "b" Aeson.Null
            <> Sentry.User.removeData "b"
            <> Sentry.User.removeData "missing"
    asUser fields
      `shouldBe` Sentry.User.empty{Sentry.User.data_ = Map.singleton "a" (Aeson.Bool True)}
    asUser (fields <> Sentry.User.clearData) `shouldBe` Sentry.User.empty
    asUser (fields <> Sentry.User.clearData <> Sentry.User.setData "c" Aeson.Null)
      `shouldBe` Sentry.User.empty{Sentry.User.data_ = Map.singleton "c" Aeson.Null}

spec_reading :: Spec
spec_reading = describe "with" do
  it "reads every preceding assignment and nothing that follows" do
    asUser
      ( Sentry.User.setName "before"
          <> Sentry.User.setGeo (Sentry.Geo.setCity "Detroit")
          <> Sentry.User.with \u ->
            [ Sentry.User.setName "after",
              Sentry.User.setData "name" (Aeson.String u.name),
              Sentry.User.setData "city" (Aeson.String (foldMap (.city) u.geo))
            ]
      )
      `shouldBe` Sentry.User.empty
        { Sentry.User.name = "after",
          Sentry.User.geo = Just Sentry.Geo.empty{Sentry.Geo.city = "Detroit"},
          Sentry.User.data_ =
            Map.fromList
              [ ("name", Aeson.String "before"),
                ("city", Aeson.String "Detroit")
              ]
        }

  it "reads nested geo mid-composition" do
    asUser
      ( Sentry.User.setGeo [Sentry.Geo.setCity "Detroit", Sentry.Geo.setCountryCode "US"]
          <> Sentry.User.modifyGeo
            ( Sentry.Geo.with \g ->
                if g.countryCode == "US" && g.city == "Detroit"
                  then Sentry.Geo.setRegion "Michigan"
                  else mempty
            )
      )
      `shouldBe` Sentry.User.empty
        { Sentry.User.geo =
            Just
              Sentry.Geo.empty
                { Sentry.Geo.city = "Detroit",
                  Sentry.Geo.countryCode = "US",
                  Sentry.Geo.region = "Michigan"
                }
        }

  it "reads an event mid-composition" do
    asEvent
      ( Sentry.Event.setTag "env" "prod"
          <> Sentry.Event.with \e ->
            Sentry.Event.appendFingerprintComponent (if Map.member "env" e.tags then "tagged" else "bare")
      )
      `shouldBe` Sentry.Event.empty
        { Sentry.Event.tags = Map.singleton "env" "prod",
          Sentry.Event.fingerprint = ["tagged"]
        }

spec_scopeOperations :: Spec
spec_scopeOperations = describe "Scope user operations" do
  it "setUser replaces wholesale; modifyUser preserves unassigned fields" do
    userAfter (seeded <> Update.modifyUser (Sentry.User.setEmail "new"))
      `shouldBe` Just Sentry.User.empty{Sentry.User.id = "123", Sentry.User.email = "new"}
    userAfter (seeded <> Update.setUser (Sentry.User.setEmail "new"))
      `shouldBe` Just Sentry.User.empty{Sentry.User.email = "new"}

  it "modifyUser creates a local user when absent" do
    userAfter (Update.modifyUser (Sentry.User.setId "123")) `shouldBe` Just Sentry.User.empty{Sentry.User.id = "123"}
    userAfter (Update.modifyUser noChange) `shouldBe` Just Sentry.User.empty
    userAfter (Update.modifyUser noChange <> Update.unsetUser) `shouldBe` Nothing

  it "stores an empty user when one is set explicitly" do
    userAfter (Update.setUser noChange) `shouldBe` Just Sentry.User.empty
    userAfter (seeded <> Update.setUser noChange) `shouldBe` Just Sentry.User.empty

  it "never applies the update for an absent user, even a diverging one" do
    scope <- Scope.create Scope.Current
    Sentry.modifyExistingUser scope (error "must not run" :: Sentry.UserUpdate)
    result <- Scope.readScopeRef scope
    result.user `shouldBe` Nothing

spec_strictness :: Spec
spec_strictness = describe "Update strictness" do
  it "raises at the call site rather than storing a bottom in the scope" do
    scope <- Scope.create Scope.Current
    Sentry.setUser scope (Sentry.User.setId "present")
    -- A diverging update as a whole.
    Sentry.modifyUser scope (error "whole update" :: Sentry.UserUpdate) `shouldThrow` anyErrorCall
    -- A diverging value inside a field setter: patrol's User fields are lazy,
    -- so this only surfaces here because the setters are strict too.
    Sentry.modifyUser scope (Sentry.User.setName (error "field value")) `shouldThrow` anyErrorCall
    Sentry.modifyUser scope (Sentry.User.setData "k" (error "data value")) `shouldThrow` anyErrorCall
    -- The scope survives each failed attempt with its original user intact.
    result <- Scope.readScopeRef scope
    fmap (.id) result.user `shouldBe` Just "present"

  it "raises when building a user rather than deferring to serialization" do
    evaluate ((asUser (Sentry.User.setId (error "built field"))).id) `shouldThrow` anyErrorCall

spec_inheritance :: Spec
spec_inheritance = describe "Inheritance" do
  it "a local user shadows an inherited one and unsetUser reveals it" do
    (_, transport) <- Test.withClient \_ ->
      Sentry.withIsolationScope \outer -> do
        Sentry.setUser outer (Sentry.User.setId "inherited")
        Sentry.withScope \inner -> do
          Sentry.setUser inner (Sentry.User.setName "local")
          Sentry.captureMessage_ Sentry.Info "local"
          Sentry.unsetUser inner
          Sentry.captureMessage_ Sentry.Info "inherited"
    events <- Test.fetchAndClearEvents transport
    fmap (.user) events
      `shouldBe` [ Just Sentry.User.empty{Sentry.User.name = "local"},
                   Just Sentry.User.empty{Sentry.User.id = "inherited"}
                 ]

  it "an explicitly empty local user masks an inherited one and sends no user" do
    (_, transport) <- Test.withClient \_ ->
      Sentry.withIsolationScope \outer -> do
        Sentry.setUser outer (Sentry.User.setId "inherited")
        Sentry.withScope \inner -> do
          Sentry.setUser inner Sentry.User.empty
          Sentry.captureMessage_ Sentry.Info "masked"
    events <- Test.fetchAndClearEvents transport
    -- The empty user wins the merge, and then serializes away entirely: the
    -- event carries no user at all rather than the inherited one. Use
    -- 'unsetUser' to reveal the parent instead.
    fmap (.user) events `shouldBe` [Just Sentry.User.empty]
    fmap hasUserKey events `shouldBe` [False]

  it "cleanup-only updates preserve an inherited identity" do
    (_, transport) <- Test.withClient \_ ->
      Sentry.withIsolationScope \outer -> do
        Sentry.setUser outer (Sentry.User.setId "inherited")
        Sentry.withScope \inner -> do
          Sentry.modifyExistingUser inner noChange
          Sentry.captureMessage_ Sentry.Info "noop"
          Sentry.modifyExistingUser inner [Sentry.User.unsetGeo, Sentry.User.clearData]
          Sentry.captureMessage_ Sentry.Info "cleanup"
    events <- Test.fetchAndClearEvents transport
    fmap (.user) events
      `shouldBe` replicate 2 (Just Sentry.User.empty{Sentry.User.id = "inherited"})

spec_eventUpdates :: Spec
spec_eventUpdates = describe "EventUpdate" do
  it "assigns, removes and reads event fields" do
    let built =
          Sentry.Update.run
            [ Sentry.Event.setTag "env" "prod",
              Sentry.Event.setExtra "authorization" (Aeson.String "secret"),
              Sentry.Event.removeExtra "authorization",
              Sentry.Event.setUser (Sentry.User.setId "42")
            ]
            Sentry.Event.empty
    built.tags `shouldBe` Map.singleton "env" "prod"
    built.extra `shouldBe` Map.empty
    fmap (.id) built.user `shouldBe` Just "42"

  it "refines a nested user, with an explicit existing-only operation" do
    let withUser = asEvent (Sentry.Event.setUser (Sentry.User.setId "42"))
        refined = Sentry.Update.run (Sentry.Event.modifyUser (Sentry.User.setName "Alice")) withUser
    fmap (.id) refined.user `shouldBe` Just "42"
    fmap (.name) refined.user `shouldBe` Just "Alice"
    asEvent (Sentry.Event.modifyExistingUser (error "must not run" :: Sentry.UserUpdate))
      `shouldBe` Sentry.Event.empty

  it "drops the user wholesale, which is the common scrubbing move" do
    Sentry.Update.run
      [Sentry.Event.setUser (Sentry.User.setId "42"), Sentry.Event.unsetUser]
      Sentry.Event.empty
      `shouldBe` Sentry.Event.empty

spec_hooks :: Spec
spec_hooks = describe "Hooks returning records" do
  it "applies a beforeSend update to the delivered event" do
    let opts =
          (def @ClientOptions)
            { beforeSend = Just \ce ->
                Just $
                  Sentry.Event.apply
                    ce.event
                    ( Sentry.Event.setTag "kind" (if isJust ce.exception then "exception" else "message")
                        <> Sentry.Event.unsetUser
                    )
            }
    (_, transport) <- Test.withCustomClient opts \_ ->
      Sentry.withIsolationScope \scope -> do
        Sentry.setUser scope (Sentry.User.setId "42")
        Sentry.captureMessage_ Sentry.Info "hooked"
    events <- Test.fetchAndClearEvents transport
    fmap (.tags) events `shouldBe` [Map.singleton "kind" "message"]
    fmap (.user) events `shouldBe` [Nothing]

  it "leaves the event untouched when the hook returns its input" do
    let opts = (def @ClientOptions){beforeSend = Just \ce -> Just ce.event}
    (_, transport) <- Test.withCustomClient opts \_ ->
      Sentry.captureMessage_ Sentry.Info "untouched"
    events <- Test.fetchAndClearEvents transport
    fmap (.tags) events `shouldBe` [Map.empty]

  it "accepts a whole replacement event directly" do
    let replacement = asEvent (Sentry.Event.setTag "replaced" "yes")
        opts = (def @ClientOptions){beforeSend = Just \_ -> Just replacement}
    (_, transport) <- Test.withCustomClient opts \_ ->
      Sentry.captureMessage_ Sentry.Info "replaced"
    events <- Test.fetchAndClearEvents transport
    fmap (.tags) events `shouldBe` [Map.singleton "replaced" "yes"]

spec_eventProcessorChaining :: Spec
spec_eventProcessorChaining = describe "addEventProcessor" do
  it "composes rather than replaces, and the second sees the first's effect" do
    scope <- Scope.create Scope.Current
    Scope.setEventProcessor scope \ce -> Just (Sentry.Event.apply ce.event (Sentry.Event.setTag "first" "yes"))
    Scope.addEventProcessor scope \ce ->
      Just $ Sentry.Event.apply ce.event (Sentry.Event.setTag "saw-first" (if Map.member "first" ce.event.tags then "yes" else "no"))
    scopeData <- Scope.readScopeRef scope
    case scopeData.eventProcessor (Witch.into @CapturedEvent Sentry.Event.empty) of
      Nothing -> expectationFailure "expected the event to survive both processors"
      Just update ->
        update.tags
          `shouldBe` Map.fromList [("first", "yes"), ("saw-first", "yes")]

  it "does not run the second processor when the first drops the event" do
    scope <- Scope.create Scope.Current
    Scope.setEventProcessor scope \_ -> Nothing
    Scope.addEventProcessor scope \_ -> error "must not run"
    scopeData <- Scope.readScopeRef scope
    case scopeData.eventProcessor (Witch.into @CapturedEvent Sentry.Event.empty) of
      Nothing -> pure ()
      Just _ -> expectationFailure "expected the first processor's drop to short-circuit"

  it "applies both updates through Scope.applyToEvent, in order" do
    scope <- Scope.create Scope.Current
    Scope.setEventProcessor scope \ce -> Just (Sentry.Event.apply ce.event (Sentry.Event.setLevel Sentry.Warning))
    Scope.addEventProcessor scope \ce -> Just (Sentry.Event.apply ce.event (Sentry.Event.setLevel Sentry.Fatal))
    scopeData <- Scope.readScopeRef scope
    let captured = Witch.into @CapturedEvent Sentry.Event.empty
    fmap (.level) (scopeData `Scope.applyToEvent` captured) `shouldBe` Just (Just Sentry.Fatal)

spec_concurrency :: Spec
spec_concurrency = describe "Concurrent scope updates" do
  it "preserves distinct fields written from separate threads" do
    scope <- Scope.create Scope.Current
    Sentry.setUser scope noChange
    concurrentlyBounded
      [ Sentry.updateScope scope (Builders.modifyUser (Sentry.User.setId "123")),
        Sentry.updateScope scope (Builders.modifyUser (Sentry.User.setEmail "alice@example.com"))
      ]
    result <- Scope.readScopeRef scope
    result.user
      `shouldBe` Just
        Sentry.User.empty{Sentry.User.id = "123", Sentry.User.email = "alice@example.com"}

  it "retries read-modify-write updates under contention without losing any" do
    -- 'Update.modifyUser' forces the new user inside the scope's compare-and-swap
    -- loop, so a 'Sentry.User.with' read and the assignment that depends on it
    -- land in the same atomic update and a losing thread re-runs the whole thing.
    scope <- Scope.create Scope.Current
    Sentry.setUser scope (Sentry.User.setData "count" (Aeson.Number 0))
    let increment = Sentry.updateScope scope . Builders.modifyUser $ Sentry.User.with \u ->
          case Map.lookup "count" u.data_ of
            Just (Aeson.Number n) -> Sentry.User.setData "count" (Aeson.Number (n + 1))
            other -> error ("unexpected counter: " <> show other)
    concurrentlyBounded (replicate 4 (replicateM_ 100 increment))
    result <- Scope.readScopeRef scope
    fmap (Map.lookup "count" . (.data_)) result.user `shouldBe` Just (Just (Aeson.Number 400))

-- | Whether a serialized event actually carries a @user@ key on the wire.
--
-- patrol's encoder drops empty objects, so an all-empty user vanishes here even
-- though it is present in the merged 'Sentry.ScopeData'.
hasUserKey :: Sentry.Event -> Bool
hasUserKey event = case Aeson.toJSON event of
  Aeson.Object o -> KeyMap.member "user" o
  _ -> False

-- Start workers together, propagate their exceptions, and bound completion.
concurrentlyBounded :: [IO ()] -> IO ()
concurrentlyBounded actions = do
  start <- newEmptyMVar
  workers <-
    traverse
      ( \action -> do
          done <- newEmptyMVar
          tid <- forkFinally (readMVar start >> action) (putMVar done)
          pure (tid, done)
      )
      actions
  let wait = do
        putMVar start ()
        for_ workers \(_, done) -> takeMVar done >>= either throwIO pure
  completed <- timeout 5000000 wait `finally` for_ workers (killThread . fst)
  completed `shouldBe` Just ()

spec_updateScope :: Spec
spec_updateScope = describe "atomic public scope edits" do
  it "gives single, list, and composed forms the same ordering" do
    scopes <- traverse (const (Scope.create Scope.Current)) [1 .. 3 :: Int]
    let first = Builders.setTag "key" "first"
        lastUpdate = Builders.setTag "key" "last"
        edits =
          [Sentry.updateScope s (lastUpdate) | s <- take 1 scopes]
            <> [Sentry.updateScope s [first, lastUpdate] | s <- take 1 (drop 1 scopes)]
            <> [Sentry.updateScope s (first <> lastUpdate) | s <- drop 2 scopes]
    sequence_ edits
    results <- traverse Scope.readScopeRef scopes
    fmap (.tags) results `shouldBe` replicate 3 (Map.singleton "key" "last")

  it "never exposes a partially applied bundle to concurrent readers" do
    scope <- Scope.create Scope.Current
    let pair value = [Builders.setTag "pair" value, Builders.setUser (Sentry.User.setName value)]
        check = do
          snapshot <- Scope.readScopeRef scope
          Map.lookup "pair" snapshot.tags `shouldBe` fmap (.name) snapshot.user
    Sentry.updateScope scope (pair "initial")
    concurrentlyBounded
      [ replicateM_ 1000 (Sentry.updateScope scope (pair "one")),
        replicateM_ 1000 (Sentry.updateScope scope (pair "two")),
        replicateM_ 1000 check
      ]
    check

  it "refines a cloned local user independently and leaves absent clones absent" do
    original <- Scope.create Scope.Current
    Sentry.updateScope original (Builders.setUser (Sentry.User.setId "original"))
    cloned <- Scope.clone original
    Sentry.updateScope cloned (Builders.modifyUser (Sentry.User.setName "clone"))
    originalData <- Scope.readScopeRef original
    editedData <- Scope.readScopeRef cloned
    fmap (.name) originalData.user `shouldBe` Just ""
    fmap (.id) editedData.user `shouldBe` Just "original"
    fmap (.name) editedData.user `shouldBe` Just "clone"
    absent <- Scope.create Scope.Current >>= Scope.clone
    Sentry.updateScope absent (Builders.modifyExistingUser (Sentry.User.setName "ignored"))
    snapshot <- Scope.readScopeRef absent
    snapshot.user `shouldBe` Nothing

spec_extendedRecords :: Spec
spec_extendedRecords = describe "request and typed context builders" do
  it "sets every request scalar and map and clears them" do
    let request = Witch.from [Sentry.Request.setUrl "url", Sentry.Request.setMethod "POST", Sentry.Request.setFragment "fragment", Sentry.Request.setInferredContentType "json", Sentry.Request.setData (Aeson.Bool True), Sentry.Request.setCookie "a" "b", Sentry.Request.setEnv "a" Aeson.Null, Sentry.Request.setQueryParam "q" "v", Sentry.Request.setHeader "Accept" "json"] :: Sentry.Request
    request.url `shouldBe` "url"
    request.method `shouldBe` "POST"
    request.fragment `shouldBe` "fragment"
    request.inferredContentType `shouldBe` "json"
    request.data_ `shouldBe` Aeson.Bool True
    request.cookies `shouldBe` Map.singleton "a" "b"
    request.env `shouldBe` Map.singleton "a" Aeson.Null
    request.queryString `shouldBe` Map.singleton "q" "v"
    let cleared = Sentry.Update.run [Sentry.Request.clearData, Sentry.Request.clearCookies, Sentry.Request.clearEnv, Sentry.Request.clearQueryString, Sentry.Request.clearHeaders] request
    cleared `shouldBe` Sentry.Request.empty{Sentry.Request.url = "url", Sentry.Request.method = "POST", Sentry.Request.fragment = "fragment", Sentry.Request.inferredContentType = "json"}
    (Sentry.Update.run [Sentry.Request.removeCookie "a", Sentry.Request.removeEnv "a", Sentry.Request.removeQueryParam "q"] request).cookies `shouldBe` Map.empty
  it "normalizes only ASCII header equivalence and preserves replacement records" do
    let original = Sentry.Request.empty{Sentry.Request.headers = Map.fromList [("FOO", "1"), ("foo", "2"), ("Ä", "3"), ("ä", "4")]}
    (asEvent (Sentry.Event.setRequest original)).request `shouldBe` Just original
    let updated = Sentry.Update.run (Sentry.Request.setHeader "Foo" "last") original
    updated.headers `shouldBe` Map.fromList [("Foo", "last"), ("Ä", "3"), ("ä", "4")]
    (Sentry.Update.run (Sentry.Request.removeHeader "fOO") updated).headers `shouldBe` Map.fromList [("Ä", "3"), ("ä", "4")]
  it "composes, observes preceding assignments, and preserves unrelated request fields" do
    let event = asEvent (Sentry.Event.setRequest (Sentry.Request.setUrl "keep" <> Sentry.Request.setMethod "first") <> Sentry.Event.modifyRequest [Sentry.Request.setMethod "last", Sentry.Request.with \r -> Sentry.Request.setHeader "method" r.method])
    fmap (.url) event.request `shouldBe` Just "keep"
    fmap (.headers) event.request `shouldBe` Just (Map.singleton "method" "last")
    (asEvent (Sentry.Event.modifyExistingRequest (undefined :: Sentry.RequestUpdate))).request `shouldBe` Nothing
    (Sentry.Event.apply event Sentry.Event.unsetRequest).request `shouldBe` Nothing
  it "sets and clears all OS and App fields" do
    let os = Witch.from [Sentry.OsContext.setName "n", Sentry.OsContext.setVersion "v", Sentry.OsContext.setBuild "b", Sentry.OsContext.setKernelVersion "k", Sentry.OsContext.setRawDescription "r", Sentry.OsContext.setRooted True] :: Sentry.OsContext
    os `shouldBe` Sentry.OsContext.empty{Sentry.OsContext.name = "n", Sentry.OsContext.version = "v", Sentry.OsContext.build = "b", Sentry.OsContext.kernelVersion = "k", Sentry.OsContext.rawDescription = "r", Sentry.OsContext.rooted = Just True}
    (Sentry.Update.run Sentry.OsContext.unsetRooted os).rooted `shouldBe` Nothing
    let time = read "2026-01-01 00:00:00 UTC" :: UTCTime
        app = Witch.from [Sentry.AppContext.setAppBuild "b", Sentry.AppContext.setAppIdentifier "i", Sentry.AppContext.setAppName "n", Sentry.AppContext.setAppVersion "v", Sentry.AppContext.setBuildType "t", Sentry.AppContext.setDeviceAppHash "h", Sentry.AppContext.setAppMemory 42, Sentry.AppContext.setAppStartTime time] :: Sentry.AppContext
    app `shouldBe` Sentry.AppContext.empty{Sentry.AppContext.appBuild = "b", Sentry.AppContext.appIdentifier = "i", Sentry.AppContext.appName = "n", Sentry.AppContext.appVersion = "v", Sentry.AppContext.buildType = "t", Sentry.AppContext.deviceAppHash = "h", Sentry.AppContext.appMemory = Just 42, Sentry.AppContext.appStartTime = Just time}
    let cleared = Sentry.Update.run [Sentry.AppContext.unsetAppMemory, Sentry.AppContext.unsetAppStartTime] app
    cleared.appMemory `shouldBe` Nothing
    cleared.appStartTime `shouldBe` Nothing
  it "replaces typed payloads and edits custom event fields without touching other fields" do
    let event = asEvent (Sentry.Event.setTag "keep" "yes" <> Sentry.Event.setContextValues "os" [("old", Aeson.Null)] <> Sentry.Event.setOsContext [Sentry.OsContext.setName "old", Sentry.OsContext.setName "new"] <> Sentry.Event.setAppContext Sentry.AppContext.empty <> Sentry.Event.setRuntimeContext (Sentry.RuntimeContext.setName "ghc"))
    Map.lookup "os" event.contexts `shouldBe` Just (Sentry.Context.Os Sentry.OsContext.empty{Sentry.OsContext.name = "new"})
    let custom = Sentry.Event.apply event (Sentry.Event.setContextValues "custom" [("a", Aeson.Bool False), ("a", Aeson.Bool True)] <> Sentry.Event.setContextValue "custom" "b" Aeson.Null <> Sentry.Event.removeContextValue "custom" "a" <> Sentry.Event.modifyContextValues "os" undefined)
    Map.lookup "custom" custom.contexts `shouldBe` Just (Sentry.Context.Other (Map.singleton "b" Aeson.Null))
    custom.tags `shouldBe` event.tags
    Map.lookup "os" custom.contexts `shouldBe` Map.lookup "os" event.contexts
    let removed = Sentry.Event.apply custom (Sentry.Event.removeContextValue "custom" "b" <> Sentry.Event.removeContextValue "absent" "x")
    Map.lookup "custom" removed.contexts `shouldBe` Just (Sentry.Context.Other Map.empty)
    Map.lookup "absent" removed.contexts `shouldBe` Nothing
    Map.lookup "new" (Sentry.Event.apply event (Sentry.Event.modifyContextValues "new" (Map.insert "x" Aeson.Null))).contexts `shouldBe` Just (Sentry.Context.Other (Map.singleton "x" Aeson.Null))
  it "forces assigned records and computed maps during the edit" do
    evaluate (asEvent (Sentry.Event.setRequest (undefined :: Sentry.Request))) `shouldThrow` anyErrorCall
    evaluate (asEvent (Sentry.Event.setRequest (Sentry.Request.setUrl undefined))) `shouldThrow` anyErrorCall
    evaluate (asEvent (Sentry.Event.setRequest (Sentry.Request.setEnv "x" undefined))) `shouldThrow` anyErrorCall
    evaluate (asEvent (Sentry.Event.setOsContext (Sentry.OsContext.setRooted undefined))) `shouldThrow` anyErrorCall
    evaluate (asEvent (Sentry.Event.setAppContext (Sentry.AppContext.setAppMemory undefined))) `shouldThrow` anyErrorCall
    evaluate (asEvent (Sentry.Event.modifyContextValues "x" (const undefined))) `shouldThrow` anyErrorCall
    evaluate (Sentry.Update.run (Builders.modifyContextValues "x" (const undefined)) (mempty :: Sentry.ScopeData)) `shouldThrow` anyErrorCall

spec_modifyCreation :: Spec
spec_modifyCreation = describe "optional record modification" do
  it "creates event users, requests, and user geos and forces their assignments" do
    fmap (.id) (asEvent (Sentry.Event.modifyUser (Sentry.User.setId "new"))).user `shouldBe` Just "new"
    fmap (.url) (asEvent (Sentry.Event.modifyRequest [Sentry.Request.setUrl "new"])).request `shouldBe` Just "new"
    fmap (.city) (asUser (Sentry.User.modifyGeo (Sentry.Geo.setCity "Boston"))).geo `shouldBe` Just "Boston"
    evaluate (asEvent (Sentry.Event.modifyUser (Sentry.User.setId undefined))) `shouldThrow` anyErrorCall
    evaluate (asEvent (Sentry.Event.modifyRequest (Sentry.Request.setUrl undefined))) `shouldThrow` anyErrorCall
    evaluate (asUser (Sentry.User.modifyGeo (Sentry.Geo.setCity undefined))) `shouldThrow` anyErrorCall
  it "forces a newly created scope user and leaves failed edits atomic" do
    scope <- Scope.create Scope.Current
    Sentry.modifyUser scope (Sentry.User.setId undefined) `shouldThrow` anyErrorCall
    result <- Scope.readScopeRef scope
    result.user `shouldBe` Nothing
  it "modifies existing nested records while preserving other fields" do
    fmap (.name) (asEvent (Sentry.Event.setUser (Sentry.User.setName "keep") <> Sentry.Event.modifyExistingUser (Sentry.User.setId "new"))).user `shouldBe` Just "keep"
    fmap (.url) (asEvent (Sentry.Event.setRequest (Sentry.Request.setUrl "keep") <> Sentry.Event.modifyExistingRequest (Sentry.Request.setMethod "POST"))).request `shouldBe` Just "keep"
    fmap (.city) (asUser (Sentry.User.setGeo (Sentry.Geo.setCity "keep") <> Sentry.User.modifyExistingGeo (Sentry.Geo.setRegion "new"))).geo `shouldBe` Just "keep"
    userAfter (seeded <> Builders.modifyExistingUser (Sentry.User.setName "new")) `shouldBe` Just Sentry.User.empty{Sentry.User.id = "123", Sentry.User.email = "old", Sentry.User.name = "new"}
  it "creates local metadata without copying inherited user or context fields" do
    (_, transport) <- Test.withClient \_ ->
      Sentry.withIsolationScope \outer -> do
        Sentry.updateScope outer [Builders.setUser (Sentry.User.setId "inherited"), Builders.setOsContext (Sentry.OsContext.setVersion "inherited")]
        Sentry.withScope \inner -> do
          Sentry.updateScope inner [Builders.modifyUser (Sentry.User.setName "local"), Builders.modifyOsContext (Sentry.OsContext.setName "local")]
          Sentry.captureMessage_ Sentry.Info "local"
    events <- Test.fetchAndClearEvents transport
    fmap (.user) events `shouldBe` [Just Sentry.User.empty{Sentry.User.name = "local"}]
    fmap (Map.lookup "os" . (.contexts)) events `shouldBe` [Just (Sentry.Context.Os Sentry.OsContext.empty{Sentry.OsContext.name = "local"})]

spec_typedContextModification :: Spec
spec_typedContextModification = describe "typed context modification" do
  it "Event os: creates, preserves, composes, skips mismatches, and forces edits" do
    let run = asEvent
        create upd = Sentry.Event.modifyOsContext upd
        existing upd = Sentry.Event.modifyExistingOsContext upd
        original = Sentry.Event.setOsContext (Sentry.OsContext.setVersion "keep")
        expected = Sentry.Context.Os Sentry.OsContext.empty{Sentry.OsContext.name = "new", Sentry.OsContext.version = "keep"}
    Map.lookup "os" (run (create (Sentry.OsContext.setName "new"))).contexts `shouldBe` Just (Sentry.Context.Os Sentry.OsContext.empty{Sentry.OsContext.name = "new"})
    for_ [create, existing] \modify -> do
      Map.lookup "os" (run (original <> modify [Sentry.OsContext.setName "old", Sentry.OsContext.setName "new"])).contexts `shouldBe` Just expected
      let mismatch = Sentry.Event.setContextValues "os" [("keep", Aeson.Null)]
      (run (mismatch <> modify (undefined :: [Sentry.OsContext.OsContextUpdate]))).contexts `shouldBe` (run mismatch).contexts
      evaluate (run (original <> modify [Sentry.OsContext.setName undefined])) `shouldThrow` anyErrorCall
    (run (existing (undefined :: Sentry.OsContext.OsContextUpdate))).contexts `shouldBe` Map.empty
    evaluate (run (create (Sentry.OsContext.setName undefined))) `shouldThrow` anyErrorCall
  it "Scope os: creates, preserves, composes, skips mismatches, and forces edits" do
    let run upd = Sentry.Update.run upd (mempty :: Sentry.ScopeData)
        create upd = Builders.modifyOsContext upd
        existing upd = Builders.modifyExistingOsContext upd
        original = Builders.setOsContext (Sentry.OsContext.setVersion "keep")
        expected = Sentry.Context.Os Sentry.OsContext.empty{Sentry.OsContext.name = "new", Sentry.OsContext.version = "keep"}
    Map.lookup "os" (run (create (Sentry.OsContext.setName "new"))).contexts `shouldBe` Just (Sentry.Context.Os Sentry.OsContext.empty{Sentry.OsContext.name = "new"})
    for_ [create, existing] \modify -> do
      Map.lookup "os" (run (original <> modify [Sentry.OsContext.setName "old", Sentry.OsContext.setName "new"])).contexts `shouldBe` Just expected
      let mismatch = Builders.setContextValues "os" [("keep", Aeson.Null)]
      (run (mismatch <> modify (undefined :: [Sentry.OsContext.OsContextUpdate]))).contexts `shouldBe` (run mismatch).contexts
      evaluate (run (original <> modify [Sentry.OsContext.setName undefined])) `shouldThrow` anyErrorCall
    (run (existing (undefined :: Sentry.OsContext.OsContextUpdate))).contexts `shouldBe` Map.empty
    evaluate (run (create (Sentry.OsContext.setName undefined))) `shouldThrow` anyErrorCall
  it "Event app: creates, preserves, composes, skips mismatches, and forces edits" do
    let run = asEvent
        create upd = Sentry.Event.modifyAppContext upd
        existing upd = Sentry.Event.modifyExistingAppContext upd
        original = Sentry.Event.setAppContext (Sentry.AppContext.setAppVersion "keep")
        expected = Sentry.Context.App Sentry.AppContext.empty{Sentry.AppContext.appName = "new", Sentry.AppContext.appVersion = "keep"}
    Map.lookup "app" (run (create (Sentry.AppContext.setAppName "new"))).contexts `shouldBe` Just (Sentry.Context.App Sentry.AppContext.empty{Sentry.AppContext.appName = "new"})
    for_ [create, existing] \modify -> do
      Map.lookup "app" (run (original <> modify [Sentry.AppContext.setAppName "old", Sentry.AppContext.setAppName "new"])).contexts `shouldBe` Just expected
      let mismatch = Sentry.Event.setContextValues "app" [("keep", Aeson.Null)]
      (run (mismatch <> modify (undefined :: [Sentry.AppContext.AppContextUpdate]))).contexts `shouldBe` (run mismatch).contexts
      evaluate (run (original <> modify [Sentry.AppContext.setAppName undefined])) `shouldThrow` anyErrorCall
    (run (existing (undefined :: Sentry.AppContext.AppContextUpdate))).contexts `shouldBe` Map.empty
    evaluate (run (create (Sentry.AppContext.setAppName undefined))) `shouldThrow` anyErrorCall
  it "Scope app: creates, preserves, composes, skips mismatches, and forces edits" do
    let run upd = Sentry.Update.run upd (mempty :: Sentry.ScopeData)
        create upd = Builders.modifyAppContext upd
        existing upd = Builders.modifyExistingAppContext upd
        original = Builders.setAppContext (Sentry.AppContext.setAppVersion "keep")
        expected = Sentry.Context.App Sentry.AppContext.empty{Sentry.AppContext.appName = "new", Sentry.AppContext.appVersion = "keep"}
    Map.lookup "app" (run (create (Sentry.AppContext.setAppName "new"))).contexts `shouldBe` Just (Sentry.Context.App Sentry.AppContext.empty{Sentry.AppContext.appName = "new"})
    for_ [create, existing] \modify -> do
      Map.lookup "app" (run (original <> modify [Sentry.AppContext.setAppName "old", Sentry.AppContext.setAppName "new"])).contexts `shouldBe` Just expected
      let mismatch = Builders.setContextValues "app" [("keep", Aeson.Null)]
      (run (mismatch <> modify (undefined :: [Sentry.AppContext.AppContextUpdate]))).contexts `shouldBe` (run mismatch).contexts
      evaluate (run (original <> modify [Sentry.AppContext.setAppName undefined])) `shouldThrow` anyErrorCall
    (run (existing (undefined :: Sentry.AppContext.AppContextUpdate))).contexts `shouldBe` Map.empty
    evaluate (run (create (Sentry.AppContext.setAppName undefined))) `shouldThrow` anyErrorCall
  it "Event runtime: creates, preserves, composes, skips mismatches, and forces edits" do
    let run = asEvent
        create upd = Sentry.Event.modifyRuntimeContext upd
        existing upd = Sentry.Event.modifyExistingRuntimeContext upd
        original = Sentry.Event.setRuntimeContext (Sentry.RuntimeContext.setVersion "keep")
        expected = Sentry.Context.Runtime Sentry.RuntimeContext.empty{Sentry.RuntimeContext.name = "new", Sentry.RuntimeContext.version = "keep"}
    Map.lookup "runtime" (run (create (Sentry.RuntimeContext.setName "new"))).contexts `shouldBe` Just (Sentry.Context.Runtime Sentry.RuntimeContext.empty{Sentry.RuntimeContext.name = "new"})
    for_ [create, existing] \modify -> do
      Map.lookup "runtime" (run (original <> modify [Sentry.RuntimeContext.setName "old", Sentry.RuntimeContext.setName "new"])).contexts `shouldBe` Just expected
      let mismatch = Sentry.Event.setContextValues "runtime" [("keep", Aeson.Null)]
      (run (mismatch <> modify (undefined :: [Sentry.RuntimeContext.RuntimeContextUpdate]))).contexts `shouldBe` (run mismatch).contexts
      evaluate (run (original <> modify [Sentry.RuntimeContext.setName undefined])) `shouldThrow` anyErrorCall
    (run (existing (undefined :: Sentry.RuntimeContext.RuntimeContextUpdate))).contexts `shouldBe` Map.empty
    evaluate (run (create (Sentry.RuntimeContext.setName undefined))) `shouldThrow` anyErrorCall
  it "Scope runtime: creates, preserves, composes, skips mismatches, and forces edits" do
    let run upd = Sentry.Update.run upd (mempty :: Sentry.ScopeData)
        create upd = Builders.modifyRuntimeContext upd
        existing upd = Builders.modifyExistingRuntimeContext upd
        original = Builders.setRuntimeContext (Sentry.RuntimeContext.setVersion "keep")
        expected = Sentry.Context.Runtime Sentry.RuntimeContext.empty{Sentry.RuntimeContext.name = "new", Sentry.RuntimeContext.version = "keep"}
    Map.lookup "runtime" (run (create (Sentry.RuntimeContext.setName "new"))).contexts `shouldBe` Just (Sentry.Context.Runtime Sentry.RuntimeContext.empty{Sentry.RuntimeContext.name = "new"})
    for_ [create, existing] \modify -> do
      Map.lookup "runtime" (run (original <> modify [Sentry.RuntimeContext.setName "old", Sentry.RuntimeContext.setName "new"])).contexts `shouldBe` Just expected
      let mismatch = Builders.setContextValues "runtime" [("keep", Aeson.Null)]
      (run (mismatch <> modify (undefined :: [Sentry.RuntimeContext.RuntimeContextUpdate]))).contexts `shouldBe` (run mismatch).contexts
      evaluate (run (original <> modify [Sentry.RuntimeContext.setName undefined])) `shouldThrow` anyErrorCall
    (run (existing (undefined :: Sentry.RuntimeContext.RuntimeContextUpdate))).contexts `shouldBe` Map.empty
    evaluate (run (create (Sentry.RuntimeContext.setName undefined))) `shouldThrow` anyErrorCall

  it "Event browser: creates, preserves, composes, skips mismatches, and forces edits" do
    let run = asEvent
        create upd = Sentry.Event.modifyBrowserContext upd
        existing upd = Sentry.Event.modifyExistingBrowserContext upd
        original = Sentry.Event.setBrowserContext (Sentry.BrowserContext.setVersion "keep")
        expected = Sentry.Context.Browser Sentry.BrowserContext.empty{Sentry.BrowserContext.name = "new", Sentry.BrowserContext.version = "keep"}
    Map.lookup "browser" (run (create (Sentry.BrowserContext.setName "new"))).contexts `shouldBe` Just (Sentry.Context.Browser Sentry.BrowserContext.empty{Sentry.BrowserContext.name = "new"})
    for_ [create, existing] \modify -> do
      Map.lookup "browser" (run (original <> modify [Sentry.BrowserContext.setName "old", Sentry.BrowserContext.setName "new"])).contexts `shouldBe` Just expected
      let mismatch = Sentry.Event.setContextValues "browser" [("keep", Aeson.Null)]
      (run (mismatch <> modify (undefined :: [Sentry.BrowserContext.BrowserContextUpdate]))).contexts `shouldBe` (run mismatch).contexts
      evaluate (run (original <> modify [Sentry.BrowserContext.setName undefined])) `shouldThrow` anyErrorCall
    (run (existing (undefined :: Sentry.BrowserContext.BrowserContextUpdate))).contexts `shouldBe` Map.empty
    evaluate (run (create (Sentry.BrowserContext.setName undefined))) `shouldThrow` anyErrorCall
  it "Scope browser: creates, preserves, composes, skips mismatches, and forces edits" do
    let run upd = Sentry.Update.run upd (mempty :: Sentry.ScopeData)
        create upd = Builders.modifyBrowserContext upd
        existing upd = Builders.modifyExistingBrowserContext upd
        original = Builders.setBrowserContext (Sentry.BrowserContext.setVersion "keep")
        expected = Sentry.Context.Browser Sentry.BrowserContext.empty{Sentry.BrowserContext.name = "new", Sentry.BrowserContext.version = "keep"}
    Map.lookup "browser" (run (create (Sentry.BrowserContext.setName "new"))).contexts `shouldBe` Just (Sentry.Context.Browser Sentry.BrowserContext.empty{Sentry.BrowserContext.name = "new"})
    for_ [create, existing] \modify -> do
      Map.lookup "browser" (run (original <> modify [Sentry.BrowserContext.setName "old", Sentry.BrowserContext.setName "new"])).contexts `shouldBe` Just expected
      let mismatch = Builders.setContextValues "browser" [("keep", Aeson.Null)]
      (run (mismatch <> modify (undefined :: [Sentry.BrowserContext.BrowserContextUpdate]))).contexts `shouldBe` (run mismatch).contexts
      evaluate (run (original <> modify [Sentry.BrowserContext.setName undefined])) `shouldThrow` anyErrorCall
    (run (existing (undefined :: Sentry.BrowserContext.BrowserContextUpdate))).contexts `shouldBe` Map.empty
    evaluate (run (create (Sentry.BrowserContext.setName undefined))) `shouldThrow` anyErrorCall
  it "browser setters accept records and replace mismatched variants" do
    let record = Witch.from [Sentry.BrowserContext.setName "new"] :: Sentry.BrowserContext.BrowserContext
        replacement = Witch.from record :: Sentry.BrowserContext.BrowserContextUpdate
        expected = Just (Sentry.Context.Browser record)
    Map.lookup "browser" (asEvent (Sentry.Event.setContextValues "browser" [] <> Sentry.Event.setBrowserContext replacement)).contexts `shouldBe` expected
    Map.lookup "browser" (Sentry.Update.run (Builders.setContextValues "browser" [] <> Builders.setBrowserContext record) (mempty :: Sentry.ScopeData)).contexts `shouldBe` expected

  it "Event device: creates, preserves, composes, skips mismatches, and forces edits" do
    let run = asEvent
        create upd = Sentry.Event.modifyDeviceContext upd
        existing upd = Sentry.Event.modifyExistingDeviceContext upd
        original = Sentry.Event.setDeviceContext (Sentry.DeviceContext.setName "keep")
        expected = Sentry.Context.Device Sentry.DeviceContext.empty{Sentry.DeviceContext.model = "new", Sentry.DeviceContext.name = "keep"}
    Map.lookup "device" (run (create (Sentry.DeviceContext.setModel "new"))).contexts `shouldBe` Just (Sentry.Context.Device Sentry.DeviceContext.empty{Sentry.DeviceContext.model = "new"})
    for_ [create, existing] \modify -> do
      Map.lookup "device" (run (original <> modify [Sentry.DeviceContext.setModel "old", Sentry.DeviceContext.setModel "new"])).contexts `shouldBe` Just expected
      let mismatch = Sentry.Event.setContextValues "device" [("keep", Aeson.Null)]
      (run (mismatch <> modify (undefined :: [Sentry.DeviceContext.DeviceContextUpdate]))).contexts `shouldBe` (run mismatch).contexts
      evaluate (run (original <> modify [Sentry.DeviceContext.setModel undefined])) `shouldThrow` anyErrorCall
    (run (existing (undefined :: Sentry.DeviceContext.DeviceContextUpdate))).contexts `shouldBe` Map.empty
    evaluate (run (create (Sentry.DeviceContext.setModel undefined))) `shouldThrow` anyErrorCall
  it "Scope device: creates, preserves, composes, skips mismatches, and forces edits" do
    let run upd = Sentry.Update.run upd (mempty :: Sentry.ScopeData)
        create upd = Builders.modifyDeviceContext upd
        existing upd = Builders.modifyExistingDeviceContext upd
        original = Builders.setDeviceContext (Sentry.DeviceContext.setName "keep")
        expected = Sentry.Context.Device Sentry.DeviceContext.empty{Sentry.DeviceContext.model = "new", Sentry.DeviceContext.name = "keep"}
    Map.lookup "device" (run (create (Sentry.DeviceContext.setModel "new"))).contexts `shouldBe` Just (Sentry.Context.Device Sentry.DeviceContext.empty{Sentry.DeviceContext.model = "new"})
    for_ [create, existing] \modify -> do
      Map.lookup "device" (run (original <> modify [Sentry.DeviceContext.setModel "old", Sentry.DeviceContext.setModel "new"])).contexts `shouldBe` Just expected
      let mismatch = Builders.setContextValues "device" [("keep", Aeson.Null)]
      (run (mismatch <> modify (undefined :: [Sentry.DeviceContext.DeviceContextUpdate]))).contexts `shouldBe` (run mismatch).contexts
      evaluate (run (original <> modify [Sentry.DeviceContext.setModel undefined])) `shouldThrow` anyErrorCall
    (run (existing (undefined :: Sentry.DeviceContext.DeviceContextUpdate))).contexts `shouldBe` Map.empty
    evaluate (run (create (Sentry.DeviceContext.setModel undefined))) `shouldThrow` anyErrorCall
  it "device setters accept records and replace mismatched variants" do
    let record = Witch.from [Sentry.DeviceContext.setModel "new"] :: Sentry.DeviceContext.DeviceContext
        replacement = Witch.from record :: Sentry.DeviceContext.DeviceContextUpdate
        expected = Just (Sentry.Context.Device record)
    Map.lookup "device" (asEvent (Sentry.Event.setContextValues "device" [] <> Sentry.Event.setDeviceContext replacement)).contexts `shouldBe` expected
    Map.lookup "device" (Sentry.Update.run (Builders.setContextValues "device" [] <> Builders.setDeviceContext record) (mempty :: Sentry.ScopeData)).contexts `shouldBe` expected

  it "Event trace: creates, preserves, composes, skips mismatches, and forces edits" do
    let run = asEvent
        create upd = Sentry.Event.modifyTraceContext upd
        existing upd = Sentry.Event.modifyExistingTraceContext upd
        original = Sentry.Event.setTraceContext (Sentry.TraceContext.setTraceId "keep")
        expected = Sentry.Context.Trace Sentry.TraceContext.empty{Sentry.TraceContext.spanId = "new", Sentry.TraceContext.traceId = "keep"}
    Map.lookup "trace" (run (create (Sentry.TraceContext.setSpanId "new"))).contexts `shouldBe` Just (Sentry.Context.Trace Sentry.TraceContext.empty{Sentry.TraceContext.spanId = "new"})
    for_ [create, existing] \modify -> do
      Map.lookup "trace" (run (original <> modify [Sentry.TraceContext.setSpanId "old", Sentry.TraceContext.setSpanId "new"])).contexts `shouldBe` Just expected
      let mismatch = Sentry.Event.setContextValues "trace" [("keep", Aeson.Null)]
      (run (mismatch <> modify (undefined :: [Sentry.TraceContext.TraceContextUpdate]))).contexts `shouldBe` (run mismatch).contexts
      evaluate (run (original <> modify [Sentry.TraceContext.setSpanId undefined])) `shouldThrow` anyErrorCall
    (run (existing (undefined :: Sentry.TraceContext.TraceContextUpdate))).contexts `shouldBe` Map.empty
    evaluate (run (create (Sentry.TraceContext.setSpanId undefined))) `shouldThrow` anyErrorCall
  it "Scope trace: creates, preserves, composes, skips mismatches, and forces edits" do
    let run upd = Sentry.Update.run upd (mempty :: Sentry.ScopeData)
        create upd = Builders.modifyTraceContext upd
        existing upd = Builders.modifyExistingTraceContext upd
        original = Builders.setTraceContext (Sentry.TraceContext.setTraceId "keep")
        expected = Sentry.Context.Trace Sentry.TraceContext.empty{Sentry.TraceContext.spanId = "new", Sentry.TraceContext.traceId = "keep"}
    Map.lookup "trace" (run (create (Sentry.TraceContext.setSpanId "new"))).contexts `shouldBe` Just (Sentry.Context.Trace Sentry.TraceContext.empty{Sentry.TraceContext.spanId = "new"})
    for_ [create, existing] \modify -> do
      Map.lookup "trace" (run (original <> modify [Sentry.TraceContext.setSpanId "old", Sentry.TraceContext.setSpanId "new"])).contexts `shouldBe` Just expected
      let mismatch = Builders.setContextValues "trace" [("keep", Aeson.Null)]
      (run (mismatch <> modify (undefined :: [Sentry.TraceContext.TraceContextUpdate]))).contexts `shouldBe` (run mismatch).contexts
      evaluate (run (original <> modify [Sentry.TraceContext.setSpanId undefined])) `shouldThrow` anyErrorCall
    (run (existing (undefined :: Sentry.TraceContext.TraceContextUpdate))).contexts `shouldBe` Map.empty
    evaluate (run (create (Sentry.TraceContext.setSpanId undefined))) `shouldThrow` anyErrorCall
  it "trace setters accept records and replace mismatched variants" do
    let record = Witch.from [Sentry.TraceContext.setSpanId "new"] :: Sentry.TraceContext.TraceContext
        replacement = Witch.from record :: Sentry.TraceContext.TraceContextUpdate
        expected = Just (Sentry.Context.Trace record)
    Map.lookup "trace" (asEvent (Sentry.Event.setContextValues "trace" [] <> Sentry.Event.setTraceContext replacement)).contexts `shouldBe` expected
    Map.lookup "trace" (Sentry.Update.run (Builders.setContextValues "trace" [] <> Builders.setTraceContext record) (mempty :: Sentry.ScopeData)).contexts `shouldBe` expected
