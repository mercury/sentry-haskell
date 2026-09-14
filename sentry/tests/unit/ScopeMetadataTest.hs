-- | The public story, exercised end to end.
--
-- Everything here goes through the @sentry@ facade and the per-record builder
-- modules. There is deliberately __no @patrol@ import__: if a builder is
-- missing, or if a record's fields stop being readable through its own module,
-- this file fails to compile. That is the point of the test.
module ScopeMetadataTest where

import Control.Exception (toException)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Default (def)
import Data.Foldable (for_, toList)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Sentry qualified
import Sentry.AppContext qualified
import Sentry.Breadcrumb qualified
import Sentry.Context qualified
import Sentry.Event qualified
import Sentry.Geo qualified
import Sentry.OsContext qualified
import Sentry.Request qualified
import Sentry.RuntimeContext qualified
import Sentry.Scope qualified
import Sentry.Test qualified as Test
import Sentry.User qualified
import Test.Hspec

-- | A reusable user update shared across checkout handlers.
betaCohort :: Sentry.UserUpdate
betaCohort = Sentry.User.setData "cohort" (Aeson.String "beta")

-- | Set scope metadata using nested record updates through "Sentry".
setCheckoutMetadata :: Sentry.Scope -> IO ()
setCheckoutMetadata scope = do
  Sentry.updateScope
    scope
    [ Sentry.Scope.setUser
        [ Sentry.User.setId "user-42",
          Sentry.User.setName "Alice",
          Sentry.User.setEmail "alice@example.com",
          Sentry.User.setGeo [Sentry.Geo.setCity "Detroit", Sentry.Geo.setCountryCode "US"]
        ],
      Sentry.Scope.setTag "feature" "checkout"
    ]
  Sentry.modifyUser scope $ Sentry.User.with \user ->
    [ Sentry.User.setData "display_name" (Aeson.String user.name),
      betaCohort,
      Sentry.User.modifyGeo $ Sentry.Geo.with \geo ->
        if geo.countryCode == "US" && geo.city == "Detroit"
          then Sentry.Geo.setRegion "Michigan"
          else mempty
    ]

-- | A @beforeSend@ that scrubs, written the way an application author would:
-- read the in-flight event, return its modified record, never touch @patrol@.
scrub :: Sentry.CapturedEvent -> Maybe Sentry.Event
scrub ce =
  Just $
    Sentry.Event.apply
      ce.event
      [ Sentry.Event.setTag "kind" (if isJust ce.exception then "exception" else "message"),
        Sentry.Event.removeExtra "authorization",
        Sentry.Event.modifyUser (Sentry.User.setEmail ""),
        Sentry.Event.with \event ->
          Sentry.Event.addFingerprint (if Map.member "kind" event.tags then "tagged" else "bare")
      ]

spec_scopeMetadata :: Spec
spec_scopeMetadata = describe "scope metadata through Sentry" do
  it "composes scope metadata updates and reads fields back with record-dot" do
    scope <- Sentry.Scope.create Sentry.Scope.Current
    setCheckoutMetadata scope
    result <- Sentry.Scope.readScopeRef scope
    fmap (.name) result.user `shouldBe` Just "Alice"
    fmap (.data_) result.user
      `shouldBe` Just
        ( Map.fromList
            [ ("cohort", Aeson.String "beta"),
              ("display_name", Aeson.String "Alice")
            ]
        )
    fmap (.region) (result.user >>= (.geo)) `shouldBe` Just "Michigan"

spec_beforeSendFacade :: Spec
spec_beforeSendFacade = describe "beforeSend returning an Event" do
  it "applies the update to the delivered event" do
    (_, transport) <- Test.withCustomClient def{Sentry.beforeSend = Just scrub} \_ ->
      Sentry.withIsolationScope \scope -> do
        setCheckoutMetadata scope
        Sentry.setExtra scope "authorization" (Aeson.String "Bearer hunter2")
        Sentry.captureMessage_ Sentry.Info "checkout submitted"
    events <- Test.fetchAndClearEvents transport
    case events of
      [event] -> do
        -- The scrub added the tag and removed the authorization extra and email.
        Map.lookup "kind" event.tags `shouldBe` Just "message"
        Map.lookup "authorization" event.extra `shouldBe` Nothing
        fmap (.email) event.user `shouldBe` Just ""
        fmap (.id) event.user `shouldBe` Just "user-42"
        -- Scope metadata survived the scrub.
        fmap (.name) event.user `shouldBe` Just "Alice"
        -- 'with' saw the tag assigned earlier in the same update.
        event.fingerprint `shouldBe` ["tagged"]
      _ -> expectationFailure ("expected exactly one event, got " <> show (length events))

spec_mergedScrubbing :: Spec
spec_mergedScrubbing = describe "scoped record-returning processors" do
  it "scrubs effective inherited metadata without mutating the outer scope" do
    (_, transport) <- Test.withClient \_ ->
      Sentry.withIsolationScope \outer -> do
        Sentry.updateScope
          outer
          [ Sentry.Scope.setUser [Sentry.User.setId "42", Sentry.User.setEmail "alice@example.com"],
            Sentry.Scope.setContextValues "account" [("secret", Aeson.String "token"), ("safe", Aeson.Bool True)]
          ]
        Sentry.withScope \inner -> do
          Sentry.updateScope inner $ Sentry.Scope.addEventProcessor \ce ->
            Just
              ( Sentry.Event.apply
                  ce.event
                  (Sentry.Event.modifyUser (Sentry.User.setEmail "") <> Sentry.Event.removeContextValue "account" "secret")
              )
          Sentry.captureMessage_ Sentry.Info "scrubbed"
        Sentry.captureMessage_ Sentry.Info "outer unchanged"
    events <- Test.fetchAndClearEvents transport
    fmap (fmap (.email) . (.user)) events `shouldBe` [Just "", Just "alice@example.com"]
    fmap (fmap (.id) . (.user)) events `shouldBe` [Just "42", Just "42"]
    fmap (Map.lookup "account" . (.contexts)) events
      `shouldBe` [ Just (Sentry.Context.Other (Map.singleton "safe" (Aeson.Bool True))),
                   Just (Sentry.Context.Other (Map.fromList [("secret", Aeson.String "token"), ("safe", Aeson.Bool True)]))
                 ]

spec_breadcrumbFacade :: Spec
spec_breadcrumbFacade = describe "record-returning breadcrumb hooks" do
  it "applies builders and record updates to policy-defaulted breadcrumbs" do
    let refine crumb =
          Just
            ( Sentry.Breadcrumb.apply
                crumb{Sentry.Breadcrumb.message = crumb.message <> " processed"}
                (Sentry.Breadcrumb.setData "kept" (Aeson.Bool True))
            )
    (snapshot, _) <- Test.withCustomClient def{Sentry.beforeBreadcrumb = Just refine, Sentry.maxBreadcrumbs = 1} \_ ->
      Sentry.withIsolationScope \scope -> do
        Sentry.addBreadcrumb [Sentry.Breadcrumb.setMessage "first", Sentry.Breadcrumb.setCategory "ui"]
        Sentry.addBreadcrumb (Sentry.Breadcrumb.setMessage "second")
        Sentry.Scope.readScopeRef scope
    case toList snapshot.breadcrumbs of
      [crumb] -> do
        crumb.message `shouldBe` "second processed"
        crumb.timestamp `shouldSatisfy` isJust
        Map.lookup "kept" crumb.data_ `shouldBe` Just (Aeson.Bool True)
      _ -> expectationFailure "expected retention to keep one breadcrumb"

  it "keeps pure breadcrumb edits free of ambient policy" do
    (snapshot, _) <- Test.withCustomClient def{Sentry.beforeBreadcrumb = Just (const Nothing), Sentry.maxBreadcrumbs = 0} \_ ->
      Sentry.withIsolationScope \scope -> do
        Sentry.updateScope scope (Sentry.Scope.addBreadcrumb (Sentry.Breadcrumb.setMessage "raw"))
        Sentry.Scope.readScopeRef scope
    fmap (.timestamp) (toList snapshot.breadcrumbs) `shouldBe` [Nothing]

spec_eventConstructorDefaults :: Spec
spec_eventConstructorDefaults = describe "event constructors preserve client defaults" do
  for_
    [ ("empty", Sentry.Event.empty),
      ("fromMessage", Sentry.Event.fromMessage Sentry.Info "message"),
      ("fromException", Sentry.Event.fromException (toException (userError "failure"))),
      ("fromExceptionWith", Sentry.Event.fromExceptionWith Nothing (toException (userError "failure")))
    ]
    \(label, base) ->
      it (label <> " uses the configured environment and SDK identity") do
        let options = def{Sentry.environment = Just "staging", Sentry.defaultIntegrations = False}
        (_, transport) <- Test.withCustomClient options \_ ->
          Sentry.captureEvent_ (Sentry.Event.apply base (Sentry.Event.setTag "constructor" "tested"))
        events <- Test.fetchAndClearEvents transport
        case events of
          [event] -> do
            event.environment `shouldBe` "staging"
            Map.lookup "constructor" event.tags `shouldBe` Just "tested"
            case Aeson.toJSON event.sdk of
              Aeson.Object sdk -> KeyMap.lookup "name" sdk `shouldBe` Just (Aeson.String "sentry.haskell")
              other -> expectationFailure ("expected SDK metadata, got " <> show other)
          _ -> expectationFailure ("expected one event, got " <> show (length events))

spec_nestedRecords :: Spec
spec_nestedRecords = describe "nested public record builders" do
  it "sets typed scopes and reads nested requests without Patrol imports" do
    scope <- Sentry.Scope.create Sentry.Scope.Current
    Sentry.setOsContext scope (Sentry.OsContext.setName "Linux")
    Sentry.setAppContext scope [Sentry.AppContext.setAppName "checkout"]
    snapshot <- Sentry.Scope.readScopeRef scope
    Map.lookup "os" snapshot.contexts `shouldBe` Just (Sentry.Context.Os Sentry.OsContext.empty{Sentry.OsContext.name = "Linux"})
    Map.lookup "app" snapshot.contexts `shouldBe` Just (Sentry.Context.App Sentry.AppContext.empty{Sentry.AppContext.appName = "checkout"})
    let event =
          Sentry.Event.apply
            Sentry.Event.empty
            ( Sentry.Event.setRequest [Sentry.Request.setMethod "POST", Sentry.Request.with \r -> Sentry.Request.setHeader "Method" r.method]
                <> Sentry.Event.setRuntimeContext (Sentry.RuntimeContext.setName "ghc")
            )
    fmap (.headers) event.request `shouldBe` Just (Map.singleton "Method" "POST")
