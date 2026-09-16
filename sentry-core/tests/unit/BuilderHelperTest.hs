module BuilderHelperTest where

import Control.Concurrent (forkFinally, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (evaluate, throwIO)
import Control.Monad (forM_, replicateM, void)
import Data.Aeson qualified as Aeson
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Context.ThreadLocal qualified as ThreadLocal
import Patrol.Type.EventProcessingError qualified as ProcessingError
import Sentry.AppContext qualified as App
import Sentry.Breadcrumb qualified as B
import Sentry.BrowserContext qualified as Browser
import Sentry.Client (pattern NON_RECORDING_CLIENT)
import Sentry.Context qualified as C
import Sentry.Core qualified as Sentry
import Sentry.DeviceContext qualified as Device
import Sentry.Event qualified as E
import Sentry.Exception qualified as X
import Sentry.Mechanism qualified as M
import Sentry.OsContext qualified as Os
import Sentry.Request qualified as R
import Sentry.RuntimeContext qualified as Runtime
import Sentry.Scope qualified as S
import Sentry.Scope.Operations qualified as Ops
import Sentry.Test qualified as Test
import Sentry.TraceContext qualified as Trace
import Sentry.Update qualified as U
import Sentry.User qualified as User
import Test.Hspec

-- Exercise both public surfaces against the same presence and mismatch contract.
typedSpec :: (Eq a, Show a) => String -> Text -> a -> r -> (r -> Map Text C.Context) -> (Text -> C.Context -> U.Update r) -> (r -> Maybe a) -> ((Maybe a -> Maybe a) -> U.Update r) -> Spec
typedSpec label key value initial contexts setContext lookupTyped alter = describe label do
  it "looks up, creates, preserves and removes matching payloads" do
    lookupTyped initial `shouldBe` Nothing
    let created = U.run (alter (const (Just value))) initial
    lookupTyped created `shouldBe` Just value
    lookupTyped (U.run (alter id) created) `shouldBe` Just value
    Map.lookup key (contexts (U.run (alter (const Nothing)) created)) `shouldBe` Nothing
    Map.lookup key (contexts (U.run (alter (const Nothing)) initial)) `shouldBe` Nothing
  it "preserves mismatches without evaluating callbacks" do
    let mismatched = U.run (setContext key (C.Other Map.empty)) initial
    lookupTyped mismatched `shouldBe` Nothing
    contexts (U.run (alter (error "unused callback")) mismatched) `shouldBe` contexts mismatched
  it "forces assigned records during the edit" do
    evaluate (U.run (alter (const (Just (error "record")))) initial) `shouldThrow` anyErrorCall

spec_typedContexts :: Spec
spec_typedContexts = describe "typed context helpers" do
  typedSpec "S.App" "app" App.empty mempty (.contexts) S.setContext S.lookupAppContext S.alterAppContext
  typedSpec "S.Os" "os" Os.empty mempty (.contexts) S.setContext S.lookupOsContext S.alterOsContext
  typedSpec "S.Runtime" "runtime" Runtime.empty mempty (.contexts) S.setContext S.lookupRuntimeContext S.alterRuntimeContext
  typedSpec "S.Browser" "browser" Browser.empty mempty (.contexts) S.setContext S.lookupBrowserContext S.alterBrowserContext
  typedSpec "S.Device" "device" Device.empty mempty (.contexts) S.setContext S.lookupDeviceContext S.alterDeviceContext
  typedSpec "S.Trace" "trace" Trace.empty mempty (.contexts) S.setContext S.lookupTraceContext S.alterTraceContext
  typedSpec "E.App" "app" App.empty E.empty (.contexts) E.setContext E.lookupAppContext E.alterAppContext
  typedSpec "E.Os" "os" Os.empty E.empty (.contexts) E.setContext E.lookupOsContext E.alterOsContext
  typedSpec "E.Runtime" "runtime" Runtime.empty E.empty (.contexts) E.setContext E.lookupRuntimeContext E.alterRuntimeContext
  typedSpec "E.Browser" "browser" Browser.empty E.empty (.contexts) E.setContext E.lookupBrowserContext E.alterBrowserContext
  typedSpec "E.Device" "device" Device.empty E.empty (.contexts) E.setContext E.lookupDeviceContext E.alterDeviceContext
  typedSpec "E.Trace" "trace" Trace.empty E.empty (.contexts) E.setContext E.lookupTraceContext E.alterTraceContext

customSpec :: String -> r -> (r -> Map Text C.Context) -> (Text -> C.Context -> U.Update r) -> (Text -> Text -> r -> Maybe Aeson.Value) -> (Text -> Text -> (Maybe Aeson.Value -> Maybe Aeson.Value) -> U.Update r) -> (Text -> Text -> (Aeson.Value -> Aeson.Value) -> U.Update r) -> Spec
customSpec label initial contexts setContext lookupValue alter modify = describe label do
  it "creates, transforms and removes fields while retaining the final empty context" do
    lookupValue "custom" "key" initial `shouldBe` Nothing
    let created = U.run (alter "custom" "key" (const (Just (Aeson.String "first")))) initial
        changed = U.run (modify "custom" "key" (const Aeson.Null)) created
        removed = U.run (alter "custom" "key" (const Nothing)) changed
    lookupValue "custom" "key" created `shouldBe` Just (Aeson.String "first")
    lookupValue "custom" "key" changed `shouldBe` Just Aeson.Null
    Map.lookup "custom" (contexts removed) `shouldBe` Just (C.Other Map.empty)
    contexts (U.run (alter "custom" "key" (const Nothing)) initial) `shouldBe` contexts initial
    contexts (U.run (modify "custom" "key" (error "unused")) initial) `shouldBe` contexts initial
  it "skips typed payloads and missing fields, preserving unrelated entries" do
    let typed = U.run (setContext "custom" (C.App App.empty)) initial
        seeded = U.run (setContext "custom" (C.Other (Map.singleton "other" Aeson.Null))) initial
    lookupValue "custom" "key" typed `shouldBe` Nothing
    contexts (U.run (alter "custom" "key" (error "unused")) typed) `shouldBe` contexts typed
    contexts (U.run (modify "custom" "missing" (error "unused")) seeded) `shouldBe` contexts seeded
    lookupValue "custom" "other" (U.run (alter "custom" "key" (const (Just Aeson.Null))) seeded) `shouldBe` Just Aeson.Null
  it "forces computed maps and assigned values without deeply evaluating JSON" do
    evaluate (U.run (alter "custom" "key" (const (Just (error "value")))) initial) `shouldThrow` anyErrorCall
    let lazyJson = Aeson.Array (pure (error "nested JSON"))
    evaluate (Map.size (contexts (U.run (alter "custom" "key" (const (Just lazyJson))) initial))) `shouldReturn` 1

spec_customFields :: Spec
spec_customFields = do
  customSpec "scope custom fields" mempty (.contexts) S.setContext S.lookupContextValue S.alterContextValue S.modifyExistingContextValue
  customSpec "event custom fields" E.empty (.contexts) E.setContext E.lookupContextValue E.alterContextValue E.modifyExistingContextValue

spec_lookups :: Spec
spec_lookups = describe "key-first lookups" do
  it "S.lookupTag distinguishes present and missing entries" do
    let record = U.run (S.setTag "key" ("value")) (mempty :: S.ScopeData)
    S.lookupTag "key" record `shouldBe` Just ("value")
    S.lookupTag "missing" record `shouldBe` Nothing
  it "S.lookupExtra distinguishes present and missing entries" do
    let record = U.run (S.setExtra "key" (Aeson.Null)) (mempty :: S.ScopeData)
    S.lookupExtra "key" record `shouldBe` Just (Aeson.Null)
    S.lookupExtra "missing" record `shouldBe` Nothing
  it "S.lookupContext distinguishes present and missing entries" do
    let record = U.run (S.setContext "key" (C.Other Map.empty)) (mempty :: S.ScopeData)
    S.lookupContext "key" record `shouldBe` Just (C.Other Map.empty)
    S.lookupContext "missing" record `shouldBe` Nothing
  it "E.lookupTag distinguishes present and missing entries" do
    let record = U.run (E.setTag "key" ("value")) E.empty
    E.lookupTag "key" record `shouldBe` Just ("value")
    E.lookupTag "missing" record `shouldBe` Nothing
  it "E.lookupExtra distinguishes present and missing entries" do
    let record = U.run (E.setExtra "key" (Aeson.Null)) E.empty
    E.lookupExtra "key" record `shouldBe` Just (Aeson.Null)
    E.lookupExtra "missing" record `shouldBe` Nothing
  it "E.lookupModule distinguishes present and missing entries" do
    let record = U.run (E.setModule "key" ("value")) E.empty
    E.lookupModule "key" record `shouldBe` Just ("value")
    E.lookupModule "missing" record `shouldBe` Nothing
  it "E.lookupContext distinguishes present and missing entries" do
    let record = U.run (E.setContext "key" (C.Other Map.empty)) E.empty
    E.lookupContext "key" record `shouldBe` Just (C.Other Map.empty)
    E.lookupContext "missing" record `shouldBe` Nothing
  it "User.lookupData distinguishes present and missing entries" do
    let record = U.run (User.setData "key" (Aeson.Null)) User.empty
    User.lookupData "key" record `shouldBe` Just (Aeson.Null)
    User.lookupData "missing" record `shouldBe` Nothing
  it "B.lookupData distinguishes present and missing entries" do
    let record = U.run (B.setData "key" (Aeson.Null)) B.empty
    B.lookupData "key" record `shouldBe` Just (Aeson.Null)
    B.lookupData "missing" record `shouldBe` Nothing
  it "M.lookupData distinguishes present and missing entries" do
    let record = U.run (M.setData "key" (Aeson.Null)) M.empty
    M.lookupData "key" record `shouldBe` Just (Aeson.Null)
    M.lookupData "missing" record `shouldBe` Nothing
  it "R.lookupCookie distinguishes present and missing entries" do
    let record = U.run (R.setCookie "key" ("value")) R.empty
    R.lookupCookie "key" record `shouldBe` Just ("value")
    R.lookupCookie "missing" record `shouldBe` Nothing
  it "R.lookupEnv distinguishes present and missing entries" do
    let record = U.run (R.setEnv "key" (Aeson.Null)) R.empty
    R.lookupEnv "key" record `shouldBe` Just (Aeson.Null)
    R.lookupEnv "missing" record `shouldBe` Nothing
  it "R.lookupQueryParam distinguishes present and missing entries" do
    let record = U.run (R.setQueryParam "key" ("value")) R.empty
    R.lookupQueryParam "key" record `shouldBe` Just ("value")
    R.lookupQueryParam "missing" record `shouldBe` Nothing
  it "headers prefer exact spelling and otherwise use ascending stored-key order" do
    let record = R.empty{R.headers = Map.fromList [("X-Key", "first"), ("x-key", "second"), ("Other", "keep"), ("Ä", "unicode")]}
    R.lookupHeader "x-key" record `shouldBe` Just "second"
    R.lookupHeader "X-KEY" record `shouldBe` Just "first"
    R.lookupHeader "missing" record `shouldBe` Nothing
    R.lookupHeader "ä" record `shouldBe` Nothing
    let changed = U.run (R.setHeader "x-KEY" "replacement") record
    changed.headers `shouldBe` Map.fromList [("x-KEY", "replacement"), ("Other", "keep"), ("Ä", "unicode")]
    (U.run (R.removeHeader "X-KEY") changed).headers `shouldBe` Map.fromList [("Other", "keep"), ("Ä", "unicode")]

spec_with :: Spec
spec_with = describe "with transformations" do
  it "uses ordinary setters for mandatory, optional and keyed edits" do
    let user = U.run [User.setName " Alice ", User.with \record -> User.setName (Text.strip record.name)] User.empty
    user.name `shouldBe` "Alice"
    let event = E.apply E.empty (E.with \record -> E.setOptionalLevel (fmap (error "absent callback") record.level))
    event.level `shouldBe` Nothing
    let local = U.run [S.setTag "region" "east", S.with \record -> S.setOptionalTag "region" (Text.toUpper <$> S.lookupTag "region" record)] (mempty :: S.ScopeData)
    S.lookupTag "region" local `shouldBe` Just "EAST"
  it "checks the guide's keyed insertion/removal and typed-context examples" do
    scope <- S.create S.Current
    let cycleAttempt = Sentry.updateScope scope $
          S.with \local ->
            S.setOptionalTag "attempt" $
              (\case
                Nothing -> Just "first"
                Just "first" -> Just "retry"
                Just _ -> Nothing)
              (S.lookupTag "attempt" local)
    forM_ [Just "first", Just "retry", Nothing] \expected -> do
      cycleAttempt
      S.lookupTag "attempt" <$> S.readScopeRef scope `shouldReturn` expected
    Sentry.updateScope scope $ S.alterAppContext \existing -> Just (maybe App.empty id existing)
    S.lookupAppContext <$> S.readScopeRef scope `shouldReturn` Just App.empty
    Sentry.updateScope scope (S.alterAppContext (fmap (U.run (App.setAppName "updated"))))
    fmap (.appName) . S.lookupAppContext <$> S.readScopeRef scope `shouldReturn` Just "updated"
  it "observes only local metadata and removal reveals inheritance" do
    let inherited = U.run (S.setTag "region" "inherited") (mempty :: S.ScopeData)
        edited = U.run (S.with \local -> S.setOptionalTag "copy" (S.lookupTag "region" local)) (mempty :: S.ScopeData)
        removed = U.run [S.setTag "region" "local", S.with \_ -> S.setOptionalTag "region" Nothing] edited
    S.lookupTag "copy" edited `shouldBe` Nothing
    S.lookupTag "region" (inherited <> removed) `shouldBe` Just "inherited"
  it "observes preceding edits in atomic concurrent updates" do
    scope <- S.create S.Current
    completions <- replicateM 100 do
      done <- newEmptyMVar
      void $ forkFinally (Sentry.updateScope scope (S.with \local -> S.setTag "count" (maybe "x" (<> "x") (S.lookupTag "count" local)))) (putMVar done)
      pure done
    forM_ completions \done -> takeMVar done >>= either throwIO pure
    snapshot <- S.readScopeRef scope
    S.lookupTag "count" snapshot `shouldBe` Just (Text.replicate 100 "x")

spec_ordered :: Spec
spec_ordered = describe "ordered searches and filters" do
  it "finds the first breadcrumb and filters stably without deduplicating" do
    let a = U.run (B.setMessage "a") B.empty
        b = U.run (B.setMessage "b") B.empty
        collection = B.Breadcrumbs [a, b, a]
    B.findBreadcrumb (const True) collection `shouldBe` Just a
    B.findBreadcrumb (const True) B.emptyCollection `shouldBe` Nothing
    (U.run (B.filterBreadcrumbs ((== "a") . (.message))) collection).values `shouldBe` [a, a]
    let local = U.run (S.setBreadcrumbs collection) (mempty :: S.ScopeData)
    S.findBreadcrumb (const True) local `shouldBe` Just a
    toList (U.run (S.filterBreadcrumbs ((== "a") . (.message))) local).breadcrumbs `shouldBe` [a, a]
    (E.apply E.empty (E.modifyExistingBreadcrumbs (B.filterBreadcrumbs (error "unused")))).breadcrumbs `shouldBe` Nothing
    (E.apply (E.apply E.empty (E.setBreadcrumbs collection)) (E.modifyExistingBreadcrumbs (B.filterBreadcrumbs (const False)))).breadcrumbs `shouldBe` Just B.emptyCollection
  it "finds the first exception and filters stably" do
    let a = U.run (X.setValue "a") X.empty
        b = U.run (X.setValue "b") X.empty
        collection = X.Exceptions [a, b, a]
    X.findException (const True) collection `shouldBe` Just a
    X.findException (const True) X.emptyChain `shouldBe` Nothing
    (U.run (X.filterExceptions ((== "a") . (.value))) collection).values `shouldBe` [a, a]
    (E.apply E.empty (E.modifyExistingExceptionChain (X.filterExceptions (error "unused")))).exception `shouldBe` Nothing
    (E.apply (E.apply E.empty (E.setExceptionChain collection)) (E.modifyExistingExceptionChain (X.filterExceptions (const False)))).exception `shouldBe` Just X.emptyChain
  it "keeps fingerprint order, duplicates and optional presence" do
    let event = E.apply E.empty (E.setFingerprint ["a", "b", "a"])
        local = U.run (S.setFingerprint ["a", "b", "a"]) (mempty :: S.ScopeData)
    E.findFingerprintComponent (const True) event `shouldBe` Just "a"
    S.findFingerprintComponent (const True) local `shouldBe` Just "a"
    (E.apply event (E.filterFingerprint (== "a"))).fingerprint `shouldBe` ["a", "a"]
    (U.run (S.filterFingerprint (== "a")) local).fingerprint `shouldBe` Just ["a", "a"]
    (U.run (S.filterFingerprint (const False)) local).fingerprint `shouldBe` Just []
    (U.run (S.filterFingerprint (error "unused")) (mempty :: S.ScopeData)).fingerprint `shouldBe` Nothing
    S.findFingerprintComponent (error "unused") (mempty :: S.ScopeData) `shouldBe` Nothing
    E.findFingerprintComponent (error "unused") (E.apply E.empty E.clearFingerprint) `shouldBe` Nothing
  it "searches errors from the front and preserves filtered duplicates" do
    let a = ProcessingError.empty{ProcessingError.name = "a"}
        b = ProcessingError.empty{ProcessingError.name = "b"}
        event = E.apply E.empty (E.setErrors [a, b, a])
    E.findError (const True) event `shouldBe` Just a
    E.findError ((== "missing") . (.name)) event `shouldBe` Nothing
    (E.apply event (E.filterErrors ((== "a") . (.name)))).errors `shouldBe` [a, a]
  it "skips predicates for empty error lists" do
    E.findError (error "unused") E.empty `shouldBe` Nothing
    (E.apply E.empty (E.filterErrors (error "unused"))).errors `shouldBe` []

seed :: S.ScopeUpdate
seed =
  mconcat
    [ S.setFingerprint ["a", "b"],
      S.setBreadcrumbs (B.singleton B.empty),
      S.setContextValue "custom" "key" Aeson.Null,
      S.setAppContext App.empty,
      S.setOsContext Os.empty,
      S.setRuntimeContext Runtime.empty,
      S.setBrowserContext Browser.empty,
      S.setDeviceContext Device.empty,
      S.setTraceContext Trace.empty
    ]

-- Each row checks the public wrapper against its pure builder on a seeded scope.
operations :: [(String, S.ScopeUpdate, S.Scope -> IO (), Context.Context -> IO (), IO (), Context.Context -> IO (), IO ())]
operations =
  [ ("alterAppContext", S.alterAppContext (const Nothing), \scope -> Ops.alterAppContext scope (const Nothing), \ctx -> Ops.alterAppContextAt ctx (const Nothing), Sentry.alterAppContext (const Nothing), \ctx -> Ops.alterAppContextAt ctx (error "unused"), Sentry.alterAppContext (error "unused")),
    ("alterOsContext", S.alterOsContext (const Nothing), \scope -> Ops.alterOsContext scope (const Nothing), \ctx -> Ops.alterOsContextAt ctx (const Nothing), Sentry.alterOsContext (const Nothing), \ctx -> Ops.alterOsContextAt ctx (error "unused"), Sentry.alterOsContext (error "unused")),
    ("alterRuntimeContext", S.alterRuntimeContext (const Nothing), \scope -> Ops.alterRuntimeContext scope (const Nothing), \ctx -> Ops.alterRuntimeContextAt ctx (const Nothing), Sentry.alterRuntimeContext (const Nothing), \ctx -> Ops.alterRuntimeContextAt ctx (error "unused"), Sentry.alterRuntimeContext (error "unused")),
    ("alterBrowserContext", S.alterBrowserContext (const Nothing), \scope -> Ops.alterBrowserContext scope (const Nothing), \ctx -> Ops.alterBrowserContextAt ctx (const Nothing), Sentry.alterBrowserContext (const Nothing), \ctx -> Ops.alterBrowserContextAt ctx (error "unused"), Sentry.alterBrowserContext (error "unused")),
    ("alterDeviceContext", S.alterDeviceContext (const Nothing), \scope -> Ops.alterDeviceContext scope (const Nothing), \ctx -> Ops.alterDeviceContextAt ctx (const Nothing), Sentry.alterDeviceContext (const Nothing), \ctx -> Ops.alterDeviceContextAt ctx (error "unused"), Sentry.alterDeviceContext (error "unused")),
    ("alterTraceContext", S.alterTraceContext (const Nothing), \scope -> Ops.alterTraceContext scope (const Nothing), \ctx -> Ops.alterTraceContextAt ctx (const Nothing), Sentry.alterTraceContext (const Nothing), \ctx -> Ops.alterTraceContextAt ctx (error "unused"), Sentry.alterTraceContext (error "unused")),
    ("modifyExistingContextValue", S.modifyExistingContextValue "custom" "key" (const (Aeson.String "changed")), \scope -> Ops.modifyExistingContextValue scope "custom" "key" (const (Aeson.String "changed")), \ctx -> Ops.modifyExistingContextValueAt ctx "custom" "key" (const (Aeson.String "changed")), Sentry.modifyExistingContextValue "custom" "key" (const (Aeson.String "changed")), \ctx -> Ops.modifyExistingContextValueAt ctx (error "key") (error "field") (error "unused"), Sentry.modifyExistingContextValue (error "key") (error "field") (error "unused")),
    ("alterContextValue", S.alterContextValue "custom" "key" (const Nothing), \scope -> Ops.alterContextValue scope "custom" "key" (const Nothing), \ctx -> Ops.alterContextValueAt ctx "custom" "key" (const Nothing), Sentry.alterContextValue "custom" "key" (const Nothing), \ctx -> Ops.alterContextValueAt ctx (error "key") (error "field") (error "unused"), Sentry.alterContextValue (error "key") (error "field") (error "unused")),
    ("filterFingerprint", S.filterFingerprint (const False), \scope -> Ops.filterFingerprint scope (const False), \ctx -> Ops.filterFingerprintAt ctx (const False), Sentry.filterFingerprint (const False), \ctx -> Ops.filterFingerprintAt ctx (error "unused"), Sentry.filterFingerprint (error "unused")),
    ("filterBreadcrumbs", S.filterBreadcrumbs (const False), \scope -> Ops.filterBreadcrumbs scope (const False), \ctx -> Ops.filterBreadcrumbsAt ctx (const False), Sentry.filterBreadcrumbs (const False), \ctx -> Ops.filterBreadcrumbsAt ctx (error "unused"), Sentry.filterBreadcrumbs (error "unused"))
  ]

metadata :: S.ScopeData -> (Map Text C.Context, Maybe [Text], [B.Breadcrumb])
metadata record = (record.contexts, record.fingerprint, toList record.breadcrumbs)

spec_effectful :: Spec
spec_effectful = before_ Test.cleanScopes $ after_ Test.cleanScopes $ describe "scope transformation wrappers" do
  forM_ operations \(label, pureUpdate, explicit, at, ambient, invalidAt, invalidAmbient) -> describe label do
    it "supports explicit edits without initialization" $ do
      scope <- S.create S.Current
      Sentry.updateScope scope seed
      explicit scope
      metadata <$> S.readScopeRef scope `shouldReturn` metadata (U.run (seed <> pureUpdate) (mempty :: S.ScopeData))
    it "supports explicit context edits without initialization" $ do
      isolation <- S.getIsolationScope
      Sentry.updateScope isolation seed
      ThreadLocal.getContext >>= at
      metadata <$> S.readScopeRef isolation `shouldReturn` metadata (U.run (seed <> pureUpdate) (mempty :: S.ScopeData))
    it "routes ambient and context operations to isolation" $ void $ Test.withClient \_ -> do
      isolation <- S.getIsolationScope
      current <- S.getCurrentScope
      Sentry.updateScope current seed
      Sentry.updateScope isolation seed
      ambient
      metadata <$> S.readScopeRef isolation `shouldReturn` metadata (U.run (seed <> pureUpdate) (mempty :: S.ScopeData))
      metadata <$> S.readScopeRef current `shouldReturn` metadata (U.run seed mempty)
      Sentry.updateScope isolation seed
      ctx <- ThreadLocal.getContext
      at ctx
      metadata <$> S.readScopeRef isolation `shouldReturn` metadata (U.run (seed <> pureUpdate) (mempty :: S.ScopeData))
      metadata <$> S.readScopeRef current `shouldReturn` metadata (U.run seed mempty)
    it "skips arguments and scope creation without a recording client" $ do
      ThreadLocal.adjustContext (S.removeIsolation . S.removeCurrent)
      invalidAmbient
      ctx <- ThreadLocal.getContext
      invalidAt ctx
      S.lookupIsolation ctx `shouldSatisfy` maybe True (const False)
    it "skips absent context targets even with a recording parent" $ void $ Test.withClient \_ -> do
      ctx <- ThreadLocal.getContext
      client <- S.lookupClient
      current <- S.getCurrentScope
      S.bindClient client current
      invalidAt (S.removeIsolation ctx)
    it "respects a disabled current client shadowing its recording parent" $ void $ Test.withClient \_ -> do
      isolation <- S.getIsolationScope
      Sentry.updateScope isolation seed
      current <- S.getCurrentScope
      S.bindClient (Just NON_RECORDING_CLIENT) current
      invalidAmbient
      metadata <$> S.readScopeRef isolation `shouldReturn` metadata (U.run seed mempty)
