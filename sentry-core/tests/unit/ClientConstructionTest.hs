module ClientConstructionTest where

import Data.Bifunctor qualified as Bifunctor
import Data.Default (def)
import Data.Either (isLeft)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Kind (Type)
import Data.Maybe (isJust, isNothing)
import Data.Vector qualified as Vector
import Patrol qualified
import Patrol.Type.Dsn qualified as Patrol.Dsn
import Patrol.Type.Event qualified
import Patrol.Type.Level qualified as Level
import Sentry.Capture qualified as Capture
import Sentry.Client qualified as Client
import Sentry.Client.Options (ClientOptions (..), TransportProvider (..))
import Sentry.Client.Options.Dsn qualified as Dsn
import Sentry.Event.Captured (CapturedEvent (..))
import Sentry.Integration (Integration (..), fromIntegration)
import Sentry.Scope.IO qualified as ScopeIO
import Sentry.Scope.Operations qualified as Scope
import Sentry.Test qualified as Test
import Sentry.Transport (SomeTransport (..))
import Test.Hspec
import Witch qualified

-- | Counter and configurable setup edit; records the finalized capture view.
type OptionsEditIntegration :: Type
data OptionsEditIntegration = OptionsEditIntegration (IORef Int) (ClientOptions -> ClientOptions) (IORef [ClientOptions])

instance Integration OptionsEditIntegration where
  name _ = "edit"
  setup (OptionsEditIntegration calls edit _) opts = do
    modifyIORef' calls (+ 1)
    pure (edit opts)
  processEvent (OptionsEditIntegration _ _ seen) ce opts = do
    modifyIORef' seen (opts :)
    pure (Just ce.event)

type ObserveDisabled :: Type
newtype ObserveDisabled = ObserveDisabled (IORef [Dsn.DsnSource])

instance Integration ObserveDisabled where
  setup (ObserveDisabled seen) opts = do
    modifyIORef' seen (opts.dsn :)
    pure opts{dsn = Dsn.Explicit Test.TEST_DSN}

spec_construction :: Spec
spec_construction = describe "explicit client construction" do
  it "deduplicates setup and realizes the final factory once with the installed roster" do
    setups <- newIORef (0 :: Int)
    factories <- newIORef (0 :: Int)
    seen <- newIORef []
    transport <- Test.new
    let edit opts =
          opts
            { environment = Just "from-setup",
              sampleRate = Just 2,
              integrations = Vector.empty,
              transport = Just provider
            }
        integration = fromIntegration (OptionsEditIntegration setups edit seen)
        duplicate = fromIntegration (OptionsEditIntegration setups (\o -> o{environment = Just "wrong duplicate"}) seen)
        provider = DeferredTransport \dsn opts -> do
          modifyIORef' factories (+ 1)
          dsn `shouldBe` Test.TEST_DSN
          (opts.environment, opts.sampleRate) `shouldBe` (Just "from-setup", Just 1)
          Vector.length opts.integrations `shouldBe` 1
          pure (SomeTransport transport)
        unexpectedTransportFactory = DeferredTransport \_ _ -> do
          expectationFailure "setup must replace the initial factory"
          pure (SomeTransport transport)
        initialOptions =
          def
            { dsn = Dsn.Explicit Test.TEST_DSN,
              debug = Just False,
              defaultIntegrations = False,
              integrations = Vector.fromList [integration, duplicate],
              transport = Just unexpectedTransportFactory
            }
    client <- Client.new initialOptions
    readIORef setups `shouldReturn` 1
    readIORef factories `shouldReturn` 1
    Vector.length client.integrations `shouldBe` 1
    _ <- ScopeIO.withClient client $ Capture.captureMessage Level.Info "configured"
    recorded <- readIORef seen
    map (\o -> (o.environment, o.sampleRate, Vector.length o.integrations)) recorded `shouldBe` [(Just "from-setup", Just 1, 1)]
    events <- Test.fetchAndClearEvents transport
    map (\e -> e.environment) events `shouldBe` ["from-setup"]

  it "keeps caller Disabled authoritative after every hook and skips the factory" do
    setups <- newIORef (0 :: Int)
    factories <- newIORef (0 :: Int)
    seen <- newIORef []
    dsns <- newIORef []
    transport <- Test.new
    let provider = DeferredTransport \_ _ -> do
          modifyIORef' factories (+ 1)
          pure (SomeTransport transport)
        roster = Vector.fromList [fromIntegration (OptionsEditIntegration setups (\o -> o{dsn = Dsn.Explicit Test.TEST_DSN}) seen), fromIntegration (ObserveDisabled dsns)]
    client <- Client.new def{dsn = Dsn.Disabled, defaultIntegrations = False, integrations = roster, transport = Just provider}
    readIORef setups `shouldReturn` 1
    readIORef dsns `shouldReturn` [Dsn.Disabled]
    readIORef factories `shouldReturn` 0
    client.options.dsn `shouldBe` Dsn.Disabled
    isNothing client.transport `shouldBe` True

  it "still realizes a factory at sample rate zero, then drops capture" do
    calls <- newIORef (0 :: Int)
    transport <- Test.new
    let provider = DeferredTransport \_ _ -> do
          modifyIORef' calls (+ 1)
          pure (SomeTransport transport)
    client <- Client.new def{dsn = Dsn.Explicit Test.TEST_DSN, sampleRate = Just 0, transport = Just provider}
    readIORef calls `shouldReturn` 1
    ScopeIO.withClient client (Capture.captureMessage Level.Info "sampled out") `shouldReturn` Nothing
    Test.fetchAndClearEvents transport `shouldReturn` []

  it "uses terminal defaults after setup clears fields" do
    calls <- newIORef (0 :: Int)
    seen <- newIORef []
    transport <- Test.new
    let edit opts = opts{debug = Nothing, sampleRate = Nothing, environment = Nothing}
    client <- Test.mkCustomClient transport def{debug = Just True, sampleRate = Just 0, environment = Just "before", integrations = Vector.singleton (fromIntegration (OptionsEditIntegration calls edit seen))}
    (client.options.debug, client.options.sampleRate, client.options.environment) `shouldBe` (Just False, Just 1, Just "production")
    result <- ScopeIO.withClient client (Capture.captureMessage Level.Info "terminal defaults")
    result `shouldSatisfy` isJust
    events <- Test.fetchAndClearEvents transport
    map (\e -> e.environment) events `shouldBe` ["production"]

  it "gives all test helpers the same initialized options and setup semantics" do
    calls <- newIORef (0 :: Int)
    seen <- newIORef []
    transport <- Test.new
    let opts = def{integrations = Vector.singleton (fromIntegration (OptionsEditIntegration calls id seen))}
    first <- Test.mkCustomClient transport opts
    (second, _) <- Test.withCustomClient opts (const Scope.resolveClient)
    readIORef calls `shouldReturn` 2
    (first.options.dsn, first.options.environment, Vector.length first.integrations)
      `shouldBe` (second.options.dsn, second.options.environment, Vector.length second.integrations)
    ordinary <- Test.mkClient transport
    (scoped, _) <- Test.withClient (const Scope.resolveClient)
    Vector.length ordinary.integrations `shouldBe` Vector.length scoped.integrations
    ordinary.options.serverName `shouldSatisfy` isJust

  it "test helpers preserve explicit DSNs and respect Disabled" do
    transport <- Test.new
    disabled <- Test.mkCustomClient transport def{dsn = Dsn.Disabled}
    isNothing disabled.transport `shouldBe` True
    disabled.options.dsn `shouldBe` Dsn.Disabled
    let other = (Test.TEST_DSN){Patrol.Dsn.host = "other.invalid"}
    explicit <- Test.mkCustomClient transport def{dsn = Dsn.Explicit other}
    explicit.options.dsn `shouldBe` Dsn.Explicit other
    isJust explicit.transport `shouldBe` True

  it "converts options back to a DSN only for Explicit" do
    let opts = Witch.from Test.TEST_DSN :: ClientOptions
    opts.dsn `shouldBe` Dsn.Explicit Test.TEST_DSN
    Bifunctor.first (const ()) (Witch.tryInto @Patrol.Dsn opts) `shouldBe` Right Test.TEST_DSN
    map (\source -> isLeft (Witch.tryInto @Patrol.Dsn def{dsn = source})) [Dsn.Inherit, Dsn.Disabled] `shouldBe` [True, True]

  it "keeps the pure no-op client fully normalized" do
    let client = Client.NON_RECORDING_CLIENT
    (client.options.dsn, client.options.debug, client.options.sampleRate, client.options.environment)
      `shouldBe` (Dsn.Disabled, Just False, Just 1, Just "production")
    Vector.null client.integrations `shouldBe` True
