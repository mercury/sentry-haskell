module IntegrationSetupTest where

import Data.Default (def)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Vector qualified as Vector
import Sentry.Client qualified as Client
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Integration (Integration (..), fromIntegration)
import Sentry.Test qualified as Test
import Test.Hspec

spec_integration_setup :: Spec
spec_integration_setup = do
  describe "Client.new / Integration.setup" do
    it "calls setup on user-provided integrations" do
      ref <- newIORef ([] :: [Text])
      let int = RecordingIntegration ref
          opts = def{integrations = Vector.fromList [fromIntegration int]}
      client <- Client.new opts{dsn = Just Test.TEST_DSN}
      recorded <- readIORef ref
      -- resolveOptionDefaults fills environment = "production" before setup runs
      recorded `shouldBe` ["production"]
      client.options.environment `shouldBe` Just "set-by-setup"

    it "threads options through integrations in roster order" do
      ref <- newIORef ([] :: [Text])
      let int1 = RecordingIntegration ref
          int2 = ObservingIntegration ref
          opts =
            def
              { integrations = Vector.fromList [fromIntegration int1, fromIntegration int2],
                environment = Nothing
              }
      client <- Client.new opts{dsn = Just Test.TEST_DSN}
      recorded <- readIORef ref
      -- resolveOptionDefaults fills "production"; ContextIntegration (builtin) runs first
      -- but doesn't write to `ref`.  int1 sees "production", sets "set-by-setup".
      -- int2 sees "set-by-setup", sets "observed-by-second".
      recorded `shouldBe` ["production", "set-by-setup"]
      -- the last write wins in the chain
      client.options.environment `shouldBe` Just "observed-by-second"

    it "does not call setup when defaultIntegrations is False and no integrations given" do
      ref <- newIORef ([] :: [Text])
      let opts = def{defaultIntegrations = False}
      client <- Client.new opts{dsn = Just Test.TEST_DSN}
      recorded <- readIORef ref
      recorded `shouldBe` []
      Vector.length client.integrations `shouldBe` 0

    it "pins builtinIntegrations count (ContextIntegration is the only builtin)" do
      -- builtinIntegrations = [ContextIntegration], so True adds 1 integration.
      -- This test pins the wiring so it fails loudly when builtins change without
      -- updating the tests.
      clientTrue <- Client.new def{dsn = Just Test.TEST_DSN, defaultIntegrations = True}
      clientFalse <- Client.new def{dsn = Just Test.TEST_DSN, defaultIntegrations = False}
      Vector.length clientTrue.integrations `shouldBe` 1
      Vector.length clientFalse.integrations `shouldBe` 0

    it "default setup (no override) leaves options unchanged" do
      let opts = def{environment = Just "original"}
          int = NoopIntegration
      client <- Client.new opts{dsn = Just Test.TEST_DSN, integrations = Vector.fromList [fromIntegration int]}
      client.options.environment `shouldBe` Just "original"

    it "dedup collapses two integrations of the same type to one" do
      ref <- newIORef ([] :: [Text])
      let int1 = RecordingIntegration ref
          int2 = RecordingIntegration ref
          opts = def{integrations = Vector.fromList [fromIntegration int1, fromIntegration int2]}
      client <- Client.new opts{dsn = Just Test.TEST_DSN}
      -- ContextIntegration (builtin) + one RecordingIntegration survive
      Vector.length client.integrations `shouldBe` 2
      -- RecordingIntegration.setup ran exactly once (deduped)
      recorded <- readIORef ref
      length recorded `shouldBe` 1

-- ---------------------------------------------------------------------------
-- Helpers

-- | Records calls to 'setup' (appends current @environment@ to the IORef) and
-- sets @environment = Just "set-by-setup"@.
type RecordingIntegration :: Type
newtype RecordingIntegration = RecordingIntegration (IORef [Text])

instance Integration RecordingIntegration where
  name _ = "recording"
  setup (RecordingIntegration ref) opts = do
    modifyIORef' ref (<> [fromMaybe "" opts.environment])
    pure opts{environment = Just "set-by-setup"}

-- | Observes what @environment@ was set to by the previous integration and
-- overwrites it with @"observed-by-second"@.
type ObservingIntegration :: Type
newtype ObservingIntegration = ObservingIntegration (IORef [Text])

instance Integration ObservingIntegration where
  name _ = "observing"
  setup (ObservingIntegration ref) opts = do
    modifyIORef' ref (<> [fromMaybe "" opts.environment])
    pure opts{environment = Just "observed-by-second"}

-- | Uses the default 'setup' (a no-op returning options unchanged).
type NoopIntegration :: Type
data NoopIntegration = NoopIntegration

instance Integration NoopIntegration where
  name _ = "noop"
