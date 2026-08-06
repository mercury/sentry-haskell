module LifecycleTest where

import Control.Exception (Exception, SomeException, bracket, throwIO, try)
import Data.Default (def)
import Data.IORef (IORef, newIORef, readIORef)
import Data.IORef qualified as IORef
import Data.Kind (Type)
import Data.Maybe (isJust)
import Patrol.Type.Level qualified as Patrol.Level
import Sentry.Core qualified as Sentry
import Sentry.Client (Client (..))
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Test (withGlobalScope)
import Sentry.Test qualified as Test
import Sentry.Transport (FlushResponse (..), SendResponse (..), ShutdownResponse (..), SomeTransport (..), Transport (..))
import Test.Hspec
import Witch qualified

spec_lifecycle :: Spec
spec_lifecycle = do
  describe "Sentry.init / Sentry.close" do
    it "flushes and shuts down the transport on normal exit" do
      lt <- newLifecycleTransport
      let opts = def{transport = Just (Witch.from (SomeTransport lt)), dsn = Just Test.TEST_DSN}
      withGlobalScope $ bracket (Sentry.init opts) Sentry.close \_ -> pure ()
      flushCount <- readIORef lt.flushes
      shutdownCount <- readIORef lt.shutdowns
      flushCount `shouldBe` 1
      shutdownCount `shouldBe` 1

    it "flushes and shuts down the transport on exception, and rethrows" do
      lt <- newLifecycleTransport
      let opts = def{transport = Just (Witch.from (SomeTransport lt)), dsn = Just Test.TEST_DSN}
      result <-
        try @SomeException $
          withGlobalScope $
            bracket (Sentry.init opts) Sentry.close \_ -> throwIO TestError
      case result of
        Right _ -> expectationFailure "expected exception to propagate"
        Left _ -> pure ()
      flushCount <- readIORef lt.flushes
      shutdownCount <- readIORef lt.shutdowns
      flushCount `shouldBe` 1
      shutdownCount `shouldBe` 1

    it "is a no-op on exit when there is no transport" do
      result <-
        withGlobalScope $
          bracket (Sentry.init def) Sentry.close \_ -> pure (42 :: Int)
      result `shouldBe` 42

    it "returns a client carrying the configured transport" do
      lt <- newLifecycleTransport
      let opts = def{transport = Just (Witch.from (SomeTransport lt)), dsn = Just Test.TEST_DSN}
      withGlobalScope $ bracket (Sentry.init opts) Sentry.close \client ->
        isJust client.transport `shouldBe` True

    it "binds the client to the global scope so capture works without scopes" do
      transport <- Test.new
      let opts = def{transport = Just (Witch.from (SomeTransport transport)), dsn = Just Test.TEST_DSN}
      withGlobalScope $
        bracket (Sentry.init opts) Sentry.close \_ ->
          () <$ Sentry.captureMessage Patrol.Level.Info "scope-free capture"
      events <- Test.fetchAndClearEvents transport
      length events `shouldBe` 1

-- Helpers

-- | A 'Transport' that records flush and shutdown calls.
type LifecycleTransport :: Type
data LifecycleTransport = LifecycleTransport
  { flushes :: IORef Int,
    shutdowns :: IORef Int
  }

newLifecycleTransport :: IO LifecycleTransport
newLifecycleTransport = LifecycleTransport <$> newIORef 0 <*> newIORef 0

instance Transport LifecycleTransport where
  send _ _ = pure SendProcessed
  flush t _ = FlushSucceeded <$ IORef.modifyIORef' t.flushes (+ 1)
  shutdown t _ = ShutdownSucceeded <$ IORef.modifyIORef' t.shutdowns (+ 1)

type TestError :: Type
data TestError = TestError
  deriving stock (Show)

instance Exception TestError
