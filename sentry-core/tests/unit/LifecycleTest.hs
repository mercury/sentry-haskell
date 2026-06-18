module LifecycleTest where

import Control.Exception (Exception, SomeException, throwIO, try)
import Data.Default (def)
import Data.IORef (IORef, newIORef, readIORef)
import Data.IORef qualified as IORef
import Data.Kind (Type)
import Data.Maybe (isJust)
import Sentry.Client (Client (..))
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Init (withSentry, withSentryM)
import Sentry.Monad (askClient)
import Sentry.Test qualified as Test
import Sentry.Transport (FlushResponse (..), SendResponse (..), ShutdownResponse (..), SomeTransport (..), Transport (..))
import Test.Hspec
import Witch qualified

spec_lifecycle :: Spec
spec_lifecycle = do
  describe "withSentry" do
    it "flushes and shuts down the transport on normal exit" do
      lt <- newLifecycleTransport
      let opts = def{transport = Just (Witch.from (SomeTransport lt)), dsn = Just Test.TEST_DSN}
      withSentry opts \_ -> pure ()
      flushCount <- readIORef lt.flushes
      shutdownCount <- readIORef lt.shutdowns
      flushCount `shouldBe` 1
      shutdownCount `shouldBe` 1

    it "flushes and shuts down the transport on exception, and rethrows" do
      lt <- newLifecycleTransport
      let opts = def{transport = Just (Witch.from (SomeTransport lt)), dsn = Just Test.TEST_DSN}
      result <- try @SomeException $ withSentry opts \_ -> throwIO TestError
      case result of
        Right _ -> expectationFailure "expected exception to propagate"
        Left _ -> pure ()
      flushCount <- readIORef lt.flushes
      shutdownCount <- readIORef lt.shutdowns
      flushCount `shouldBe` 1
      shutdownCount `shouldBe` 1

    it "is a no-op on exit when there is no transport" do
      -- withSentry with no transport should not throw and should run the body
      result <- withSentry def \_ -> pure (42 :: Int)
      result `shouldBe` 42

  describe "withSentryM" do
    it "provides the client via askClient inside SentryT" do
      lt <- newLifecycleTransport
      let opts = def{transport = Just (Witch.from (SomeTransport lt)), dsn = Just Test.TEST_DSN}
      client <- withSentryM opts askClient
      -- The client should have a transport installed
      isJust client.transport `shouldBe` True

    it "flushes and shuts down on exit from withSentryM" do
      lt <- newLifecycleTransport
      let opts = def{transport = Just (Witch.from (SomeTransport lt)), dsn = Just Test.TEST_DSN}
      withSentryM opts (pure ())
      flushCount <- readIORef lt.flushes
      shutdownCount <- readIORef lt.shutdowns
      flushCount `shouldBe` 1
      shutdownCount `shouldBe` 1

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
