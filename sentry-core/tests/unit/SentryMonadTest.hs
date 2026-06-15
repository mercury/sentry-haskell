{-# LANGUAGE DerivingVia #-}

module SentryMonadTest where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), runReaderT)
import Patrol.Type.Level qualified as Patrol.Level
import Sentry.Capture (captureException, captureMessage)
import Sentry.Client (Client)
import Sentry.Monad (HasClient (..), SentryT (..), askClient, runSentryT)
import Sentry.Test qualified as Test
import Test.Hspec

-- | A sample application environment that embeds a 'Client' alongside other
-- fields. Demonstrates that 'HasClient' composes with arbitrary environments.
data AppEnv = AppEnv
  { appClient :: Client,
    appName :: String
  }

instance HasClient AppEnv where
  clientL f env = (\c -> env{appClient = c}) <$> f env.appClient

-- | Like 'Test.withTestClient' but runs in a @'ReaderT' 'AppEnv' IO@ context.
withAppEnv :: ReaderT AppEnv IO a -> IO (a, Test.TestTransport)
withAppEnv action = do
  transport <- Test.new
  let env = AppEnv{appClient = Test.mkClient transport, appName = "test"}
  result <- runReaderT action env
  pure (result, transport)

-- | A newtype over 'ReaderT Client IO' that derives its 'MonadReader' instance
-- via 'SentryT IO'. This verifies that the two newtypes are coercible and
-- that @DerivingVia@ through 'SentryT' compiles correctly.
--
-- Note: 'HasClient' is for /environments/ (kind 'Type'), not monads
-- (kind @Type -> Type@). @captureMessage@ works here because 'MyApp' gets
-- @MonadReader Client@ (which @askClient@ uses) and 'HasClient Client' is
-- the pre-existing base instance.
newtype MyApp a = MyApp {unMyApp :: ReaderT Client IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)
  deriving (MonadReader Client) via (SentryT IO)

runMyApp :: Test.TestTransport -> MyApp a -> IO a
runMyApp transport (MyApp action) = runReaderT action (Test.mkClient transport)

spec_sentryMonad :: Spec
spec_sentryMonad = describe "Sentry.Monad" do
  describe "SentryT / runSentryT" do
    it "askClient returns the client bound via runSentryT" do
      transport <- Test.new
      client <- runSentryT (Test.mkClient transport) askClient
      _ <- runSentryT client $ captureMessage Patrol.Level.Info "roundtrip"
      events <- Test.fetchAndClearEvents transport
      length events `shouldBe` 1

    it "captureMessage reaches the transport" do
      (_, transport) <- Test.withTestClient \_ ->
        captureMessage Patrol.Level.Warning "watch out"
      events <- Test.fetchAndClearEvents transport
      length events `shouldBe` 1

    it "captureException reaches the transport" do
      (_, transport) <- Test.withTestClient \_ ->
        captureException (userError "boom in SentryT")
      events <- Test.fetchAndClearEvents transport
      length events `shouldBe` 1

  describe "HasClient for a custom env via MonadReader" do
    it "captureMessage works in ReaderT AppEnv IO" do
      (_, transport) <-
        withAppEnv $
          captureMessage Patrol.Level.Info "hello from AppEnv"
      events <- Test.fetchAndClearEvents transport
      length events `shouldBe` 1

    it "captureException works in ReaderT AppEnv IO" do
      (_, transport) <-
        withAppEnv $
          captureException (userError "boom via AppEnv")
      events <- Test.fetchAndClearEvents transport
      length events `shouldBe` 1

  describe "DerivingVia SentryT" do
    it "a newtype over ReaderT Client IO can derive MonadReader Client via SentryT IO" do
      transport <- Test.new
      _ <-
        runMyApp transport $
          captureMessage Patrol.Level.Debug "via MyApp"
      events <- Test.fetchAndClearEvents transport
      length events `shouldBe` 1
