{-# LANGUAGE DerivingVia #-}

module SentryMonadTest where

import Data.Kind (Type)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), runReaderT)
import Patrol.Type.Level qualified as Patrol.Level
import Sentry.Capture (captureException, captureMessage)
import Sentry.Client (Client)
import Sentry.Monad (HasClient (..), SentryT (..), askClient, runSentryT)
import Sentry.Test qualified as Test
import Test.Hspec

type Env :: Type
data Env = Env
  { client :: Client,
    name :: String
  }

instance HasClient Env where
  clientL f env = (\client -> env{client}) <$> f env.client

-- | Like 'Test.withClient' but runs in a @'ReaderT' 'Env' IO@ context.
withEnv :: ReaderT Env IO a -> IO (a, Test.TestTransport)
withEnv action = do
  transport <- Test.new
  let env = Env{client = Test.mkClient transport, name = "test"}
  result <- runReaderT action env
  pure (result, transport)

type App :: Type -> Type
newtype App a = App {unApp :: ReaderT Client IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)
  deriving (MonadReader Client) via (SentryT IO)

type role App nominal

runApp :: Test.TestTransport -> App a -> IO a
runApp transport (App action) = runReaderT action (Test.mkClient transport)

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
      (_, transport) <- Test.withClient \_ ->
        captureMessage Patrol.Level.Warning "watch out"
      events <- Test.fetchAndClearEvents transport
      length events `shouldBe` 1

    it "captureException reaches the transport" do
      (_, transport) <- Test.withClient \_ ->
        captureException (userError "boom in SentryT")
      events <- Test.fetchAndClearEvents transport
      length events `shouldBe` 1

  describe "HasClient for a custom env via MonadReader" do
    it "captureMessage works in ReaderT Env IO" do
      (_, transport) <-
        withEnv $
          captureMessage Patrol.Level.Info "hello from Env"
      events <- Test.fetchAndClearEvents transport
      length events `shouldBe` 1

    it "captureException works in ReaderT Env IO" do
      (_, transport) <-
        withEnv $
          captureException (userError "boom via Env")
      events <- Test.fetchAndClearEvents transport
      length events `shouldBe` 1

  describe "DerivingVia SentryT" do
    it "a newtype over ReaderT Client IO can derive MonadReader Client via SentryT IO" do
      transport <- Test.new
      _ <-
        runApp transport $
          captureMessage Patrol.Level.Debug "via App"
      events <- Test.fetchAndClearEvents transport
      length events `shouldBe` 1
