module ExecutorCaptureTest where

import Control.Exception (bracket)
import Control.Monad (replicateM, void)
import Data.Default (def)
import Data.Maybe (isJust)
import Network.Connection (TLSSettings (TLSSettingsSimple))
import Network.HTTP.Client qualified as Http
import Network.HTTP.Client.TLS (mkManagerSettings)
import Sentry.Client.Options qualified as Options
import Sentry.Client.Options.Dsn qualified as Dsn
import Sentry.Core qualified as Sentry
import Sentry.Init qualified as Init
import Sentry.Scope.Monad qualified as Scoped
import Sentry.TestKit.Sink qualified as Sink
import Sentry.Transport qualified as Transport
import Sentry.Transport.Delivery qualified as Delivery
import Sentry.Transport.Executor.Async qualified as Executor
import Sentry.Transport.HTTP.Sync qualified as Sync
import System.Timeout (timeout)
import Test.Hspec

spec_ownedCapture :: Spec
spec_ownedCapture =
  describe "owned capture through HTTPS" do
    it "delivers captured exceptions before closing the owned scope" do
      completed <- timeout 15_000_000 $ Sink.withSink \sink -> do
        let dsn = Sink.dsnFor sink "1"
        manager <- Http.newManager $ mkManagerSettings (TLSSettingsSimple True False False def) Nothing
        sync <- Sync.build def Nothing manager dsn
        let sendFn envelope = do
              Transport.send sync envelope `shouldReturn` Transport.SendProcessed
              pure Delivery.accepted
        bracket (Executor.new 32 Nothing sendFn) (\executor -> void $ Transport.shutdown executor 0) \executor -> do
          let opts =
                (Options.defaultClientOptions)
                  { Options.dsn = Dsn.Explicit dsn,
                    Options.transport = Just $ Options.PrebuiltTransport $ Transport.SomeTransport executor,
                    Options.sendClientReports = False
                  }
          bracket (Init.acquireClient opts) (\h -> Init.close h `shouldReturn` Transport.ShutdownSucceeded) \owned ->
            Scoped.withClient (Init.clientOf owned) do
              accepted <- replicateM 8 $ Sentry.captureException (userError "scope exit smoke test")
              all isJust accepted `shouldBe` True
          requests <- Sink.received sink
          length requests `shouldBe` 8
      completed `shouldBe` Just ()
