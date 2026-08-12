module ShutdownTest where

import Control.Monad (replicateM, void)
import Data.Default (def)
import Patrol.Type.Event qualified as Patrol.Event
import Sentry.Capture (captureEvent)
import Sentry.Client (Client)
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Scope.IO (withClient)
import Sentry.TestKit.Kent qualified as Kent
import Sentry.Transport (ShutdownResponse (..), SomeTransport (..))
import Sentry.Transport qualified as Transport
import Sentry.Transport.HTTP.Async qualified as AsyncHttpTransport
import Test.Hspec
import UnliftIO.Exception (toException)
import Witch qualified

spec_shutdownDrains :: Spec
spec_shutdownDrains = describe "graceful shutdown" do
  it "drains queued events before the worker exits" $
    Kent.withKent \kent -> do
      Kent.flushKent kent
      let dsn = Kent.dsnFor kent "1"
      transport <- AsyncHttpTransport.build def Nothing 100 kent.manager dsn
      let opts =
            (def @ClientOptions)
              { dsn = Just dsn,
                transport = Just (Witch.from (SomeTransport transport)),
                sendClientReports = False
              }
          client = Witch.from @ClientOptions @Client opts
          n = 25 :: Int
      events <-
        replicateM n $
          Patrol.Event.fromSomeException . toException $
            userError "boom"
      -- Enqueue everything, then shut down *without* a prior flush: the worker
      -- must process the backlog before honouring the shutdown signal.
      void $ traverse (\e -> withClient client $ captureEvent e) events
      shutdownResult <- Transport.shutdown transport 5
      shutdownResult `shouldBe` ShutdownSucceeded
      received <- Kent.listEvents kent
      length received `shouldBe` n
