module CaptureEventTest where

import Control.Monad (replicateM)
import Data.Default (def)
import Data.Maybe (catMaybes)
import Kent qualified
import Patrol qualified
import Patrol.Type.Event qualified as Patrol.Event
import Sentry.Capture (captureEvent)
import Sentry.Client (Client)
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Monad (runSentryT)
import Sentry.Transport (FlushResponse (..), SomeTransport (..))
import Sentry.Transport qualified as Transport
import Sentry.Transport.HTTP.Async qualified as AsyncHttpTransport
import Test.Hspec
import UnliftIO.Exception (SomeException (..))
import Witch qualified

spec_captureEvent :: Spec
spec_captureEvent = describe "captureEvent against kent" do
  it "delivers every event to the server" $
    Kent.withKent 5001 \kent -> do
      Kent.flushKent kent
      let dsn = Kent.dsnFor kent "1"
      transport <- AsyncHttpTransport.new 100 kent.manager Nothing dsn
      let opts =
            (def @ClientOptions)
              { dsn = Just dsn,
                transport = Just (SomeTransport transport)
              }
          client = Witch.from @ClientOptions @Client opts
          n = 50 :: Int
      events <-
        replicateM n $
          Patrol.Event.fromSomeException $
            SomeException (userError "boom")
      sentIds <- traverse (\e -> runSentryT client $ captureEvent e) events
      flushResult <- Transport.flush transport 5
      flushResult `shouldBe` FlushSucceeded
      received <- Kent.listEvents kent
      length (catMaybes sentIds) `shouldBe` n
      length received `shouldBe` n
