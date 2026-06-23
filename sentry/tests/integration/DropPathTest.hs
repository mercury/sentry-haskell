module DropPathTest where

import Control.Monad (replicateM, void)
import Data.Default (def)
import Patrol.Type.Event qualified as Patrol.Event
import Sentry.Capture (captureEvent)
import Sentry.Client (Client)
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Scope.IO (withClient)
import Sentry.TestKit.Kent qualified as Kent
import Sentry.Transport (FlushResponse (..), SomeTransport (..))
import Sentry.Transport qualified as Transport
import Sentry.Transport.HTTP.Async qualified as AsyncHttpTransport
import Test.Hspec
import UnliftIO.Exception (SomeException (..))
import Witch qualified

-- | Capture exceptions through a client built from the given options, flush,
-- and return the number of events kent received.
deliveredUnder :: (ClientOptions -> ClientOptions) -> IO Int
deliveredUnder tweak =
  Kent.withKent \kent -> do
    Kent.flushKent kent
    let dsn = Kent.dsnFor kent "1"
    transport <- AsyncHttpTransport.build def Nothing 100 kent.manager dsn
    let opts =
          tweak
            (def @ClientOptions)
              { dsn = Just dsn,
                transport = Just (Witch.from (SomeTransport transport)),
                sendClientReports = False
              }
        client = Witch.from @ClientOptions @Client opts
    events <-
      replicateM 10 $
        Patrol.Event.fromSomeException $
          SomeException (userError "boom")
    void $ traverse (\e -> withClient client $ captureEvent e) events
    flushResult <- Transport.flush transport 5
    flushResult `shouldBe` FlushSucceeded
    length <$> Kent.listEvents kent

spec_dropPaths :: Spec
spec_dropPaths = describe "events dropped before the wire" do
  it "delivers nothing when the sample rate is 0" do
    delivered <- deliveredUnder \opts -> opts{sampleRate = 0}
    delivered `shouldBe` 0

  it "delivers nothing when beforeSend rejects every event" do
    delivered <- deliveredUnder \opts -> opts{beforeSend = Just (const Nothing)}
    delivered `shouldBe` 0
