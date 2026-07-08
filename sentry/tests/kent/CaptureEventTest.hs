module CaptureEventTest where

import Control.Monad (replicateM)
import Data.Aeson qualified as Aeson
import Data.Default (def)
import Data.Maybe (catMaybes, isJust)
import Patrol.Type.Event qualified as Patrol.Event
import Sentry.Capture (captureEvent)
import Sentry.Client (Client)
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Scope.IO (withClient)
import Sentry.TestKit.Kent qualified as Kent
import Sentry.Transport (FlushResponse (..), SomeTransport (..))
import Sentry.Transport qualified as Transport
import Sentry.Transport.HTTP.Async qualified as AsyncHttpTransport
import Sentry.Transport.HTTP.Sync qualified as SyncHttpTransport
import Test.Hspec
import UnliftIO.Exception (SomeException (..))
import Witch qualified

spec_captureEvent :: Spec
spec_captureEvent = describe "captureEvent against kent (async transport)" do
  it "delivers every event to the server" $
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
          n = 50 :: Int
      events <-
        replicateM n $
          Patrol.Event.fromSomeException $
            SomeException (userError "boom")
      sentIds <- traverse (\e -> withClient client $ captureEvent e) events
      flushResult <- Transport.flush transport 5
      flushResult `shouldBe` FlushSucceeded
      received <- Kent.listEvents kent
      length (catMaybes sentIds) `shouldBe` n
      length received `shouldBe` n

spec_captureEventSync :: Spec
spec_captureEventSync = describe "captureEvent against kent (sync transport)" do
  it "delivers events synchronously, without a flush" $
    Kent.withKent \kent -> do
      Kent.flushKent kent
      let dsn = Kent.dsnFor kent "1"
      transport <- SyncHttpTransport.build def Nothing kent.manager dsn
      let opts =
            (def @ClientOptions)
              { dsn = Just dsn,
                transport = Just (Witch.from (SomeTransport transport)),
                sendClientReports = False
              }
          client = Witch.from @ClientOptions @Client opts
          n = 10 :: Int
      events <-
        replicateM n $
          Patrol.Event.fromSomeException $
            SomeException (userError "boom")
      -- The sync transport blocks until each send completes, so by the time
      -- 'captureEvent' returns the event is already on the server.
      sentIds <- traverse (\e -> withClient client $ captureEvent e) events
      received <- Kent.listEvents kent
      length (catMaybes sentIds) `shouldBe` n
      length received `shouldBe` n

spec_eventPayload :: Spec
spec_eventPayload = describe "event payload delivered to kent" do
  it "round-trips the event id and SDK-applied defaults" $
    Kent.withKent \kent -> do
      Kent.flushKent kent
      let dsn = Kent.dsnFor kent "1"
      transport <- SyncHttpTransport.build def Nothing kent.manager dsn
      let opts =
            (def @ClientOptions)
              { dsn = Just dsn,
                transport = Just (Witch.from (SomeTransport transport)),
                sendClientReports = False
              }
          client = Witch.from @ClientOptions @Client opts
      event <- Patrol.Event.fromSomeException $ SomeException (userError "boom")
      mEventId <- withClient client $ captureEvent event
      ids <- Kent.eventIds kent
      case (mEventId, ids) of
        (Just eventId, [kentId]) -> do
          stored <- Kent.getEvent kent kentId
          -- The item header marks this as an event (not a client report).
          Kent.dig ["payload", "header", "type"] stored
            `shouldBe` Just (Aeson.String "event")
          -- The SDK's event id round-trips into the delivered body.
          Kent.dig ["payload", "body", "event_id"] stored
            `shouldBe` Just (Aeson.toJSON eventId)
          -- 'applyClientDefaults' stamps the platform and SDK metadata.
          Kent.dig ["payload", "body", "platform"] stored
            `shouldBe` Just (Aeson.String "haskell")
          (Kent.dig ["payload", "body", "sdk", "name"] stored >>= Kent.asText)
            `shouldSatisfy` isJust
        (Nothing, _) -> expectationFailure "captureEvent returned Nothing"
        (_, _) ->
          expectationFailure $
            "expected exactly one stored event, got " <> show (length ids)
