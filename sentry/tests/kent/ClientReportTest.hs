module ClientReportTest where

import Control.Exception (bracket)
import Control.Monad (replicateM)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Default (def)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Patrol.Type.Event qualified as Patrol.Event
import Sentry.Capture (captureEvent)
import Sentry.Client qualified as Client
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Client.Options.Dsn qualified as Dsn
import Sentry.ClientReport qualified as ClientReport
import Sentry.Init qualified as Init
import Sentry.Scope.IO (withClient)
import Sentry.TestKit.Kent qualified as Kent
import Sentry.Transport (FlushResponse (..), ShutdownResponse (..), SomeTransport (..))
import Sentry.Transport qualified as Transport
import Sentry.Transport.HTTP.Async qualified as AsyncHttpTransport
import Test.Hspec
import UnliftIO.Exception (toException)
import Witch qualified

spec_clientReport :: Spec
spec_clientReport = describe "client report delivery" do
  it "reports locally dropped events to the server on flush" $
    Kent.withKent \kent -> do
      Kent.flushKent kent
      let dsn = Kent.dsnFor kent "1"
      -- Client reports enabled on the transport; beforeSend drops every event,
      -- so each capture records a 'before_send' discard but sends no event.
      clientReports <- ClientReport.new
      transport <- AsyncHttpTransport.build def (Just clientReports) Nothing 100 kent.manager dsn
      let opts =
            (def @ClientOptions)
              { dsn = Dsn.Explicit dsn,
                transport = Just (Witch.from (SomeTransport transport)),
                sendClientReports = True,
                beforeSend = Just (const Nothing)
              }
      client <- Client.new opts
      let n = 3 :: Int
      events <-
        replicateM n $
          Patrol.Event.fromSomeException . toException $
            userError "boom"
      sentIds <- traverse (\e -> withClient client $ captureEvent e) events
      -- Nothing was delivered as an event.
      catMaybes sentIds `shouldBe` []
      -- Flush force-drains the accumulator, sending a standalone client report.
      flushResult <- Transport.flush transport 5
      flushResult `shouldBe` FlushSucceeded
      stored <- Kent.eventIds kent >>= traverse (Kent.getEvent kent)
      let receivedReports =
            [ report
            | report <- stored,
              Kent.dig ["payload", "header", "type"] report
                == Just (Aeson.String "client_report")
            ]
      case receivedReports of
        [report] ->
          Kent.dig ["payload", "body", "discarded_events"] report
            `shouldBe` Just
              ( Aeson.toJSON
                  [ Aeson.object
                      [ "reason" .= ("before_send" :: Text),
                        "category" .= ("error" :: Text),
                        "quantity" .= (n :: Int)
                      ]
                  ]
              )
        _ ->
          expectationFailure $
            "expected exactly one client report, got " <> show (length receivedReports)

  it "client handle close drains pending reports without a preliminary flush" $
    Kent.withKent \kent -> do
      Kent.flushKent kent
      let dsn = Kent.dsnFor kent "1"
      reports <- ClientReport.new
      transport <- AsyncHttpTransport.build def (Just reports) Nothing 100 kent.manager dsn
      let opts =
            def
              { dsn = Dsn.Explicit dsn,
                transport = Just (Witch.from (SomeTransport transport)),
                sendClientReports = True,
                shutdownTimeout = 5,
                beforeSend = Just (const Nothing)
              }
      bracket (Init.acquireClient opts) Init.close \handle -> do
        event <- Patrol.Event.fromSomeException (toException (userError "dropped on close"))
        withClient (Init.clientOf handle) (captureEvent event) `shouldReturn` Nothing
        Init.close handle `shouldReturn` ShutdownSucceeded
        stored <- Kent.eventIds kent >>= traverse (Kent.getEvent kent)
        let received = [report | report <- stored, Kent.dig ["payload", "header", "type"] report == Just (Aeson.String "client_report")]
        fmap (Kent.dig ["payload", "body", "discarded_events"]) received
          `shouldBe` [Just (Aeson.toJSON [Aeson.object ["reason" .= ("before_send" :: Text), "category" .= ("error" :: Text), "quantity" .= (1 :: Int)]])]
