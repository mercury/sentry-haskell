{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
-- Explicit manager cleanup keeps test resources bounded despite deprecated closeManager.
{-# OPTIONS_GHC -Wno-x-sentry-experimental -Wno-deprecations #-}

module DeliveryOutcomeTest where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Default (def)
import Data.Foldable (for_, toList)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Network.Connection (TLSSettings (TLSSettingsSimple))
import Network.HTTP.Client qualified as Http
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Types qualified as Status
import Patrol.Type.ClientReport qualified as Report
import Patrol.Type.DataCategory qualified as Category
import Patrol.Type.DiscardedEvent qualified
import Patrol.Type.Envelope qualified as Envelope
import Patrol.Type.Item qualified as Item
import Patrol.Type.Items qualified as Items
import Sentry.ClientReport qualified as Reports
import Sentry.Test qualified as Test
import Sentry.TestKit.Gen qualified as Gen
import Sentry.TestKit.Sink qualified as Sink
import Sentry.Transport qualified as Transport
import Sentry.Transport.Executor.RateLimiter qualified as RateLimiter
import Sentry.Transport.HTTP.Async qualified as Async
import Sentry.Transport.HTTP.Sync qualified as Sync
import Sentry.Transport.HTTP2.Async qualified as Http2
import System.Timeout (timeout)
import Test.Hspec

spec_outcomes :: Spec
spec_outcomes = describe "delivery outcomes across TLS transports" do
  for_ ["sync", "async", "h2"] \backend -> do
    for_ [200, 204, 429, 400, 413, 500] \code ->
      it (backend <> " accounts HTTP " <> show code) $
        bounded $ withTransport backend \sink reports transport -> do
          Sink.setResponder sink \_ _ -> pure Sink.ok{Sink.status = Status.mkStatus code "test"}
          result <- Transport.send transport (Gen.sampleEnvelope (Sink.dsnFor sink "1"))
          result `shouldBe` if backend == "sync" then (if code < 300 then Transport.SendProcessed else Transport.SendFailed_Other) else Transport.SendProcessed
          Transport.flush transport 5 `shouldReturn` Transport.FlushSucceeded
          requests <- Sink.received sink
          -- No retry: at most one additional standalone report request.
          length requests `shouldBe` if code >= 300 && code /= 429 && backend /= "sync" then 2 else 1
          totals <- allDrops sink reports
          totals `shouldBe` if code >= 300 && code /= 429 then [discard "send_error" 1] else []
    for_ [200, 429] \code ->
      it (backend <> " learns limits from " <> show code <> " and counts local suppression once") $
        bounded $ withTransport backend \sink reports transport -> do
          Sink.setResponder sink \_ _ -> pure Sink.ok{Sink.status = Status.mkStatus code "test", Sink.responseHeaders = [("X-Sentry-Rate-Limits", "60:error:organization")]}
          let envelope = Gen.sampleEnvelope (Sink.dsnFor sink "1")
          _ <- Transport.send transport envelope
          Transport.flush transport 5 `shouldReturn` Transport.FlushSucceeded
          first <- Sink.received sink
          length first `shouldBe` 1
          allDrops sink reports `shouldReturn` []
          result <- Transport.send transport envelope
          result `shouldBe` if backend == "sync" then Transport.SendFailed_Other else Transport.SendProcessed
          Transport.flush transport 5 `shouldReturn` Transport.FlushSucceeded
          totals <- allDrops sink reports
          totals `shouldBe` [discard "ratelimit_backoff" 1]

-- | Include reports already drained into requests, even if the server rejected them.
allDrops :: Sink.SinkHandle -> Reports.ClientReports -> IO [Aeson.Value]
allDrops sink reports = do
  requests <- Sink.received sink
  now <- getCurrentTime
  remaining <- Reports.takePending reports now True
  let wire =
        [ entry
        | request <- requests,
          Aeson.Object body <- mapMaybe Aeson.decode (LBS.lines (Sink.decodeBody request)),
          Just (Aeson.Array entries) <- [KeyMap.lookup "discarded_events" body],
          entry <- toList entries
        ]
  pure $ wire <> maybe [] (map Aeson.toJSON . (.discardedEvents)) remaining

-- | Expected wire entry, compared as generic JSON without a protocol parser.
discard :: Text -> Int -> Aeson.Value
discard reason quantity = Aeson.object ["reason" .= reason, "category" .= ("error" :: Text), "quantity" .= quantity]

bounded :: IO () -> IO ()
bounded action = timeout 15_000_000 action `shouldReturn` Just ()

withTransport :: String -> (Sink.SinkHandle -> Reports.ClientReports -> Transport.SomeTransport -> IO ()) -> IO ()
withTransport backend action = Sink.withSink \sink ->
  bracket (Http.newManager (mkManagerSettings (TLSSettingsSimple True False False def) Nothing)) Http.closeManager \manager -> do
    reports <- Reports.new
    let dsn = Sink.dsnFor sink "1"
        build = case backend of
          "sync" -> Transport.SomeTransport <$> Sync.build def (Just reports) manager dsn
          "async" -> Transport.SomeTransport <$> Async.build def (Just reports) 32 manager dsn
          _ -> Transport.SomeTransport <$> Http2.build def{Http2.validateCert = False} (Just reports) 32 dsn
    bracket build (\transport -> void $ Transport.shutdown transport 5) (action sink reports)

spec_networkFailure :: Spec
spec_networkFailure = describe "synchronous injected network failures" do
  it "returns the failure and excludes piggybacked reports from attempted counts" $
    bounded $ withTransport "sync" \sink reports _ -> do
      limiter <- newIORef RateLimiter.new
      attempted <- newIORef []
      let transport = Sync.SyncHttpTransport limiter (\attempt -> modifyIORef' attempted (attempt :) *> pure (Left Http.ConnectionTimeout)) (Just reports)
          envelope = Gen.sampleEnvelope (Sink.dsnFor sink "1")
      Reports.record reports Reports.BeforeSend Category.Error 2
      Transport.send transport envelope `shouldReturn` Transport.SendFailed_Other
      sent <- readIORef attempted
      length sent `shouldBe` 1
      let attached = [r | e <- sent, Items.EnvelopeItems items <- [e.items], Item.ClientReport r <- items]
      fmap (fmap (.quantity) . (.discardedEvents)) attached `shouldBe` [[2]]
      drops <- allDrops sink reports
      drops `shouldBe` [discard "network_error" 1]
      -- A standalone report failure must not report itself.
      for_ attached \report -> do
        let reportOnly = envelope{Envelope.items = Items.EnvelopeItems [Item.ClientReport report]}
        Transport.send transport reportOnly `shouldReturn` Transport.SendFailed_Other
      allDrops sink reports `shouldReturn` []
  it "works with reports disabled" do
    limiter <- newIORef RateLimiter.new
    let transport = Sync.SyncHttpTransport limiter (const $ pure $ Left Http.ConnectionTimeout) Nothing
    Transport.send transport (Gen.sampleEnvelope Test.TEST_DSN) `shouldReturn` Transport.SendFailed_Other
  it "counts locally suppressed events separately when the remaining report fails" $
    bounded $ withTransport "sync" \sink reports _ -> do
      now <- getCurrentTime
      limiter <- newIORef (RateLimiter.updateFromSentryHeader RateLimiter.new now "60:error:organization")
      attempted <- newIORef []
      let transport = Sync.SyncHttpTransport limiter (\e -> modifyIORef' attempted (e :) *> pure (Left Http.ConnectionTimeout)) (Just reports)
          report = Report.ClientReport Nothing []
          envelope = Reports.attach report (Gen.sampleEnvelope (Sink.dsnFor sink "1"))
      Transport.send transport envelope `shouldReturn` Transport.SendFailed_Other
      sent <- readIORef attempted
      let drops = [d | e <- sent, Items.EnvelopeItems items <- [e.items], Item.ClientReport r <- items, d <- r.discardedEvents]
      fmap (\d -> (d.reason, d.quantity)) drops `shouldBe` [("ratelimit_backoff", 1)]
      -- Local drop was drained into the attempted report, not a network_error.
      allDrops sink reports `shouldReturn` []
