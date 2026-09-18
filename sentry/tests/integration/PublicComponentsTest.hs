{-# OPTIONS_GHC -Wno-x-sentry-experimental #-}

-- | Byte-fidelity contract tests for the transport pieces a caller can
-- assemble themselves.
--
-- The guarantee an instrumented sender depends on is that the bytes it
-- measured are the bytes that shipped, so this drives both backends against a
-- real sink and compares what arrived with what was prepared.
module PublicComponentsTest where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.ByteString.Lazy qualified as LBS
import Data.Default (def)
import Data.Foldable (for_)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Network.Connection (TLSSettings (TLSSettingsSimple))
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Types qualified as Status
import Patrol.Type.DataCategory qualified as Category
import Patrol.Type.Envelope qualified
import Patrol.Type.Item qualified as Item
import Patrol.Type.Items qualified as Items
import Sentry.ClientReport qualified as Reports
import Sentry.TestKit.Gen qualified as Gen
import Sentry.TestKit.Sink qualified as Sink
import Sentry.Transport qualified as Transport
import Sentry.Transport.Encoding qualified as Encoding
import Sentry.Transport.HTTP.Async qualified as Async
import Sentry.Transport.HTTP.Delivery qualified as Delivery
import Sentry.Transport.HTTP.Request qualified as Request
import Sentry.Transport.HTTP.Sync qualified as HTTP1
import Sentry.Transport.HTTP2.Async qualified as Async2
import Sentry.Transport.HTTP2.Connection qualified as HTTP2
import Sentry.Transport.Instrument qualified as Instrument
import System.Timeout (timeout)
import Test.Hspec

spec_publicBodies :: Spec
spec_publicBodies = describe "public encoded bodies and native requests" do
  for_ [Encoding.None, Encoding.Gzip] \compression ->
    it ("sends the same prepared bytes with matching headers on both backends: " <> show compression) $
      bounded $
        Sink.withSink \sink -> do
          let dsn = Sink.dsnFor sink "1"
              envelope = Gen.sampleEnvelope dsn
              encoded = Encoding.encode compression envelope
              body = Encoding.fromBytes compression (Encoding.bytes encoded)
              template = Request.prepare dsn
              -- Deliberately the opposite of the body's encoding: the request
              -- builders must follow the body, not this default.
              endpoint = HTTP2.mkEndpoint Encoding.None dsn
              request = Request.attach template body
          withHTTP1 \manager -> HTTP1.sendRequest manager mempty request >>= assertOK
          bracket (HTTP2.newManager endpoint False 5_000_000 def (pure HTTP2.DontReconnect)) HTTP2.closeManager \manager ->
            HTTP2.sendRequest manager (HTTP2.buildRequest endpoint body) >>= assertOK
          received <- Sink.received sink
          length received `shouldBe` 2
          for_ received \r -> do
            r.body `shouldBe` Encoding.bytes body
            Sink.decodeBody r `shouldBe` Encoding.bytes (Encoding.encode Encoding.None envelope)
            lookup "content-encoding" r.headers `shouldBe` case compression of
              Encoding.None -> Nothing
              Encoding.Gzip -> Just "gzip"

-- | @http-client@ closes managers itself once they fall out of use, so this
-- deliberately does not call the deprecated 'HTTP.closeManager'.
withHTTP1 :: (HTTP.Manager -> IO a) -> IO a
withHTTP1 action =
  HTTP.newManager (mkManagerSettings (TLSSettingsSimple True False False def) Nothing) >>= action

assertOK :: Delivery.Outcome -> IO ()
assertOK = \case
  Delivery.Responded status _ -> status `shouldBe` Status.status200
  Delivery.NetworkFailure err -> expectationFailure (show err)

bounded :: IO () -> IO ()
bounded action = timeout 15_000_000 action `shouldReturn` Just ()

-- | Standard builders preserve instrumentation through filtering and reports.
spec_builderObservation :: Spec
spec_builderObservation = describe "HTTP builder observations" do
  for_ ["sync", "async", "h2"] \backend ->
    for_ [200, 500] \code ->
      it ("observes prepared bodies and returned outcomes: " <> show (backend, code)) $
        bounded $ Sink.withSink \sink -> withHTTP1 \manager -> do
          reports <- Reports.new
          -- Make the async report eligible for piggybacking without a sleep.
          now <- getCurrentTime
          Reports.record reports Reports.BeforeSend Category.Error 1
          void $ Reports.takePending reports (addUTCTime (-60) now) True
          Reports.record reports Reports.BeforeSend Category.Error 2
          observed <- newIORef []
          let report attempt = atomicModifyIORef' observed (\xs -> (xs <> [attempt], ()))
              opts = def{HTTP1.wrapSender = Instrument.observing report}
              dsn = Sink.dsnFor sink "1"
              envelope = Gen.sampleEnvelope dsn
              build = case backend of
                "sync" -> Transport.SomeTransport <$> HTTP1.build opts (Just reports) Nothing manager dsn
                "async" -> Transport.SomeTransport <$> Async.build opts (Just reports) Nothing 32 manager dsn
                _ -> Transport.SomeTransport <$> Async2.build def{Async2.validateCert = False, Async2.wrapSender = Instrument.observing report} (Just reports) Nothing 32 dsn
          Sink.setResponder sink \_ _ ->
            pure
              Sink.ok
                { Sink.status = Status.mkStatus code "test",
                  Sink.responseHeaders = [("Retry-After", "60")]
                }
          bracket build (\t -> void $ Transport.shutdown t 5) \transport -> do
            void $ Transport.send transport envelope
            Transport.flush transport 5 `shouldReturn` Transport.FlushSucceeded
            attempts <- readIORef observed
            requests <- Sink.received sink
            -- Async may send a standalone report during flush.
            length attempts `shouldBe` length requests
            attempts `shouldSatisfy` (not . null)
            for_ (zip attempts requests) \(attempt, request) -> do
              attempt.size `shouldBe` LBS.length request.body
              request.body `shouldBe` Encoding.bytes (Encoding.encode attempt.compression attempt.envelope)
              case attempt.outcome of
                Right (Delivery.Responded status _) -> Status.statusCode status `shouldBe` code
                other -> expectationFailure (show other)
            let attached = [() | attempt <- attempts, Items.EnvelopeItems items <- [attempt.envelope.items], Item.ClientReport _ <- items]
            attached `shouldSatisfy` (not . null)
            void $ Transport.send transport envelope
            Transport.flush transport 5 `shouldReturn` Transport.FlushSucceeded
            later <- readIORef observed
            -- A global limit suppresses events; flush may still send reports.
            let eventAttempts xs = [() | a <- xs, Items.EnvelopeItems items <- [a.envelope.items], Item.Event _ <- items]
            eventAttempts later `shouldBe` eventAttempts attempts
