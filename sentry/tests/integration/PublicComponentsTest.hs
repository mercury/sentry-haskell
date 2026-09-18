{-# OPTIONS_GHC -Wno-x-sentry-experimental #-}

-- | Byte-fidelity contract tests for the transport pieces a caller can
-- assemble themselves.
--
-- The guarantee an instrumented sender depends on is that the bytes it
-- measured are the bytes that shipped, so this drives both backends against a
-- real sink and compares what arrived with what was prepared.
module PublicComponentsTest where

import Control.Exception (AsyncException (ThreadKilled), bracket, throwIO, try)
import Control.Monad (void)
import Data.ByteString.Lazy qualified as LBS
import Data.Default (def)
import Data.Foldable (for_)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
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
import Sentry.Transport.Delivery qualified as Policy
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
  Delivery.Responded _ status _ -> status `shouldBe` Status.status200
  Delivery.NetworkFailure err -> expectationFailure (show err)

bounded :: IO () -> IO ()
bounded action = timeout 15_000_000 action `shouldReturn` Just ()

-- | Standard builders preserve instrumentation through filtering and reports.
spec_builderObservation :: Spec
spec_builderObservation = describe "HTTP builder observations" do
  for_ ["sync", "async", "h2"] \(backend :: String) ->
    for_ [200, 500] \code ->
      it ("observes prepared bodies and returned outcomes: " <> show (backend, code)) $
        bounded $ Sink.withSink \sink -> withHTTP1 \manager -> do
          -- Make the async report eligible for piggybacking without a sleep.
          now <- getCurrentTime
          reports <- Reports.new (addUTCTime (-60) now)
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
                Right (Delivery.Responded _ status _) -> Status.statusCode status `shouldBe` code
                other -> expectationFailure (show other)
            let attached = [() | attempt <- attempts, Items.EnvelopeItems items <- [attempt.envelope.items], Item.ClientReport _ <- items]
            attached `shouldSatisfy` (not . null)
            void $ Transport.send transport envelope
            Transport.flush transport 5 `shouldReturn` Transport.FlushSucceeded
            later <- readIORef observed
            -- A global limit suppresses events; flush may still send reports.
            let eventAttempts xs = [() | a <- xs, Items.EnvelopeItems items <- [a.envelope.items], Item.Event _ <- items]
            eventAttempts later `shouldBe` eventAttempts attempts

-- | The HTTP response callback exposes headers before the first body read.
-- Injecting the reader failure also verifies that draining is best-effort,
-- while cancellation remains observable by the caller.
spec_responseTiming :: Spec
spec_responseTiming = describe "HTTP response receipt" do
  it "timestamps headers before draining and retains them if draining fails" $
    bounded $ Sink.withSink \sink -> do
      drainingAt <- newIORef Nothing
      let settings =
            (mkManagerSettings (TLSSettingsSimple True False False def) Nothing)
              { HTTP.managerModifyResponse = \response ->
                  pure
                    response
                      { HTTP.responseBody = do
                          now <- getCurrentTime
                          writeIORef drainingAt (Just now)
                          throwIO (HTTP.HttpExceptionRequest HTTP.defaultRequest HTTP.ResponseTimeout)
                      }
              }
      manager <- HTTP.newManager settings
      Sink.setResponder sink \_ _ -> pure Sink.ok{Sink.status = Status.status429, Sink.responseHeaders = [("Retry-After", "60")], Sink.responseBody = "body"}
      let dsn = Sink.dsnFor sink "1"
          request = Request.attach (Request.prepare dsn) (Encoding.encode Encoding.None $ Gen.sampleEnvelope dsn)
      response <- HTTP1.sendRequest manager mempty request
      Just bodyReadAt <- readIORef drainingAt
      case response of
        Delivery.Responded receivedAt status _ -> do
          receivedAt `shouldSatisfy` (<= bodyReadAt)
          status `shouldBe` Status.status429
          (Delivery.interpret response).rateLimits
            `shouldBe` [Policy.allCategoriesUntil (addUTCTime 60 receivedAt)]
        other -> expectationFailure (show other)

  it "propagates unexpected exceptions from the body reader" $
    bounded $ Sink.withSink \sink -> do
      let settings =
            (mkManagerSettings (TLSSettingsSimple True False False def) Nothing)
              { HTTP.managerModifyResponse = \response ->
                  pure response{HTTP.responseBody = throwIO (userError "unexpected reader failure")}
              }
      manager <- HTTP.newManager settings
      let dsn = Sink.dsnFor sink "1"
          request = Request.attach (Request.prepare dsn) (Encoding.encode Encoding.None $ Gen.sampleEnvelope dsn)
      HTTP1.sendRequest manager mempty request
        `shouldThrow` (== userError "unexpected reader failure")

  it "does not swallow cancellation while draining" $
    bounded $ Sink.withSink \sink -> do
      let settings =
            (mkManagerSettings (TLSSettingsSimple True False False def) Nothing)
              { HTTP.managerModifyResponse = \response ->
                  pure response{HTTP.responseBody = throwIO ThreadKilled}
              }
      manager <- HTTP.newManager settings
      let dsn = Sink.dsnFor sink "1"
          request = Request.attach (Request.prepare dsn) (Encoding.encode Encoding.None $ Gen.sampleEnvelope dsn)
      result <- try @AsyncException $ HTTP1.sendRequest manager mempty request
      case result of
        Left exception -> exception `shouldBe` ThreadKilled
        Right response -> expectationFailure ("expected cancellation, got " <> show response)

  for_ ["h1", "h2"] \(backend :: String) ->
    it ("retains rate-limit headers from a truncated body on " <> backend) $
      bounded $ Sink.withSink \sink -> do
        Sink.setResponder sink \_ _ ->
          pure
            Sink.ok
              { Sink.status = Status.status429,
                Sink.responseHeaders = [("Retry-After", "60"), ("Content-Length", "10")] <> [("Connection", "close") | backend == "h1"],
                Sink.responseBody = "x"
              }
        let dsn = Sink.dsnFor sink "1"
            body = Encoding.encode Encoding.None (Gen.sampleEnvelope dsn)
            endpoint = HTTP2.mkEndpoint Encoding.None dsn
        response <-
          if backend == "h1"
            then withHTTP1 \manager -> HTTP1.sendRequest manager mempty (Request.attach (Request.prepare dsn) body)
            else bracket (HTTP2.newManager endpoint False 5_000_000 def (pure HTTP2.DontReconnect)) HTTP2.closeManager \manager ->
              HTTP2.sendRequest manager (HTTP2.buildRequest endpoint body)
        case response of
          Delivery.Responded receivedAt status _ -> do
            status `shouldBe` Status.status429
            (Delivery.interpret response).rateLimits
              `shouldBe` [Policy.allCategoriesUntil (addUTCTime 60 receivedAt)]
          other -> expectationFailure (show other)
