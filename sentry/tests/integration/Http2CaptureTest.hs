-- The HTTP/2 transport is deliberately under test here, so silence its
-- experimental-use warning for this module.
{-# OPTIONS_GHC -Wno-x-sentry-experimental #-}

module Http2CaptureTest where

import Control.Monad (replicateM_)
import Data.Default (def)
import Network.HTTP.Types qualified as Http
import Network.HTTP2.TLS.Client qualified as HTTP2TLS
import Sentry.TestKit.Gen qualified as Gen
import Sentry.TestKit.Sink qualified as Sink
import Sentry.Transport (FlushResponse (..))
import Sentry.Transport qualified as Transport
import Sentry.Transport.HTTP2.Async (Http2Settings (..), Http2TransportOptions (..))
import Sentry.Transport.HTTP2.Async qualified as Http2Async
import Sentry.Transport.HTTP2.Connection (applyHttp2Settings)
import Test.Hspec

-- | Transport options for all specs: disable cert validation so the embedded
-- self-signed certificate is accepted without installing it as a trust anchor.
tlsOpts :: Http2TransportOptions
tlsOpts = def{validateCert = False}

spec_tlsDelivery :: Spec
spec_tlsDelivery = describe "h2-TLS delivery (ALPN negotiation)" do
  it "delivers every envelope and negotiates HTTP/2" $
    Sink.withSink \sink -> do
      let dsn = Sink.dsnFor sink "1"
          n = 5 :: Int
      transport <- Http2Async.build tlsOpts Nothing 100 dsn
      replicateM_ n $ Transport.send transport (Gen.sampleEnvelope dsn)
      flushResult <- Transport.flush transport 10
      flushResult `shouldBe` FlushSucceeded
      reqs <- Sink.received sink
      -- Receipt: all n envelopes arrived.
      length reqs `shouldBe` n
      -- Protocol: every request was served over HTTP/2 (not a silent 1.1 fallback).
      all (\r -> r.httpVersion == Http.http20) reqs `shouldBe` True
      -- Headers: Sentry auth header is present on every request.
      let hasAuth r = any ((== "x-sentry-auth") . fst) r.headers
      all hasAuth reqs `shouldBe` True

spec_rateLimitHttp429 :: Spec
spec_rateLimitHttp429 = describe "rate-limit: HTTP 429 suppresses subsequent sends" do
  it "stops sending after a 429 Retry-After: 60 response" $
    Sink.withSink \sink -> do
      let dsn = Sink.dsnFor sink "1"
      transport <- Http2Async.build tlsOpts Nothing 100 dsn

      -- First send succeeds (default 200 responder).
      _ <- Transport.send transport (Gen.sampleEnvelope dsn)
      _ <- Transport.flush transport 10
      reqs1 <- Sink.received sink
      length reqs1 `shouldBe` 1

      -- Arm: the next response will be 429 with Retry-After: 60.
      Sink.setResponder sink \_ _ ->
        pure
          Sink.SinkResponse
            { Sink.status = Http.tooManyRequests429,
              Sink.responseHeaders = [("Retry-After", "60")]
            }

      -- Second send reaches the server and triggers the 429 — the worker
      -- updates the rate limiter from the response.
      _ <- Transport.send transport (Gen.sampleEnvelope dsn)
      _ <- Transport.flush transport 10
      reqs2 <- Sink.received sink
      length reqs2 `shouldBe` 2 -- both the 200 and the 429 reached the sink

      -- Restore a 200 responder so the sink would accept more if they arrived.
      Sink.setResponder sink \_ _ -> pure Sink.ok

      -- Third send: the rate limiter (global, 60 s from now) filters the
      -- envelope before it hits the wire — 'filterEnvelope' returns kept=Nothing.
      _ <- Transport.send transport (Gen.sampleEnvelope dsn)
      _ <- Transport.flush transport 10
      reqs3 <- Sink.received sink
      length reqs3 `shouldBe` 2 -- unchanged: the third envelope was filtered

spec_rateLimitSentryHeader :: Spec
spec_rateLimitSentryHeader =
  describe "rate-limit: X-Sentry-Rate-Limits suppresses matching-category sends" do
    it "stops sending error events after X-Sentry-Rate-Limits: 60:error:organization" $
      Sink.withSink \sink -> do
        let dsn = Sink.dsnFor sink "1"
        transport <- Http2Async.build tlsOpts Nothing 100 dsn

        -- First send: 200 response carries X-Sentry-Rate-Limits for the error
        -- category with a 60-second window.
        Sink.setResponder sink \_ _ ->
          pure
            Sink.SinkResponse
              { Sink.status = Http.status200,
                Sink.responseHeaders = [("X-Sentry-Rate-Limits", "60:error:organization")]
              }

        _ <- Transport.send transport (Gen.sampleEnvelope dsn)
        _ <- Transport.flush transport 10
        reqs1 <- Sink.received sink
        length reqs1 `shouldBe` 1

        -- Switch back to a plain 200 — the sink would accept more if they arrived.
        Sink.setResponder sink \_ _ -> pure Sink.ok

        -- Second send: sampleEnvelope wraps an error event (DataCategory.Error).
        -- The rate limiter's per-category limit filters it; nothing reaches the wire.
        _ <- Transport.send transport (Gen.sampleEnvelope dsn)
        _ <- Transport.flush transport 10
        reqs2 <- Sink.received sink
        length reqs2 `shouldBe` 1 -- unchanged

-- | Pure unit test for 'applyHttp2Settings': verify that 'Just' overrides
-- replace the corresponding field in the base settings, and 'Nothing' fields
-- leave the base value unchanged.
spec_settingsMapping :: Spec
spec_settingsMapping =
  describe "applyHttp2Settings" do
    it "applies Just overrides and preserves Nothing fields" $ do
      let overrides =
            Http2Settings
              { tcpNoDelay = False, -- no socket-open side effect in this pure test
                pingRateLimit = Just 999,
                emptyFrameRateLimit = Just 42,
                settingsRateLimit = Nothing,
                rstRateLimit = Just 7,
                connectionWindowSize = Just 1048576,
                streamWindowSize = Just 32768,
                maxConcurrentStreams = Just 16
              }
          base = HTTP2TLS.defaultSettings
          result = applyHttp2Settings overrides base

      -- Overridden fields take the supplied value.
      HTTP2TLS.settingsPingRateLimit result `shouldBe` 999
      HTTP2TLS.settingsEmptyFrameRateLimit result `shouldBe` 42
      HTTP2TLS.settingsRstRateLimit result `shouldBe` 7
      HTTP2TLS.settingsConnectionWindowSize result `shouldBe` 1048576
      HTTP2TLS.settingsStreamWindowSize result `shouldBe` 32768
      HTTP2TLS.settingsConcurrentStreams result `shouldBe` 16

      -- Nothing field is preserved from the base.
      HTTP2TLS.settingsSettingsRateLimit result
        `shouldBe` HTTP2TLS.settingsSettingsRateLimit base

-- | Smoke test: build the transport with non-default 'Http2Settings' and
-- confirm that the h2\/TLS handshake still succeeds and envelopes are delivered.
spec_customSettingsDelivery :: Spec
spec_customSettingsDelivery =
  describe "custom Http2Settings: handshake and delivery" do
    it "delivers envelopes with non-default settings applied" $
      Sink.withSink \sink -> do
        let dsn = Sink.dsnFor sink "1"
            -- Use non-default values to exercise the settings path; TCP_NODELAY
            -- is disabled to avoid any socket-option interaction in tests.
            customSettings =
              (def :: Http2Settings)
                { tcpNoDelay = False,
                  pingRateLimit = Just 50,
                  connectionWindowSize = Just (4 * 1024 * 1024)
                }
            opts = tlsOpts{http2Settings = customSettings}
        transport <- Http2Async.build opts Nothing 100 dsn
        replicateM_ 3 $ Transport.send transport (Gen.sampleEnvelope dsn)
        flushResult <- Transport.flush transport 10
        flushResult `shouldBe` FlushSucceeded
        reqs <- Sink.received sink
        length reqs `shouldBe` 3
        all (\r -> r.httpVersion == Http.http20) reqs `shouldBe` True
