module Http2CaptureTest where

import Control.Monad (replicateM_)
import Data.Default (def)
import Network.HTTP.Types qualified as Http
import Sentry.TestKit.Gen qualified as Gen
import Sentry.TestKit.Http2Sink qualified as Http2Sink
import Sentry.Transport (FlushResponse (..))
import Sentry.Transport qualified as Transport
import Sentry.Transport.HTTP2.Async (Http2TransportOptions (..))
import Sentry.Transport.HTTP2.Async qualified as Http2Async
import Test.Hspec

-- | Transport options for all specs: disable cert validation so the embedded
-- self-signed certificate is accepted without installing it as a trust anchor.
tlsOpts :: Http2TransportOptions
tlsOpts = def{validateCert = False}

spec_tlsDelivery :: Spec
spec_tlsDelivery = describe "h2-TLS delivery (ALPN negotiation)" do
  it "delivers every envelope and negotiates HTTP/2" $
    Http2Sink.withHttp2Sink \sink -> do
      let dsn = Http2Sink.dsnFor sink "1"
          n = 5 :: Int
      transport <- Http2Async.build tlsOpts Nothing 100 dsn
      replicateM_ n $ Transport.send transport (Gen.sampleEnvelope dsn)
      flushResult <- Transport.flush transport 10
      flushResult `shouldBe` FlushSucceeded
      reqs <- Http2Sink.received sink
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
    Http2Sink.withHttp2Sink \sink -> do
      let dsn = Http2Sink.dsnFor sink "1"
      transport <- Http2Async.build tlsOpts Nothing 100 dsn

      -- First send succeeds (default 200 responder).
      _ <- Transport.send transport (Gen.sampleEnvelope dsn)
      _ <- Transport.flush transport 10
      reqs1 <- Http2Sink.received sink
      length reqs1 `shouldBe` 1

      -- Arm: the next response will be 429 with Retry-After: 60.
      Http2Sink.setResponder sink \_ _ ->
        pure
          Http2Sink.SinkResponse
            { Http2Sink.status = Http.tooManyRequests429,
              Http2Sink.responseHeaders = [("Retry-After", "60")]
            }

      -- Second send reaches the server and triggers the 429 — the worker
      -- updates the rate limiter from the response.
      _ <- Transport.send transport (Gen.sampleEnvelope dsn)
      _ <- Transport.flush transport 10
      reqs2 <- Http2Sink.received sink
      length reqs2 `shouldBe` 2 -- both the 200 and the 429 reached the sink

      -- Restore a 200 responder so the sink would accept more if they arrived.
      Http2Sink.setResponder sink \_ _ -> pure Http2Sink.ok

      -- Third send: the rate limiter (global, 60 s from now) filters the
      -- envelope before it hits the wire — 'filterEnvelope' returns kept=Nothing.
      _ <- Transport.send transport (Gen.sampleEnvelope dsn)
      _ <- Transport.flush transport 10
      reqs3 <- Http2Sink.received sink
      length reqs3 `shouldBe` 2 -- unchanged: the third envelope was filtered

spec_rateLimitSentryHeader :: Spec
spec_rateLimitSentryHeader =
  describe "rate-limit: X-Sentry-Rate-Limits suppresses matching-category sends" do
    it "stops sending error events after X-Sentry-Rate-Limits: 60:error:organization" $
      Http2Sink.withHttp2Sink \sink -> do
        let dsn = Http2Sink.dsnFor sink "1"
        transport <- Http2Async.build tlsOpts Nothing 100 dsn

        -- First send: 200 response carries X-Sentry-Rate-Limits for the error
        -- category with a 60-second window.
        Http2Sink.setResponder sink \_ _ ->
          pure
            Http2Sink.SinkResponse
              { Http2Sink.status = Http.status200,
                Http2Sink.responseHeaders = [("X-Sentry-Rate-Limits", "60:error:organization")]
              }

        _ <- Transport.send transport (Gen.sampleEnvelope dsn)
        _ <- Transport.flush transport 10
        reqs1 <- Http2Sink.received sink
        length reqs1 `shouldBe` 1

        -- Switch back to a plain 200 — the sink would accept more if they arrived.
        Http2Sink.setResponder sink \_ _ -> pure Http2Sink.ok

        -- Second send: sampleEnvelope wraps an error event (DataCategory.Error).
        -- The rate limiter's per-category limit filters it; nothing reaches the wire.
        _ <- Transport.send transport (Gen.sampleEnvelope dsn)
        _ <- Transport.flush transport 10
        reqs2 <- Http2Sink.received sink
        length reqs2 `shouldBe` 1 -- unchanged
