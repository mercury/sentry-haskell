-- The HTTP/2 connection layer is deliberately under test here (driven
-- concurrently, bypassing the serial executor), so silence its experimental
-- warning for this module.
{-# OPTIONS_GHC -Wno-x-sentry-experimental #-}

-- | Concurrency tests for the HTTP/2 connection manager.
--
-- These drive 'Connection.sendEnvelope' directly from many threads — bypassing
-- the (serial) async executor — to exercise the lock-free 'State'
-- single-flight machine and stream multiplexing under genuine contention.
module Http2ConcurrencyTest where

import Control.Concurrent.Async qualified as Async
import Data.Default (def)
import Network.HTTP.Types qualified as Http
import Sentry.TestKit.Gen qualified as Gen
import Sentry.TestKit.Sink qualified as Sink
import Sentry.Transport.Delivery (Outcome (..))
import Sentry.Transport.HTTP.Request (Compression (Gzip))
import Sentry.Transport.HTTP2.Connection (ReconnectDecision (..))
import Sentry.Transport.HTTP2.Connection qualified as Connection
import Test.Hspec
import UnliftIO.Exception (bracket)

-- | A 30-second handshake timeout, in microseconds.
connectTimeout :: Int
connectTimeout = 30_000_000

spec_connectStorm :: Spec
spec_connectStorm =
  describe "concurrent first sends (single-flight connect)" do
    it "opens exactly one connection and delivers every envelope under a connect storm" $
      Sink.withSink \sink -> do
        let dsn = Sink.dsnFor sink "1"
            endpoint = Connection.mkEndpoint Gzip dsn
            -- Stay under the default SETTINGS_MAX_CONCURRENT_STREAMS (64) so the
            -- storm exercises the connect race, not stream-level backpressure.
            n = 40 :: Int
        bracket
          (Connection.newManager endpoint False connectTimeout def (pure DontReconnect))
          Connection.closeManager
          \mgr -> do
            -- Fire n concurrent *first* sends with no pre-warm: they all race on
            -- the initial connect. Single-flight must collapse that to exactly
            -- one connection while still delivering every envelope.
            outcomes <-
              Async.forConcurrently [1 .. n] \_ ->
                Connection.sendEnvelope mgr (Gen.sampleEnvelope dsn)

            -- Every send got an HTTP response (no NetworkFailure).
            let responded = \case Responded{} -> True; _ -> False
            all responded outcomes `shouldBe` True

            -- All n envelopes reached the sink, every one over HTTP/2.
            reqs <- Sink.received sink
            length reqs `shouldBe` n
            all (\r -> r.httpVersion == Http.http20) reqs `shouldBe` True

            -- The crux: despite the connect storm, exactly one TCP connection
            -- was opened (the multiplexed h2 connection).
            conns <- Sink.connectionsOpened sink
            conns `shouldBe` 1

spec_concurrentReuse :: Spec
spec_concurrentReuse =
  describe "concurrent sends over an established connection" do
    it "multiplexes successive concurrent batches on the same single connection" $
      Sink.withSink \sink -> do
        let dsn = Sink.dsnFor sink "1"
            endpoint = Connection.mkEndpoint Gzip dsn
            batch = 20 :: Int
        bracket
          (Connection.newManager endpoint False connectTimeout def (pure DontReconnect))
          Connection.closeManager
          \mgr -> do
            -- Warm the connection, then run two concurrent batches: all sends
            -- must reuse the one connection (hot path), never reconnect.
            _ <- Connection.sendEnvelope mgr (Gen.sampleEnvelope dsn)
            let runBatch =
                  Async.forConcurrently_ [1 .. batch] \_ ->
                    Connection.sendEnvelope mgr (Gen.sampleEnvelope dsn)
            runBatch
            runBatch

            reqs <- Sink.received sink
            length reqs `shouldBe` (1 + 2 * batch)
            conns <- Sink.connectionsOpened sink
            conns `shouldBe` 1
