-- This harness deliberately exercises the experimental HTTP/2 transport, so
-- silence its experimental-use warning.
{-# OPTIONS_GHC -Wno-x-sentry-experimental #-}

-- | End-to-end profiling harness for the HTTP transports.
--
-- Pushes a fixed number of envelopes through one of the HTTP transports against
-- a TLS sink, then flushes and shuts the transport down.  It is a single
-- deterministic pass so it can be driven by both @hyperfine@ and under the
-- profiling RTS (@+RTS -p\/-hc\/-l@).
--
-- == What is compared
--
-- Both legs talk to the __same TLS sink__ ('Sentry.TestKit.Sink') over the same
-- wire — Sentry is always TLS in production, so there is no plaintext leg.  The
-- sink advertises ALPN @h2@ + @http\/1.1@; the client picks:
--
--   * @h1@ — HTTP\/1.1 over TLS via @http-client@ ('Sentry.Transport.HTTP.Sync'
--     / '…HTTP.Async'), selected by the @mode@ argument.
--   * @h2@ — HTTP\/2 over TLS ('Sentry.Transport.HTTP2.Async'); always async,
--     so the @mode@ argument is ignored.
--
-- == Sink selection
--
-- If @SENTRY_PROFILE_SINK_PORT@ is set, the harness talks to an already-running
-- standalone @sentry-sink@ on @SENTRY_PROFILE_SINK_HOST@ (default @127.0.0.1@)
-- so the sink's CPU\/allocation stays out of the profiled process.  Otherwise it
-- spins up an in-process discarding TLS sink (convenient for a one-shot
-- @just profile-prof@, but then the profile includes the server side).
--
-- Usage:
--
-- > sentry-profile <sync|async> [count] [queueSize] [payloadBytes]
-- >   env SENTRY_PROFILE_BACKEND   h1 | h2   (default h2)
-- >   env SENTRY_PROFILE_SINK_HOST host       (default 127.0.0.1)
-- >   env SENTRY_PROFILE_SINK_PORT port        (external sink; else in-process)
module Main where

import Control.Concurrent (threadDelay)
import Data.Default (def)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Network.Connection (TLSSettings (TLSSettings))
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.TLS (ClientHooks (..), ClientParams (..), defaultParamsClient)
import Patrol qualified
import Patrol.Type.Dsn qualified as Patrol.Dsn
import Sentry.TestKit.Gen qualified as Gen
import Sentry.TestKit.Sink qualified as Sink
import Sentry.Transport qualified as Transport
import Sentry.Transport.Executor.Async (ExecutorOptions (ExecutorOptions))
import Sentry.Transport.HTTP.Async qualified as AsyncHttp
import Sentry.Transport.HTTP.Sync qualified as SyncHttp
import Sentry.Transport.HTTP2.Async (Http2TransportOptions (..))
import Sentry.Transport.HTTP2.Async qualified as Http2
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- | Which HTTP\/1.1 transport to exercise (h2 is always async).
type Mode :: Type
data Mode = Sync | Async

-- | Which transport family to exercise.
type Backend :: Type
data Backend = H1 | H2

-- | Tallied outcomes from a run.
type Counts :: Type
data Counts = Counts
  { -- | Envelopes accepted by the transport (every event, barring shutdown).
    processed :: !Int,
    -- | Backpressure events: a 'Transport.SendFailed_QueueFull' that we paused
    -- on and retried (so the event still went through).
    retries :: !Int,
    -- | Events abandoned because the transport reported shutdown mid-run.
    shutdownDropped :: !Int
  }

-- | Parsed harness configuration.
type Config :: Type
data Config = Config
  { mode :: Mode,
    count :: Int,
    queueSize :: Int,
    payloadBytes :: Int
  }

defaultCount :: Int
defaultCount = 5000

main :: IO ()
main = do
  cfg <- parseArgs =<< getArgs
  backend <- parseBackend . fromMaybe "h2" =<< lookupEnv "SENTRY_PROFILE_BACKEND"
  host <- fromMaybe "127.0.0.1" <$> lookupEnv "SENTRY_PROFILE_SINK_HOST"
  externalPort <- (>>= readMaybe) <$> lookupEnv "SENTRY_PROFILE_SINK_PORT"
  -- Either talk to a standalone sink (preferred — keeps the server out of the
  -- profile) or spin up an in-process one for a quick standalone run.
  let withDsn act = case externalPort of
        Just port -> act (mkDsn host port)
        Nothing -> Sink.withDiscardingSink \port -> act (mkDsn "127.0.0.1" port)
  withDsn \dsn -> case backend of
    H1 -> do
      manager <- newTlsManager host
      runH1 cfg manager dsn
    H2 -> runH2 cfg dsn

-- | Drive a profiling run against the HTTP\/2 transport.
--
-- The HTTP\/2 transport is always asynchronous; the 'Mode' argument is ignored.
runH2 :: Config -> Patrol.Dsn -> IO ()
runH2 cfg dsn = do
  let envelope = mkEnvelope cfg dsn
      opts = def{validateCert = False}
  putStrLn $
    "sentry-profile: h2 count="
      <> show cfg.count
      <> " queue="
      <> show cfg.queueSize
      <> " payloadBytes="
      <> show cfg.payloadBytes
  start <- getCurrentTime
  transport <- Http2.build opts Nothing (ExecutorOptions cfg.queueSize 1) dsn
  counts <- drive transport cfg.count envelope
  _ <- Transport.flush transport 300
  _ <- Transport.shutdown transport 300
  end <- getCurrentTime
  report counts (realToFrac (diffUTCTime end start))

-- | Drive a profiling run against the HTTP\/1.1 transport (sync or async).
runH1 :: Config -> HttpClient.Manager -> Patrol.Dsn -> IO ()
runH1 cfg manager dsn = do
  let envelope = mkEnvelope cfg dsn
  putStrLn $
    "sentry-profile: h1-"
      <> modeLabel cfg.mode
      <> " count="
      <> show cfg.count
      <> (case cfg.mode of Async -> " queue=" <> show cfg.queueSize; Sync -> "")
      <> " payloadBytes="
      <> show cfg.payloadBytes
  start <- getCurrentTime
  counts <- case cfg.mode of
    Sync -> do
      transport <- SyncHttp.build def Nothing manager dsn
      counts <- drive transport cfg.count envelope
      -- Generous timeouts: flush must drain the whole queue through real TLS.
      _ <- Transport.flush transport 300
      _ <- Transport.shutdown transport 300
      pure counts
    Async -> do
      transport <- AsyncHttp.build def Nothing (ExecutorOptions cfg.queueSize 1) manager dsn
      counts <- drive transport cfg.count envelope
      _ <- Transport.flush transport 300
      _ <- Transport.shutdown transport 300
      pure counts
  end <- getCurrentTime
  report counts (realToFrac (diffUTCTime end start))

-- | Push @total@ envelopes all the way through the transport, applying
-- backpressure on a full queue so every event traverses the worker rather than
-- being dropped.
drive :: (Transport.Transport t) => t -> Int -> Patrol.Envelope -> IO Counts
drive transport total envelope = go total (Counts 0 0 0)
  where
    go 0 !counts = pure counts
    go k !counts =
      Transport.send transport envelope >>= \case
        Transport.SendProcessed -> go (k - 1) counts{processed = counts.processed + 1}
        -- Queue full: back off briefly (rather than hot-spinning, which would
        -- dominate the profile) and retry the same envelope; k is not
        -- decremented, so the event is delivered rather than dropped.
        Transport.SendFailed_QueueFull -> threadDelay 100 *> go k counts{retries = counts.retries + 1}
        -- Transport shut down underneath us: abandon the remaining k events.
        Transport.SendFailed_Shutdown -> pure counts{shutdownDropped = counts.shutdownDropped + k}

report :: Counts -> Double -> IO ()
report counts elapsed = do
  printf "  elapsed: %.3fs\n" elapsed
  let throughput = if elapsed > 0 then fromIntegral counts.processed / elapsed else 0 :: Double
  printf "  processed: %d  (%.0f/s)\n" counts.processed throughput
  printf "  queue-full retries: %d\n" counts.retries
  printf "  shutdown drops: %d\n" counts.shutdownDropped

-- | An @https@ DSN pointing at the sink for project @1@.
mkDsn :: String -> Int -> Patrol.Dsn
mkDsn host port =
  Patrol.Dsn.Dsn
    { Patrol.Dsn.protocol = "https",
      Patrol.Dsn.publicKey = "public",
      Patrol.Dsn.secretKey = "",
      Patrol.Dsn.host = Text.pack host,
      Patrol.Dsn.port = Just (fromIntegral port),
      Patrol.Dsn.path = "/",
      Patrol.Dsn.projectId = "1"
    }

-- | An @http-client@ manager that speaks TLS but skips certificate validation,
-- so the sink's embedded self-signed cert is accepted without a trust anchor.
newTlsManager :: String -> IO HttpClient.Manager
newTlsManager host =
  HttpClient.newManager (mkManagerSettings (TLSSettings params) Nothing)
  where
    base = defaultParamsClient host ""
    -- Returning no failures accepts any certificate (dev/profiling only).
    params = base{clientHooks = (clientHooks base){onServerCertificate = \_ _ _ _ -> pure []}}

mkEnvelope :: Config -> Patrol.Dsn -> Patrol.Envelope
mkEnvelope cfg dsn
  | cfg.payloadBytes > 0 = Gen.messageEnvelope dsn cfg.payloadBytes
  | otherwise = Gen.sampleEnvelope dsn

modeLabel :: Mode -> String
modeLabel = \case
  Sync -> "sync"
  Async -> "async"

parseBackend :: String -> IO Backend
parseBackend = \case
  "h1" -> pure H1
  "h2" -> pure H2
  other -> die $ "unknown SENTRY_PROFILE_BACKEND " <> show other <> " (expected h1 or h2)"

parseArgs :: [String] -> IO Config
parseArgs = \case
  [] -> die usage
  (m : rest) -> do
    mode <- case m of
      "sync" -> pure Sync
      "async" -> pure Async
      _ -> die usage
    let count = positional 0 defaultCount rest
    pure
      Config
        { mode,
          count,
          -- Default the queue to the event count so the default run applies no
          -- backpressure; pass a smaller value explicitly to study a bounded
          -- queue under load.
          queueSize = positional 1 count rest,
          payloadBytes = positional 2 0 rest
        }
  where
    positional i fallback xs = fromMaybe fallback (readMaybe =<< nth i xs)
    nth i xs = case drop i xs of
      (x : _) -> Just x
      [] -> Nothing

usage :: String
usage =
  unlines
    [ "usage: sentry-profile <sync|async> [count] [queueSize] [payloadBytes]",
      "",
      "  count         number of envelopes to send (default 5000)",
      "  queueSize     async bounded-queue capacity (async only; default: count,",
      "                i.e. no backpressure)",
      "  payloadBytes  if > 0, send a message event of this size instead of the",
      "                default exception event (default 0)",
      "",
      "  env SENTRY_PROFILE_BACKEND    h1 (HTTP/1.1 over TLS) | h2 (HTTP/2); default h2",
      "  env SENTRY_PROFILE_SINK_HOST  sink host (default 127.0.0.1)",
      "  env SENTRY_PROFILE_SINK_PORT  talk to a standalone sentry-sink on this",
      "                                port; if unset, spin up an in-process sink"
    ]
