-- | THROWAWAY probe: does HTTP/2 multiplexing beat HTTP/1.1 pooling under
-- concurrency, in time and in memory?
--
-- Drives both backends through the __real async executor__, sweeping its
-- concurrency cap ('AsyncExecutor.ExecutorOptions' @concurrency@). The executor
-- prepares each envelope serially on its dispatcher (cheap: rate-limit filter,
-- no client reports here) and then fans the actual send out up to the cap, so
-- the dispatcher no longer hides h2's multiplexing. Only the injected send
-- function differs between backends:
--
--   * h2 — 'Connection.sendEnvelope' on ONE 'Connection.Manager' (one TLS
--     connection, multiplexed streams).
--   * h1 — 'SyncHttp.sendEnvelope' on one @http-client@ 'Sink.tlsManager'
--     (connection pool, up to @cap@ parallel connections).
--
-- Throughput is measured by enqueuing @cap * sendsPerWorker@ envelopes and
-- timing how long the executor takes to drain them (the 'Transport.flush'
-- barrier). The sink injects a fixed per-request delay to stand in for network
-- RTT.
--
-- Modes:
--
-- > sentry-concprobe sweep [sendsPerWorker] [delayMs]
-- >     in-process sink; sweep the concurrency cap and print a throughput table.
-- > sentry-concprobe serve <port> [delayMs]
-- >     run the latency TLS sink on a fixed port, forever (separate process).
-- > sentry-concprobe one <h1|h2> <concurrency> [sendsPerWorker]
-- >     env SINK_PORT=<port>: drive ONE config against an external `serve`
-- >     sink. Run under `+RTS -s` for client-only max residency (the sink's
-- >     memory is in the other process, so it doesn't pollute the number).
--
-- Not part of the SDK — delete it once the question is answered.
module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async qualified as Async
import Control.Monad (replicateM_, void, when)
import Data.Default (def)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Network.HTTP.Types qualified as Http
import Network.Socket qualified as Socket
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS qualified as WarpTLS
import Patrol qualified
import Patrol.Type.Dsn qualified as Patrol.Dsn
import Sentry.TestKit.Gen qualified as Gen
import Sentry.TestKit.Sink qualified as Sink
import Sentry.Transport qualified as Transport
import Sentry.Transport.Delivery (Outcome (..))
import Sentry.Transport.Executor.Async qualified as AsyncExecutor
import Sentry.Transport.HTTP.Request (Compression (Gzip))
import Sentry.Transport.HTTP.Request qualified as Request
import Sentry.Transport.HTTP.Sync qualified as SyncHttp
import Sentry.Transport.HTTP2.Connection (ReconnectDecision (..))
import Sentry.Transport.HTTP2.Connection qualified as Connection
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)
import Text.Printf (printf)
import Text.Read (readMaybe)
import UnliftIO.Exception (bracket)

-- | The executor concurrency caps swept by 'sweep'.
concurrencies :: [Int]
concurrencies = [1, 2, 4, 8, 16, 32, 64, 128]

-- | A backend send function, as injected into the async executor.
type SendFn :: Type
type SendFn = Patrol.Envelope -> IO Outcome

main :: IO ()
main =
  getArgs >>= \case
    ("serve" : portS : rest)
      | Just port <- readMaybe portS ->
          let delayMs = readArg 0 10 rest
           in do
                printf "concprobe: latency sink on 127.0.0.1:%d, delay=%dms\n" port delayMs
                runLatencySink port (delayMs * 1000)
    ("sweep" : rest) -> sweep (readArg 0 100 rest) (readArg 1 10 rest)
    ("one" : backend : concS : rest)
      | Just conc <- readMaybe concS -> runOne backend conc (readArg 0 2000 rest)
    [] -> sweep 100 10
    _ -> die usage

usage :: String
usage =
  unlines
    [ "usage:",
      "  sentry-concprobe sweep [sendsPerWorker] [delayMs]",
      "  sentry-concprobe serve <port> [delayMs]",
      "  sentry-concprobe one <h1|h2> <concurrency> [sendsPerWorker]   (env SINK_PORT)"
    ]

-- | In-process sink; sweep the executor concurrency cap and print an h1-vs-h2
-- throughput table.
sweep :: Int -> Int -> IO ()
sweep perWorker delayMs = do
  printf
    "concprobe sweep: sendsPerWorker=%d delay=%dms (ideal = cap/%dms)\n\n"
    perWorker
    delayMs
    delayMs
  withLatencySink (delayMs * 1000) \port -> do
    let dsn = mkDsn "127.0.0.1" port
        envelope = Gen.sampleEnvelope dsn
    (sendH1, sendH2, h2fails, cleanup) <- mkSenders dsn
    void (sendH1 envelope) *> void (sendH2 envelope) -- pre-warm both connections
    printf "%-6s | %12s | %12s | %8s | %10s\n" ("cap" :: String) ("h1 (req/s)" :: String) ("h2 (req/s)" :: String) ("h2/h1" :: String) ("ideal/s" :: String)
    printf "%s\n" (replicate 60 '-')
    for_ concurrencies \cap -> do
      thr1 <- throughputVia cap perWorker sendH1 envelope
      thr2 <- throughputVia cap perWorker sendH2 envelope
      let ideal = fromIntegral cap / (fromIntegral delayMs / 1000) :: Double
      printf "%-6d | %12.0f | %12.0f | %8.2f | %10.0f\n" cap thr1 thr2 (thr2 / thr1) ideal
    warnFails h2fails
    cleanup

-- | Drive ONE (backend, concurrency) config against an external `serve` sink.
-- Run under @+RTS -s@: only this process's memory is measured, so the figure is
-- the client's, not the sink's.
runOne :: String -> Int -> Int -> IO ()
runOne backend conc perWorker = do
  port <- maybe (die "set SINK_PORT to a running `concprobe serve` port") pure . (>>= readMaybe) =<< lookupEnv "SINK_PORT"
  let dsn = mkDsn "127.0.0.1" port
      envelope = Gen.sampleEnvelope dsn
  (sendH1, sendH2, h2fails, cleanup) <- mkSenders dsn
  (send, label) <- case backend of
    "h1" -> pure (sendH1, "h1")
    "h2" -> pure (sendH2, "h2")
    _ -> die usage
  void (send envelope) -- pre-warm the chosen backend
  thr <- throughputVia conc perWorker send envelope
  printf "concprobe one: %s cap=%d sends=%d  throughput=%.0f req/s\n" (label :: String) conc (conc * perWorker) thr
  warnFails h2fails
  cleanup

-- | Build both backend send functions sharing one DSN. Returns @(sendH1,
-- sendH2, h2FailRef, cleanup)@; each send function is the transport-neutral
-- @'Patrol.Envelope' -> 'IO' 'Outcome'@ injected into an 'AsyncExecutor'.
mkSenders :: Patrol.Dsn -> IO (SendFn, SendFn, IORef Int, IO ())
mkSenders dsn = do
  -- h1: a no-verify http-client manager + a reusable request template; the
  -- send/Outcome pair is exactly what the real async HTTP/1.1 transport injects.
  manager <- Sink.tlsManager
  let template = Request.prepare Gzip dsn
      sendH1 = fmap SyncHttp.toOutcome . SyncHttp.sendEnvelope manager mempty template
  -- h2: one long-lived multiplexed connection; sendEnvelope already returns an
  -- Outcome. Wrap it only to tally network failures for the suspect-numbers warning.
  let endpoint = Connection.mkEndpoint Gzip dsn
  h2 <- Connection.newManager endpoint False 30_000_000 def (pure DontReconnect)
  h2fails <- newIORef (0 :: Int)
  let sendH2 envelope = do
        outcome <- Connection.sendEnvelope h2 envelope
        case outcome of
          NetworkFailure{} -> atomicModifyIORef' h2fails \n -> (n + 1, ())
          _ -> pure ()
        pure outcome
  pure (sendH1, sendH2, h2fails, Connection.closeManager h2)

warnFails :: IORef Int -> IO ()
warnFails ref = do
  fails <- readIORef ref
  when (fails > 0) $ printf "\nWARNING: %d h2 sends failed — numbers suspect\n" fails

-- | Push @cap * perWorker@ sends through a fresh executor at concurrency cap
-- @cap@, then flush; return total/wall-clock. The executor is rebuilt per call
-- (cheap: a dispatcher thread + queue) so the cap can vary across the sweep,
-- while the underlying connection in @sendFn@ stays warm and shared.
--
-- The queue is sized to hold every enqueued send so none are dropped as
-- overflow, and client reports are disabled, so the dispatcher's only serial
-- work is the (trivial) rate-limit prep before it forks each send.
throughputVia :: Int -> Int -> SendFn -> Patrol.Envelope -> IO Double
throughputVia cap perWorker sendFn envelope = do
  let total = cap * perWorker
  -- queueSize = total + 1 leaves room for every send plus the Flush marker, so
  -- nothing is dropped; concurrency = cap is the dimension under test.
  executor <- AsyncExecutor.new (AsyncExecutor.ExecutorOptions (total + 1) cap) Nothing sendFn
  start <- getCurrentTime
  replicateM_ total (void (Transport.send executor envelope))
  flushed <- Transport.flush executor 120
  end <- getCurrentTime
  void (Transport.shutdown executor 5)
  case flushed of
    Transport.FlushSucceeded -> pure ()
    _ -> printf "\nWARNING: flush did not succeed at cap=%d — numbers suspect\n" cap
  let elapsed = realToFrac (diffUTCTime end start) :: Double
      n = fromIntegral total :: Double
  pure (if elapsed > 0 then n / elapsed else 0)

-- | TLS sink (drain, wait @delayUs@, 200) on an OS-assigned free port.
withLatencySink :: Int -> (Int -> IO a) -> IO a
withLatencySink delayUs action =
  bracket Warp.openFreePort (Socket.close . snd) \(port, sock) -> do
    let runner = WarpTLS.runTLSSocket Sink.tlsSettings Warp.defaultSettings sock (latencyApp delayUs)
    Async.withAsync runner \server -> Async.link server >> action port

-- | TLS sink on a fixed port (127.0.0.1), forever.
runLatencySink :: Int -> Int -> IO ()
runLatencySink port delayUs =
  WarpTLS.runTLS Sink.tlsSettings settings (latencyApp delayUs)
  where
    settings = Warp.setHost "127.0.0.1" (Warp.setPort port Warp.defaultSettings)

latencyApp :: Int -> Wai.Application
latencyApp delayUs request respond = do
  _ <- Wai.consumeRequestBodyStrict request
  threadDelay delayUs
  respond (Wai.responseLBS Http.status200 [] mempty)

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

readArg :: Int -> Int -> [String] -> Int
readArg i fallback xs = fromMaybe fallback (readMaybe =<< nth i xs)
  where
    nth n ys = case drop n ys of
      (y : _) -> Just y
      [] -> Nothing
