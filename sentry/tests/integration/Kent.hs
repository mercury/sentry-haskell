-- | Lifecycle and HTTP client for the @kent-server@ mock Sentry backend.
--
-- This module spawns @kent@ as a subprocess on a known port, waits for it to
-- become ready, and exposes helpers to construct DSNs that point at it and to
-- inspect/clear the events it has received.
module Kent
  ( KentHandle (..),
    withKent,
    dsnFor,
    listEvents,
    flushKent,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, SomeException, throwIO, try)
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Network.HTTP.Client qualified as HttpClient
import Patrol qualified
import Patrol.Type.Dsn qualified as Patrol.Dsn
import System.Directory (findExecutable)
import System.Process.Typed qualified as Process

-- | A handle to a running @kent-server@.
data KentHandle = KentHandle
  { port :: Int,
    manager :: HttpClient.Manager
  }

-- | Thrown when @kent-server@ is missing from @PATH@.
newtype KentNotFound = KentNotFound String
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Spawn @kent-server@ on the given port, wait for it to become ready,
-- run the action, then terminate the subprocess.
--
-- Errors fast with 'KentNotFound' if the binary is missing — no point
-- polling readiness on a server that will never start.
withKent :: Int -> (KentHandle -> IO a) -> IO a
withKent port action = do
  bin <-
    findExecutable "kent-server" >>= \case
      Just b -> pure b
      Nothing ->
        throwIO . KentNotFound $
          "kent-server not found on PATH; run inside the nix dev shell"
  manager <- HttpClient.newManager HttpClient.defaultManagerSettings
  let config =
        Process.setStdout Process.nullStream
          . Process.setStderr Process.nullStream
          $ Process.proc bin ["run", "--host", "127.0.0.1", "--port", show port]
  Process.withProcessTerm config \_ -> do
    let handle = KentHandle{port, manager}
    waitReady handle
    action handle

-- | Poll @GET /api/eventlist/@ until kent responds 200 or we give up.
--
-- Kent boots in well under a second, but just in case something goes wrong we
-- retry for up to 5 seconds.
waitReady :: KentHandle -> IO ()
waitReady handle = go (50 :: Int)
  where
    go 0 =
      throwIO . KentNotFound $
        "kent-server failed to become ready within 5 seconds"
    go n = do
      result <- try @SomeException $ do
        req <- HttpClient.parseRequest (eventListUrl handle)
        _ <- HttpClient.httpNoBody req handle.manager
        pure ()
      case result of
        Right () -> pure ()
        Left _ -> threadDelay 100_000 *> go (n - 1)

-- | A DSN pointing at this kent instance for the given project ID.
--
-- Kent accepts any integer project ID; the public key value is also
-- arbitrary, kent does not validate it.
dsnFor :: KentHandle -> Text -> Patrol.Dsn
dsnFor handle pid =
  Patrol.Dsn.Dsn
    { Patrol.Dsn.protocol = "http",
      Patrol.Dsn.publicKey = "public",
      Patrol.Dsn.secretKey = "",
      Patrol.Dsn.host = "127.0.0.1",
      Patrol.Dsn.port = Just (fromIntegral handle.port),
      Patrol.Dsn.path = "/",
      Patrol.Dsn.projectId = pid
    }

-- | Fetch every event summary kent has stored across all project IDs.
--
-- Kent returns @{"events": [...]}@; each entry has @project_id@,
-- @event_id@, and @summary@ fields.
listEvents :: KentHandle -> IO [Aeson.Value]
listEvents handle = do
  req <- HttpClient.parseRequest (eventListUrl handle)
  resp <- HttpClient.httpLbs req handle.manager
  case Aeson.eitherDecode (HttpClient.responseBody resp) of
    Right (EventList events) -> pure events
    Left err -> fail $ "failed to decode kent eventlist response: " <> err

newtype EventList = EventList [Aeson.Value]

instance Aeson.FromJSON EventList where
  parseJSON = Aeson.withObject "EventList" \o -> EventList <$> o .: "events"

-- | Clear kent's in-memory event store. Useful between test cases.
flushKent :: KentHandle -> IO ()
flushKent handle = do
  req0 <- HttpClient.parseRequest ("http://127.0.0.1:" <> show handle.port <> "/api/flush/")
  let req = req0{HttpClient.method = "POST"}
  _ <- HttpClient.httpNoBody req handle.manager
  pure ()

eventListUrl :: KentHandle -> String
eventListUrl handle =
  "http://127.0.0.1:" <> show handle.port <> "/api/eventlist/"
