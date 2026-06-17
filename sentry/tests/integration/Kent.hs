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
    getEvent,
    eventIds,
    flushKent,

    -- * JSON navigation
    dig,
    asText,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, SomeException, bracket, throwIO, try)
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.HTTP.Client qualified as HttpClient
import Network.Socket qualified as Socket
import Patrol qualified
import Patrol.Type.Dsn qualified as Patrol.Dsn
import System.Directory (findExecutable)
import System.Process.Typed qualified as Process

-- | A handle to a running @kent-server@.
type KentHandle :: Type
data KentHandle = KentHandle
  { port :: Int,
    manager :: HttpClient.Manager
  }

-- | Thrown when @kent-server@ is missing from @PATH@.
type KentNotFound :: Type
newtype KentNotFound = KentNotFound String
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Spawn @kent-server@ on an automatically-chosen free port, wait for it to
-- become ready, run the action, then terminate the subprocess.
--
-- The port is picked by 'getFreePort' rather than supplied by the caller, so
-- concurrently-running tests never collide on a fixed port.
--
-- Errors fast with 'KentNotFound' if the binary is missing — no point
-- polling readiness on a server that will never start.
withKent :: (KentHandle -> IO a) -> IO a
withKent action = do
  bin <-
    findExecutable "kent-server" >>= \case
      Just b -> pure b
      Nothing ->
        throwIO . KentNotFound $
          "kent-server not found on PATH; run inside the nix dev shell"
  port <- getFreePort
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

-- | Ask the OS for an unused TCP port by binding to port 0 and reading back the
-- assignment, then closing the socket so kent can claim it.
--
-- There is a small window between closing the socket and kent binding, but the
-- ephemeral port range makes a collision between concurrent tests very
-- unlikely — and 'waitReady' surfaces the failure if it ever happens.
getFreePort :: IO Int
getFreePort =
  bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close \sock -> do
    Socket.bind sock (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
    Socket.getSocketName sock >>= \case
      Socket.SockAddrInet assigned _ -> pure (fromIntegral assigned)
      _ -> fail "freePort: expected an IPv4 socket address"

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

type EventList :: Type
newtype EventList = EventList [Aeson.Value]

instance Aeson.FromJSON EventList where
  parseJSON = Aeson.withObject "EventList" \o -> EventList <$> o .: "events"

-- | Fetch a single stored event by its kent event id.
--
-- Returns the full top-level object: @{event_id, project_id, payload}@, where
-- @payload@ carries the envelope item's @header@, @body@, and @envelope_header@.
getEvent :: KentHandle -> Text -> IO Aeson.Value
getEvent handle eid = do
  req <- HttpClient.parseRequest (eventUrl handle eid)
  resp <- HttpClient.httpLbs req handle.manager
  case Aeson.eitherDecode (HttpClient.responseBody resp) of
    Right value -> pure value
    Left err -> fail $ "failed to decode kent event response: " <> err

-- | The kent event id of every stored event, in the order kent returns them.
--
-- Note these are kent's own ids, /not/ the SDK's @event_id@ (which lives in the
-- delivered payload body); use them only to address 'getEvent'.
eventIds :: KentHandle -> IO [Text]
eventIds handle = do
  events <- listEvents handle
  pure [eid | event <- events, Just eid <- [dig ["event_id"] event >>= asText]]

eventUrl :: KentHandle -> Text -> String
eventUrl handle eid =
  "http://127.0.0.1:" <> show handle.port <> "/api/event/" <> Text.unpack eid

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

-- | Navigate nested JSON objects by a key path, returning 'Nothing' on any
-- missing key or non-object along the way.
dig :: [Text] -> Aeson.Value -> Maybe Aeson.Value
dig keys value = foldl step (Just value) keys
  where
    step acc key =
      acc >>= \case
        Aeson.Object o -> KeyMap.lookup (Key.fromText key) o
        _ -> Nothing

-- | Extract a JSON string, or 'Nothing' for any other value.
asText :: Aeson.Value -> Maybe Text
asText = \case
  Aeson.String t -> Just t
  _ -> Nothing
