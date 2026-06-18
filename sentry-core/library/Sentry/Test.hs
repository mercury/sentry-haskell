{-# LANGUAGE PatternSynonyms #-}

module Sentry.Test where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Atomics (atomicModifyIORefCAS, atomicModifyIORefCAS_)
import Data.Default (def)
import Data.Function ((&))
import Data.IORef (IORef, newIORef)
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Monoid (Endo (..))
import Patrol qualified
import Patrol.Type.DataCategory (DataCategory)
import Patrol.Type.Dsn qualified as Patrol.Dsn
import Patrol.Type.Envelope qualified as Patrol.Envelope
import Patrol.Type.Item qualified as Patrol.Item
import Patrol.Type.Items qualified as Patrol.Items
import Sentry.Client (Client)
import Sentry.Client qualified as Client
import Sentry.Client.Options (ClientOptions (..))
import Sentry.ClientReport (DiscardReason)
import Sentry.Monad (SentryT, runSentryT)
import Sentry.Transport (SomeTransport (..), Transport (..))
import Sentry.Transport qualified as Transport
import Witch qualified

pattern TEST_DSN :: Patrol.Dsn
pattern TEST_DSN =
  Patrol.Dsn.Dsn
    { Patrol.Dsn.protocol = "https",
      Patrol.Dsn.publicKey = "public",
      Patrol.Dsn.secretKey = "",
      Patrol.Dsn.host = "sentry.invalid",
      Patrol.Dsn.port = Nothing,
      Patrol.Dsn.path = "1",
      Patrol.Dsn.projectId = ""
    }

-- | A type whose 'Sentry.Transport.Transport' instance collects events instead
-- of sending them, and records any 'Transport.recordDiscards' calls so that
-- tests can assert on drop-site instrumentation.
--
-- We use 'Endo' for fast appends since this is mostly going to be written many
-- times and read in batches.
type TestTransport :: Type
data TestTransport = TestTransport
  { collected :: IORef (Endo [Patrol.Envelope]),
    recordedDrops :: IORef (Endo [(DiscardReason, DataCategory, Int)])
  }

-- | Create a new, empty, 'TestTransport'.
new :: IO TestTransport
new = TestTransport <$> newIORef mempty <*> newIORef mempty

instance Transport TestTransport where
  send transport envelope =
    Transport.SendProcessed
      <$ atomicModifyIORefCAS_ transport.collected (<> (Endo (envelope :)))

  recordDiscards transport reason category n =
    atomicModifyIORefCAS_ transport.recordedDrops (<> Endo ((reason, category, n) :))

-- | Build a 'Client' backed by the given 'TestTransport' with the 'TEST_DSN'.
mkClient :: TestTransport -> Client
mkClient = flip mkCustomClient (def @ClientOptions)

-- | Like 'mkClient' but accepts custom 'ClientOptions'. The transport is
-- always set to the given 'TestTransport'; 'TEST_DSN' is used as a fallback
-- when the options do not already include a 'dsn'.
mkCustomClient :: TestTransport -> ClientOptions -> Client
mkCustomClient transport opts =
  Witch.from @ClientOptions @Client
    opts
      { dsn = opts.dsn <|> Just TEST_DSN,
        transport = Just (Witch.from (SomeTransport transport))
      }

-- | Create a fresh 'TestTransport', build a 'Client' from it, and run the
-- given 'SentryT' action. The callback receives the 'TestTransport' so it
-- can be inspected (e.g. via 'fetchAndClearEvents') after the action returns.
--
-- Returns both the action result and the transport.
withClient :: (MonadIO m) => (TestTransport -> SentryT m a) -> m (a, TestTransport)
withClient = withCustomClient (def @ClientOptions)

-- | Like 'withClient' but uses the given 'ClientOptions' (with 'TEST_DSN' and
-- the transport always filled in).
--
-- Unlike 'mkCustomClient', this runs the full initialization lifecycle via
-- 'Sentry.Client.new', so 'Sentry.Integration.Integration.setup' is
-- invoked for every integration and default integrations are prepended when
-- 'Sentry.Client.Options.ClientOptions.defaultIntegrations' is @True@.
withCustomClient :: (MonadIO m) => ClientOptions -> (TestTransport -> SentryT m a) -> m (a, TestTransport)
withCustomClient opts f = do
  transport <- liftIO new
  client <-
    liftIO $
      Client.new
        opts
          { dsn = opts.dsn <|> Just TEST_DSN,
            transport = Just (Witch.from (SomeTransport transport))
          }
  result <- runSentryT client (f transport)
  pure (result, transport)

-- | Like 'fetchAndClearEnvelopes', but filters the 'Patrol.Type.Envelope.Envelope's
-- and only returns the 'Patrol.Type.Event.Event's contained therein (if any).
fetchAndClearEvents :: TestTransport -> IO [Patrol.Event]
fetchAndClearEvents transport = do
  envelopes <- fetchAndClearEnvelopes transport
  pure $ flip concatMap envelopes \envelope -> case envelope.items of
    Patrol.Items.EnvelopeItems items ->
      items & mapMaybe \case
        Patrol.Item.Event event -> Just event
        _ -> Nothing
    _ -> []

-- | Drains the given 'TestTransport' of any Patrol.Type.Envelope.Envelope's
-- that have been enqueued.
fetchAndClearEnvelopes :: TestTransport -> IO [Patrol.Envelope]
fetchAndClearEnvelopes transport =
  atomicModifyIORefCAS
    transport.collected
    \envelopes -> (mempty, envelopes `appEndo` [])

-- | Drains the given 'TestTransport' of any drop records accumulated via
-- 'Transport.recordDiscards', returning them oldest-first.
fetchAndClearDrops :: TestTransport -> IO [(DiscardReason, DataCategory, Int)]
fetchAndClearDrops transport =
  atomicModifyIORefCAS
    transport.recordedDrops
    \drops -> (mempty, drops `appEndo` [])
