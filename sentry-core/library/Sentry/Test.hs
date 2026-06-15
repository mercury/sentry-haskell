{-# LANGUAGE PatternSynonyms #-}

module Sentry.Test where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Atomics (atomicModifyIORefCAS, atomicModifyIORefCAS_)
import Data.Default (def)
import Data.Function ((&))
import Data.IORef (IORef, newIORef)
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Monoid (Endo (..))
import Patrol qualified
import Patrol.Type.Dsn qualified as Patrol.Dsn
import Patrol.Type.Envelope qualified as Patrol.Envelope
import Patrol.Type.Item qualified as Patrol.Item
import Patrol.Type.Items qualified as Patrol.Items
import Sentry.Client (Client)
import Sentry.Client.Options (ClientOptions (..))
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
-- of sending them.
--
-- We use 'Endo' for fast appends since this is mostly going to be written many
-- times and read in batches.
type TestTransport :: Type
newtype TestTransport = TestTransport
  { collected :: IORef (Endo [Patrol.Envelope])
  }

-- | Create a new, empty, 'TestTransport'.
new :: IO TestTransport
new = TestTransport <$> newIORef mempty

instance Transport TestTransport where
  send transport envelope =
    Transport.SendProcessed
      <$ atomicModifyIORefCAS_ transport.collected (<> (Endo (envelope :)))

-- | Build a 'Client' backed by the given 'TestTransport' with the 'TEST_DSN'.
mkClient :: TestTransport -> Client
mkClient transport =
  Witch.from @ClientOptions @Client
    (def @ClientOptions)
      { dsn = Just TEST_DSN,
        transport = Just (SomeTransport transport)
      }

-- | Create a fresh 'TestTransport', build a 'Client' from it, and run the
-- given 'SentryT' action. The callback receives the 'TestTransport' so it
-- can be inspected (e.g. via 'fetchAndClearEvents') after the action returns.
--
-- Returns both the action result and the transport.
withTestClient :: (MonadIO m) => (TestTransport -> SentryT m a) -> m (a, TestTransport)
withTestClient f = do
  transport <- liftIO new
  result <- runSentryT (mkClient transport) (f transport)
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
