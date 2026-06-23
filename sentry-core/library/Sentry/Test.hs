{-# LANGUAGE PatternSynonyms #-}

module Sentry.Test
  ( -- * Test DSN
    pattern TEST_DSN,

    -- * Test transport
    TestTransport (..),
    new,
    mkClient,
    mkCustomClient,
    withClient,
    withCustomClient,

    -- * Inspecting collected data
    fetchAndClearEvents,
    fetchAndClearEnvelopes,
    fetchAndClearDrops,

    -- * Scope isolation
    cleanScopes,
    withGlobalScope,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.Atomics (atomicModifyIORefCAS, atomicModifyIORefCAS_)
import Data.Default (def)
import Data.Function ((&))
import Data.IORef (IORef, newIORef)
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Monoid (Endo (..))
import OpenTelemetry.Context.ThreadLocal qualified as ThreadLocal
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
import Sentry.Scope qualified as Scope
import Sentry.Scope.Internal qualified as Internal
import Sentry.Scope.IO qualified as ScopeIO
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
-- given action with that client bound to the isolation scope.
--
-- The callback receives the 'TestTransport' so it can be inspected (e.g. via
-- 'fetchAndClearEvents') after the action returns.
--
-- Returns both the action result and the transport.
withClient :: (MonadUnliftIO m) => (TestTransport -> m a) -> m (a, TestTransport)
withClient = withCustomClient (def @ClientOptions)

-- | Like 'withClient' but uses the given 'ClientOptions' (with 'TEST_DSN' and
-- the transport always filled in).
--
-- Unlike 'mkCustomClient', this runs the full initialization lifecycle via
-- 'Sentry.Client.new', so 'Sentry.Integration.Integration.setup' is
-- invoked for every integration and default integrations are prepended when
-- 'Sentry.Client.Options.ClientOptions.defaultIntegrations' is @True@.
withCustomClient :: (MonadUnliftIO m) => ClientOptions -> (TestTransport -> m a) -> m (a, TestTransport)
withCustomClient opts f = do
  transport <- liftIO new
  client <-
    liftIO $
      Client.new
        opts
          { dsn = opts.dsn <|> Just TEST_DSN,
            transport = Just (Witch.from (SomeTransport transport))
          }
  result <- ScopeIO.withClient client (f transport)
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

-- Scope isolation helpers

-- | Reset all three scope layers to fresh empty scopes on the calling thread.
--
-- Each layer is replaced with a new empty 'Scope' rather than removed from the
-- thread-local context: removing the global override would fall back to the
-- true process singleton (exposing shared state across parallel tests), so it
-- must be shadowed with an empty scope instead.
--
-- For parallel tests that call 'Sentry.Init.init' \/ 'Sentry.Init.withSentry',
-- use 'withGlobalScope' instead, as it provides isolation without requiring an
-- explicit clean-up step.
cleanScopes :: IO ()
cleanScopes = do
  g <- Scope.create Scope.Global
  i <- Scope.create Scope.Isolation
  c <- Scope.create Scope.Current
  ThreadLocal.adjustContext
    ( Internal.insertGlobal g
        . Scope.insertIsolation i
        . Scope.insertCurrent c
    )

-- | Run an action with a fresh, empty global scope installed on the calling
-- thread, restoring the previous global on exit (normal or exceptional).
--
-- Every read and write to the global scope within the action (including calls
-- to 'Sentry.Init.init', 'Sentry.Init.close', 'Sentry.Init.withSentry', and
-- 'Sentry.Scope.configureGlobal') hits the thread-local copy rather than the
-- process singleton, so parallel tests cannot contaminate one another.
--
-- __Note__: this helper is intentionally /not/ re-exported from "Sentry" or
-- "Sentry.Scope". The global scope is meant to be truly global in production;
-- this override exists solely as a test-isolation escape hatch.
withGlobalScope :: (MonadUnliftIO m) => m a -> m a
withGlobalScope action = withRunInIO \run ->
  bracket acquire release \_ -> run action
  where
    acquire :: IO (Maybe Scope.Scope)
    acquire = do
      ctx <- ThreadLocal.getContext
      scope <- Scope.create Scope.Global
      ThreadLocal.adjustContext (Internal.insertGlobal scope)
      pure (Internal.lookupGlobal ctx)

    release parent =
      ThreadLocal.adjustContext \ctx ->
        maybe (Internal.removeGlobal ctx) (`Internal.insertGlobal` ctx) parent
