{-# LANGUAGE RequiredTypeArguments #-}

module Sentry.Client
  ( -- * Client
    Client (..),
    pattern NON_RECORDING_CLIENT,
    getIntegration,

    -- * Operations
    captureEvent,
    captureEvent_,
  )
where

import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (cast)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Patrol qualified
import Patrol.Type.Envelope qualified as Patrol.Envelope
import Patrol.Type.Event qualified as Patrol.Event
import Sentry.Client.Options (ClientOptions (..), pattern DEFAULT_CLIENT_OPTIONS)
import Sentry.Integration (Integration (..), SomeIntegration (..))
import Sentry.Transport (SendResponse (..), SomeTransport)
import Sentry.Transport qualified as Transport
import System.Random (randomRIO)
import Type.Reflection (someTypeRep)
import Witch qualified

-- | The 'Client' is responsible for processing events and sending them to the
-- Sentry server via the 'Sentry.Transport.SomeTransport' it contains; it can
-- be created from 'Sentry.Client.Options.ClientOptions', which it retains a
-- copy of after construction.
type Client :: Type
data Client = Client
  { options :: ClientOptions,
    transport :: Maybe SomeTransport,
    integrations :: Vector SomeIntegration
  }

instance Witch.From ClientOptions Client where
  from options@ClientOptions{transport, integrations} =
    Client
      { options,
        transport,
        integrations
      }

-- | Attempt to find a 'Sentry.Integration.Integration' in the list of
-- 'Sentry.Integration.SomeIntegration' installed in the 'Client'.
getIntegration :: forall i -> (Integration i) => Client -> Maybe i
getIntegration iType client = do
  let iid = someTypeRep (Proxy @iType)
  (SomeIntegration _ i) <- Vector.find (\(SomeIntegration rep _) -> rep == iid) client.integrations
  cast i

-- | Process an event through the client pipeline and send it to Sentry.
--
-- The pipeline is:
--
--     1. Check for a valid transport and DSN (short-circuit for non-recording clients)
--     2. Apply sampling ('ClientOptions.sampleRate')
--     3. Run each integration's 'Sentry.Integration.processEvent'
--     4. Run 'ClientOptions.beforeSend'
--     5. Wrap the event in an envelope and send it via the transport
--
-- Returns the event's 'Patrol.EventId' on success, otherwise 'Nothing' if the event
-- was dropped at any stage.
captureEvent :: (MonadIO m) => Client -> Patrol.Event -> m (Maybe Patrol.EventId)
captureEvent client event = runMaybeT do
  dsn <- MaybeT . pure $ client.options.dsn
  transport <- MaybeT . pure $ client.transport
  guard =<< sample client.options.sampleRate
  processedEvent <- MaybeT . liftIO $ runIntegrations client.options event client.integrations
  finalEvent <- MaybeT . pure $ (fromMaybe Just client.options.beforeSend) processedEvent
  let envelope = Patrol.Envelope.fromEvent dsn finalEvent
  response <- liftIO $ Transport.send transport envelope
  MaybeT . pure $ case response of
    SendFailed_Shutdown -> Nothing
    SendFailed_QueueFull -> Nothing
    SendProcessed -> Just finalEvent.eventId
  where
    sample :: (MonadIO m) => Float -> m Bool
    sample rate
      | rate >= 1.0 = pure True
      | rate <= 0.0 = pure False
      | otherwise = do
          roll <- randomRIO (0.0, 1.0)
          pure $ roll < rate

-- | Convenience alias for a 'captureEvent' call that discards its result
captureEvent_ :: (MonadIO m) => Client -> Patrol.Event -> m ()
captureEvent_ client event = void $ captureEvent client event

-- | Run each integration's 'Sentry.Integration.processEvent' in sequence.
--
-- Short-circuits on the first 'Nothing'.
runIntegrations :: (MonadIO m) => ClientOptions -> Patrol.Event -> Vector SomeIntegration -> m (Maybe Patrol.Event)
runIntegrations options initialEvent integrations =
  runMaybeT $
    Vector.foldM'
      step
      initialEvent
      integrations
  where
    step event integration = MaybeT . liftIO $ processEvent integration event options

-- | Any client which does not have a valid 'Transport' is non-recording.
pattern NON_RECORDING_CLIENT :: Client
pattern NON_RECORDING_CLIENT <- Client{transport = Nothing}
  where
    NON_RECORDING_CLIENT =
      Client
        { options = DEFAULT_CLIENT_OPTIONS,
          transport = Nothing,
          integrations = Vector.empty
        }
