-- | Capture an 'Patrol.Event' and dispatch it through a 'Client'.
module Sentry.Capture
  ( captureEvent,
    captureEvent_,
  )
where

import Control.Monad (guard, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Patrol qualified
import Patrol.Type.Envelope qualified as Patrol.Envelope
import Patrol.Type.Event qualified as Patrol.Event
import Sentry.Client (Client (..))
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Integration (Integration (..), SomeIntegration)
import Sentry.Transport (SendResponse (..))
import Sentry.Transport qualified as Transport
import System.Random (randomRIO)

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

-- | Convenience alias for a 'captureEvent' call that discards its result.
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
