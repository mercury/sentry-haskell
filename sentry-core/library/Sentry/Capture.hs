{-# LANGUAGE ViewPatterns #-}

-- | Capture an 'Patrol.Event' and dispatch it through a 'Client'.
module Sentry.Capture
  ( captureEvent,
    captureEvent_,
    captureException,
    captureException_,
  )
where

import Control.Exception (Exception (toException), SomeException, fromException)
import Control.Exception.Annotated (AnnotatedException (..), Annotation (..))
import Control.Monad (guard, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Typeable (cast)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Patrol qualified
import Patrol.Type.Envelope qualified as Patrol.Envelope
import Patrol.Type.Event qualified as Patrol.Event
import Sentry.Client (Client (..))
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Integration (Integration (..), SomeIntegration)
import Sentry.Scope (ScopeData)
import Sentry.Scope qualified as Scope
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

-- | Capture an arbitrary 'Exception', building a 'Patrol.Event' from it and
-- dispatching it through the 'Client'.
--
-- Scope selection follows the \"innermost wins\" principle that the rest of
-- the SDK uses:
--
-- 1. If the exception is wrapped in an 'AnnotatedException' that carries a
--    'ScopeData' annotation (attached by 'Sentry.Scope.IO.withScope' when the
--    exception escaped a scope boundary), that 'ScopeData' is /authoritative/.
-- 2. Otherwise, the ambient thread-local scope at the call site
--    ('Sentry.Scope.readAmbientScope') is read and applied.
--
-- The scope is then used to construct the event that gets handed off to
-- 'captureEvent' for transport to the appropriate backend.
--
-- If the scope's @eventProcessor@ drops the event, this function returns
-- 'Nothing' without invoking the transport.
captureException :: (MonadIO m, Exception e) => Client -> e -> m (Maybe Patrol.EventId)
captureException client (toException -> exc) = do
  let (anns, inner) = case fromException @(AnnotatedException SomeException) exc of
        Just (AnnotatedException as i) -> (as, i)
        Nothing -> ([], exc)
      scopeFromAnnotation = listToMaybe [s | Annotation a <- anns, Just s <- [cast @_ @ScopeData a]]
  scope <- maybe Scope.readAmbientScope pure scopeFromAnnotation
  event <- liftIO $ Patrol.Event.fromSomeException inner
  case scope `Scope.apply` event of
    Nothing -> pure Nothing
    Just finalEvent -> captureEvent client finalEvent

-- | Convenience alias for a 'captureException' call that discards its result.
captureException_ :: (MonadIO m, Exception e) => Client -> e -> m ()
captureException_ client e = void $ captureException client e

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
