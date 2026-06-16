{-# LANGUAGE ViewPatterns #-}

-- | Capture an 'Patrol.Event' and dispatch it through a 'Client'.
module Sentry.Capture
  ( captureEvent,
    captureEvent_,
    captureException,
    captureException_,
    captureMessage,
    captureMessage_,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (Exception (toException), SomeException, fromException)
import Control.Exception.Annotated (AnnotatedException (..), Annotation (..))
import Control.Monad (guard, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (getCurrentTime)
import Data.Typeable (cast)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Patrol qualified
import Patrol.Constant qualified as Patrol.Constant
import Patrol.Type.Envelope qualified as Patrol.Envelope
import Patrol.Type.Event qualified as Patrol.Event
import Patrol.Type.EventId qualified as Patrol.EventId
import Patrol.Type.Platform qualified as Patrol.Platform
import Sentry.Client (Client (..))
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Event (CapturedEvent (..))
import Sentry.Event qualified as Event
import Sentry.Integration (Integration (..), SomeIntegration)
import Sentry.Monad (HasClient, askClient)
import Sentry.Scope (ScopeData)
import Sentry.Scope qualified as Scope
import Sentry.Sdk qualified as Sdk
import Sentry.Transport (SendResponse (..))
import Sentry.Transport qualified as Transport
import System.Random (randomRIO)
import Witch qualified

-- | Process an event, applying any integrations & before-send hooks registered
-- with the 'Client' to the given event before handing it off to the transport.
--
-- Returns the event's 'Patrol.EventId' on success, otherwise 'Nothing' if the
-- event was dropped at any stage.
captureEvent :: (MonadIO m, MonadReader env m, HasClient env) => Patrol.Event -> m (Maybe Patrol.EventId)
captureEvent event = do
  client <- askClient
  captureWith client $ Witch.from event

-- | Convenience alias for a 'captureEvent' call that discards its result.
captureEvent_ :: (MonadIO m, MonadReader env m, HasClient env) => Patrol.Event -> m ()
captureEvent_ = void . captureEvent

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
-- The scope is then used to construct the event that gets handed off to the
-- internal pipeline for transport to the appropriate backend.
--
-- If the scope's @eventProcessor@ drops the event, this function returns
-- 'Nothing' without invoking the transport.
captureException :: (MonadIO m, MonadReader env m, HasClient env, Exception e) => e -> m (Maybe Patrol.EventId)
captureException (toException -> exc) = do
  client <- askClient
  let (anns, inner) = case fromException @(AnnotatedException SomeException) exc of
        Just (AnnotatedException as i) -> (as, i)
        Nothing -> ([], exc)
      scopeFromAnnotation = listToMaybe [s | Annotation a <- anns, Just s <- [cast @_ @ScopeData a]]
  scope <- maybe Scope.readAmbientScope pure scopeFromAnnotation
  let captured = Event.fromException inner `Event.withException` inner
  case scope `Scope.apply` captured of
    Nothing -> pure Nothing
    Just event -> captureWith client captured{event}

-- | Convenience alias for a 'captureException' call that discards its result.
captureException_ :: (MonadIO m, MonadReader env m, HasClient env, Exception e) => e -> m ()
captureException_ = void . captureException

-- | Capture a plain message at the given severity level, applying the ambient
-- scope before dispatch.
--
-- The message is placed in the event's @logentry@ field (both @message@ and
-- @formatted@) per the Sentry protocol. If the scope's @eventProcessor@ drops
-- the event, returns 'Nothing'.
captureMessage :: (MonadIO m, MonadReader env m, HasClient env) => Patrol.Level -> Text -> m (Maybe Patrol.EventId)
captureMessage lvl msg = do
  client <- askClient
  scope <- Scope.readAmbientScope
  let captured = Witch.from (Event.fromMessage lvl msg)
  case scope `Scope.apply` captured of
    Nothing -> pure Nothing
    Just event' -> captureWith client captured{event = event'}

-- | Convenience alias for a 'captureMessage' call that discards its result.
captureMessage_ :: (MonadIO m, MonadReader env m, HasClient env) => Patrol.Level -> Text -> m ()
captureMessage_ lvl = void . captureMessage lvl

-- | Internal event processing utility; performs the following steps:
--
--     1. Check for a valid transport and DSN (short-circuit for non-recording clients)
--     2. Apply sampling ('ClientOptions.sampleRate')
--     3. Run each integration's 'Sentry.Integration.processEvent'
--     4. Apply SDK defaults via 'applyClientDefaults'
--     5. Run 'ClientOptions.beforeSend'
--     6. Wrap the event in an envelope and send it via the transport
captureWith :: (MonadIO m) => Client -> CapturedEvent -> m (Maybe Patrol.EventId)
captureWith client captured = runMaybeT do
  dsn <- MaybeT . pure $ client.options.dsn
  transport <- MaybeT . pure $ client.transport
  guard =<< sample client.options.sampleRate
  event <- MaybeT . liftIO $ runIntegrations client.options captured client.integrations
  enriched <- liftIO $ applyClientDefaults client.options client.integrations event
  let processed = captured{event = enriched}
  finalEvent <- MaybeT . pure $ case client.options.beforeSend of
    Nothing -> Just processed.event
    Just callback -> callback processed
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

-- | Apply SDK-owned defaults to a 'Patrol.Event' at capture time.
--
-- Fills only fields that are still at their patrol sentinel value (empty
-- 'Text', @nil@ 'Patrol.EventId', 'Nothing', etc.), so explicit values set
-- by the event builder, scope, or integrations are always preserved.
--
-- Called by 'captureWith' after integrations have run and before
-- 'ClientOptions.beforeSend', so all three entry points ('captureEvent',
-- 'captureException', 'captureMessage') receive consistent defaults.
applyClientDefaults :: ClientOptions -> Vector SomeIntegration -> Patrol.Event -> IO Patrol.Event
applyClientDefaults opts integrations event = do
  eventId <-
    if event.eventId == Patrol.EventId.empty
      then Patrol.EventId.random
      else pure event.eventId
  timestamp <- case event.timestamp of
    Nothing -> Just <$> getCurrentTime
    Just t -> pure (Just t)
  pure
    event
      { Patrol.Event.eventId = eventId,
        Patrol.Event.timestamp = timestamp,
        Patrol.Event.release =
          if Text.null event.release
            then fromMaybe Text.empty opts.release
            else event.release,
        Patrol.Event.environment =
          if Text.null event.environment
            then fromMaybe Text.empty opts.environment
            else event.environment,
        Patrol.Event.serverName =
          if Text.null event.serverName
            then fromMaybe Text.empty opts.serverName
            else event.serverName,
        Patrol.Event.dist =
          if Text.null event.dist
            then fromMaybe Text.empty opts.dist
            else event.dist,
        Patrol.Event.platform = event.platform <|> Just Patrol.Platform.Haskell,
        Patrol.Event.sdk = event.sdk <|> Just (Sdk.sdkInfo integrations),
        Patrol.Event.version =
          if Text.null event.version
            then Patrol.Constant.sentryVersion
            else event.version
      }

-- | Run each integration's 'Sentry.Integration.processEvent' in sequence.
--
-- Short-circuits on the first 'Nothing'.
runIntegrations :: (MonadIO m) => ClientOptions -> CapturedEvent -> Vector SomeIntegration -> m (Maybe Patrol.Event)
runIntegrations options initial integrations =
  runMaybeT $
    Vector.foldM'
      step
      initial.event
      integrations
  where
    step event integration = MaybeT . liftIO $ processEvent integration initial{event} options
