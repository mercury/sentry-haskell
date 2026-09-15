{-# LANGUAGE ViewPatterns #-}

-- | Capture an 'Patrol.Event' and dispatch it through a 'Client'.
module Sentry.Capture
  ( -- * Capture overrides
    CaptureOverrides (..),

    -- * Capturing events
    captureEvent,
    captureEvent_,
    captureException,
    captureException_,
    captureExceptionWith,
    captureExceptionWith_,
    captureMessage,
    captureMessage_,
    captureUnhandledException,
    captureUnhandledException_,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (Exception (toException), SomeException, fromException)
import Control.Exception.Annotated (AnnotatedException (..), Annotation (..))
import Control.Monad (unless, void, when)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Default (Default (def))
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (getCurrentTime)
import Data.Typeable (cast)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Stack (CallStack, HasCallStack, callStack, withFrozenCallStack)
import Patrol qualified
import Patrol.Constant qualified as Patrol.Constant
import Patrol.Type.DataCategory qualified as Patrol.DataCategory
import Patrol.Type.Envelope qualified as Patrol.Envelope
import Patrol.Type.Event qualified as Patrol.Event
import Patrol.Type.EventId qualified as Patrol.EventId
import Patrol.Type.EventType qualified as Patrol.EventType
import Patrol.Type.Platform qualified as Patrol.Platform
import Sentry.Client (Client, pattern NON_RECORDING_CLIENT)
import Sentry.Client.Internal qualified as ClientInternal
import Sentry.Client.Options (ClientOptions (..))
import Sentry.ClientReport (DiscardReason)
import Sentry.ClientReport qualified as ClientReport
import Sentry.Event qualified
import Sentry.Event.Captured (CapturedEvent (..))
import Sentry.Event.Captured qualified as Captured
import Sentry.Integration (Integration (..), SomeIntegration)
import Sentry.Mechanism qualified
import Sentry.Scope.Operations (ScopeData)
import Sentry.Scope.Operations qualified as Scope
import Sentry.Sdk qualified as Sdk
import Sentry.Transport (SendResponse (..))
import Sentry.Transport qualified as Transport
import System.IO (hPutStrLn, stderr)
import System.Random (randomRIO)
import Witch qualified

-- | Capture a pre-built event with the ambient scope, then apply any
-- integrations and before-send hooks registered with the 'Client' before
-- handing it off to the transport.
--
-- Returns the final event ID when the pipeline completes and the transport
-- reports no immediate rejection; otherwise returns 'Nothing'. This does not
-- guarantee delivery: asynchronous failures can occur after capture returns.
captureEvent :: (MonadIO m) => Patrol.Event -> m (Maybe Patrol.EventId)
captureEvent event = do
  scope <- Scope.readMergedScope
  let client = fromMaybe NON_RECORDING_CLIENT (Scope.client scope)
      captured = Witch.from event
  prepared <- prepareCapture client scope captured
  maybe (pure Nothing) (captureWith client) prepared

-- | Convenience alias for a 'captureEvent' call that discards its result.
captureEvent_ :: (MonadIO m) => Patrol.Event -> m ()
captureEvent_ = void . captureEvent

-- | 'Patrol.Type.Event.Event' overrides for the fields that are associated
-- with capturing exceptions.
--
-- Its 'Default' instance apply no overrides at all.
type CaptureOverrides :: Type
data CaptureOverrides = CaptureOverrides
  { -- | The 'Patrol.Mechanism' to attach to the exception, if any.
    --
    -- See "Sentry.Mechanism" for smart constructors.
    mechanismOverride :: Maybe Patrol.Mechanism,
    -- | A 'Patrol.Level' that overrides both the event's own level and any
    -- ambient scope's 'Sentry.Scope.setLevel'.
    levelOverride :: Maybe Patrol.Level
  }
  deriving stock (Eq, Show)

instance Default CaptureOverrides where
  def = CaptureOverrides{mechanismOverride = Nothing, levelOverride = Nothing}

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
--    ('Sentry.Scope.readMergedScope') is read and applied.
--
-- The scope is then used to construct the event that gets handed off to the
-- internal pipeline for transport to the appropriate backend.
--
-- If the scope's @eventProcessor@ drops the event, this function returns
-- 'Nothing' without invoking the transport.
captureException :: (HasCallStack, MonadIO m, Exception e) => e -> m (Maybe Patrol.EventId)
captureException e =
  withFrozenCallStack $
    captureExceptionImpl callStack def{mechanismOverride = Just Sentry.Mechanism.generic} e

-- | Convenience alias for a 'captureException' call that discards its result.
captureException_ :: (HasCallStack, MonadIO m, Exception e) => e -> m ()
captureException_ = withFrozenCallStack $ void . captureException

-- | Like 'captureException', but with full control over the
-- 'CaptureOverrides' applied to the resulting event.
--
-- Most application code should prefer 'captureException'.
--
-- This is the general escape hatch for framework\/integration authors who need a
-- specific mechanism or a level override.
captureExceptionWith :: (HasCallStack, MonadIO m, Exception e) => CaptureOverrides -> e -> m (Maybe Patrol.EventId)
captureExceptionWith overrides e =
  withFrozenCallStack $ captureExceptionImpl callStack overrides e

-- | Convenience alias for a 'captureExceptionWith' call that discards its
-- result.
captureExceptionWith_ :: (HasCallStack, MonadIO m, Exception e) => CaptureOverrides -> e -> m ()
captureExceptionWith_ overrides = withFrozenCallStack $ void . captureExceptionWith overrides

-- | Capture an exception that is considered to be "unhandled"; that is to say
-- it has escaped to a boundary-of-last-resort rather than being caught by
-- application code.
captureUnhandledException :: (HasCallStack, MonadIO m, Exception e) => Text -> e -> m (Maybe Patrol.EventId)
captureUnhandledException ty e =
  withFrozenCallStack $
    captureExceptionImpl callStack def{mechanismOverride = Just (Sentry.Mechanism.unhandled ty)} e

-- | Convenience alias for a 'captureUnhandledException' call that discards
-- its result.
captureUnhandledException_ :: (HasCallStack, MonadIO m, Exception e) => Text -> e -> m ()
captureUnhandledException_ ty = withFrozenCallStack $ void . captureUnhandledException ty

-- | Shared implementation behind 'captureException', 'captureExceptionWith',
-- and 'captureUnhandledException'.
captureExceptionImpl ::
  (MonadIO m, Exception e) => CallStack -> CaptureOverrides -> e -> m (Maybe Patrol.EventId)
captureExceptionImpl cs overrides (toException -> orig) = do
  -- Read the call-site ambient scope once: it supplies the client, and (absent
  -- an annotation) the scope to apply.
  --
  -- The annotation carries throw-site scope data, but the client is always
  -- resolved from the call site.
  ambient <- Scope.readMergedScope
  let client = fromMaybe NON_RECORDING_CLIENT (Scope.client ambient)
      (anns, inner) = case fromException @(AnnotatedException SomeException) orig of
        Just (AnnotatedException as i) -> (as, i)
        Nothing -> ([], orig)
      scopeFromAnnotation = listToMaybe [s | Annotation a <- anns, Just s <- [cast @_ @ScopeData a]]
      scope = fromMaybe ambient scopeFromAnnotation
      captured =
        (Sentry.Event.fromExceptionWith overrides.mechanismOverride inner `Captured.withException` inner $ orig)
          { captureCallStack = Just cs
          }
  prepared <- prepareCapture client scope captured
  maybe (pure Nothing) (captureWith client . applyOverride) prepared
  where
    applyOverride captured' = captured'{event = applyLevelOverride overrides captured'.event}

-- | Apply 'CaptureOverrides.levelOverride' to the /result/ of 'Sentry.Scope.applyToEvent'.
applyLevelOverride :: CaptureOverrides -> Patrol.Event -> Patrol.Event
applyLevelOverride overrides event =
  maybe event (\lvl -> event{Patrol.Event.level = Just lvl}) overrides.levelOverride

-- | Capture a plain message at the given severity level, applying the ambient
-- scope before dispatch.
--
-- The message is placed in the event's @logentry@ field (both @message@ and
-- @formatted@) per the Sentry protocol. If the scope's @eventProcessor@ drops
-- the event, returns 'Nothing'.
--
-- The 'HasCallStack' constraint captures the call-site stack, which stacktrace
-- integrations can attach as thread frames (since message events carry no
-- exception).
captureMessage :: (HasCallStack, MonadIO m) => Patrol.Level -> Text -> m (Maybe Patrol.EventId)
captureMessage lvl msg = do
  -- One ambient read supplies both the client and the scope to apply.
  scope <- Scope.readMergedScope
  let client = fromMaybe NON_RECORDING_CLIENT (Scope.client scope)
      captured =
        (Witch.from (Sentry.Event.fromMessage lvl msg))
          { captureCallStack = Just callStack
          }
  prepared <- prepareCapture client scope captured
  maybe (pure Nothing) (captureWith client) prepared

-- | Convenience alias for a 'captureMessage' call that discards its result.
captureMessage_ :: (HasCallStack, MonadIO m) => Patrol.Level -> Text -> m ()
captureMessage_ lvl = withFrozenCallStack $ void . captureMessage lvl

-- | Apply the selected scope to a captured event before dispatching it through
-- the common capture pipeline. A scope processor drop is recorded here so
-- every public capture verb reports that drop exactly once.
prepareCapture :: (MonadIO m) => Client -> ScopeData -> CapturedEvent -> m (Maybe CapturedEvent)
prepareCapture client scope captured =
  case scope `Scope.applyToEvent` captured of
    Just event -> pure $ Just captured{event}
    Nothing ->
      Nothing <$ noteDrop client ClientReport.EventProcessor (eventCategory captured.event)

-- | Internal event processing utility; performs the following steps:
--
--     1. Check for a valid DSN and transport (short-circuit for non-recording clients)
--     2. Run each integration's 'Sentry.Integration.processEvent'
--     3. Apply SDK defaults via 'applyClientDefaults'
--     4. Run 'ClientOptions.beforeSend'
--     5. Apply sampling ('ClientOptions.sampleRate')
--     6. Wrap the event in an envelope and send it via the transport
captureWith :: (MonadIO m) => Client -> CapturedEvent -> m (Maybe Patrol.EventId)
captureWith client captured = do
  let runtime = ClientInternal.runtimeOptions client
  case (runtime.dsn, client.transport) of
    -- Non-recording client: SDK is disabled; nothing to report (no transport
    -- to hold the client-report accumulator either).
    (Nothing, _) -> pure Nothing
    (_, Nothing) -> pure Nothing
    (Just dsn, Just transport) -> do
      result <- runExceptT do
        maybeEvent <- liftIO $ runIntegrations client.options captured client.integrations
        event <- maybe (throwError ClientReport.EventProcessor) pure maybeEvent
        eventWithDefaults <- liftIO $ applyClientDefaults client.options runtime client.integrations event
        let processed = captured{event = eventWithDefaults}
        finalEvent <- case client.options.beforeSend of
          Nothing -> pure processed.event
          Just callback ->
            maybe
              (throwError ClientReport.BeforeSend)
              pure
              (callback processed)
        -- sample /after/ processing events, so that an event processor has the
        -- opportunity to drop an event even if it wasn't going to be sampled;
        -- this ensures more accurate discard counts in Sentry's dashboards.
        sampled <- liftIO $ sample runtime.sampleRate
        unless sampled $ throwError ClientReport.SampleRate
        let envelope = Patrol.Envelope.fromEvent dsn finalEvent
        response <- liftIO $ Transport.send transport envelope
        pure (finalEvent, response)
      case result of
        Left reason ->
          Nothing <$ noteDrop client reason (eventCategory captured.event)
        -- Transport failures are accounted for by the transport itself.
        Right (finalEvent, response) ->
          pure $ case response of
            SendProcessed -> Just finalEvent.eventId
            SendFailed_QueueFull -> Nothing
            SendFailed_Shutdown -> Nothing
            SendFailed_Other -> Nothing
  where
    sample :: (MonadIO m) => Float -> m Bool
    sample rate
      | rate >= 1.0 = pure True
      | rate <= 0.0 = pure False
      | otherwise = do
          roll <- randomRIO (0.0, 1.0)
          pure $ roll < rate

-- | Determine the 'Patrol.DataCategory.DataCategory' a dropped event should be
-- attributed to in client reports, derived from the event's @type_@.
eventCategory :: Patrol.Event -> Patrol.DataCategory.DataCategory
eventCategory event = case event.type_ of
  Just Patrol.EventType.Transaction -> Patrol.DataCategory.Transaction
  _ -> Patrol.DataCategory.Error

-- | Record a drop via the transport and emit a debug log line when
-- 'ClientOptions.debug' is 'True'.
noteDrop :: (MonadIO m) => Client -> DiscardReason -> Patrol.DataCategory.DataCategory -> m ()
noteDrop client reason category = liftIO do
  for_ client.transport \t ->
    Transport.recordDiscards t reason category 1
  when (ClientInternal.runtimeOptions client).debug $
    hPutStrLn stderr $
      "[sentry] event dropped: " <> Text.unpack (ClientReport.reasonText reason)

-- | Apply SDK-owned defaults to a 'Patrol.Event' at capture time.
--
-- Fills only fields that are still at their patrol sentinel value (empty
-- 'Text', @nil@ 'Patrol.EventId', 'Nothing', etc.), so explicit values set
-- by the event builder, scope, or integrations are always preserved.
applyClientDefaults :: ClientOptions -> ClientInternal.RuntimeOptions -> Vector SomeIntegration -> Patrol.Event -> IO Patrol.Event
applyClientDefaults opts runtime integrations event = do
  eventId <-
    if event.eventId == Patrol.EventId.empty
      then Patrol.EventId.random
      else pure event.eventId
  timestamp <- Just <$> maybe getCurrentTime pure event.timestamp
  pure
    event
      { Patrol.Event.eventId = eventId,
        Patrol.Event.timestamp = timestamp,
        Patrol.Event.release = event.release `orOpt` opts.release,
        Patrol.Event.environment = event.environment `orElse` runtime.environment,
        Patrol.Event.serverName = event.serverName `orOpt` opts.serverName,
        Patrol.Event.dist = event.dist `orOpt` opts.dist,
        Patrol.Event.platform = event.platform <|> Just Patrol.Platform.Haskell,
        Patrol.Event.sdk = event.sdk <|> Just (Sdk.sdkInfo integrations),
        Patrol.Event.version = event.version `orElse` Patrol.Constant.sentryVersion
      }
  where
    -- \| Use @t@ if non-empty; otherwise use the option value (defaulting to @""@).
    orOpt :: Text -> Maybe Text -> Text
    t `orOpt` opt = t `orElse` fromMaybe Text.empty opt

    -- \| Use @t@ if non-empty; otherwise use @fallback@.
    orElse :: Text -> Text -> Text
    t `orElse` fallback = if Text.null t then fallback else t

-- | Run each integration's 'Sentry.Integration.processEvent' in sequence,
-- passing each resulting event directly to the next.
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
