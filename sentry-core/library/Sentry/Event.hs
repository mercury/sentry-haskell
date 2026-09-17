-- | An event reported to Sentry, and builders for its fields.
--
-- @
-- scrub ce = Just $ Sentry.Event.apply
--   ce.event
--   [Sentry.Event.unsetUser, Sentry.Event.removeExtra "authorization"]
-- @
--
-- The record and its fields are exported here, so hooks can also use record
-- updates without depending on @patrol@ directly.
--
-- See "Sentry.Update" for the composition rules every builder module shares.
module Sentry.Event
  ( -- * Record
    Event (..),
    empty,

    -- * Updates
    EventUpdate,
    with,
    apply,

    -- * Text fields
    setDist,
    setEnvironment,
    setLogger,
    setRelease,
    setServerName,
    setTransaction,
    setVersion,

    -- * Identity and severity
    setEventId,
    setLevel,
    setOptionalLevel,
    unsetLevel,
    setType,
    setOptionalType,
    unsetType,
    setPlatform,
    setOptionalPlatform,
    unsetPlatform,
    setTimestamp,
    setOptionalTimestamp,
    unsetTimestamp,
    setTimeSpent,
    setOptionalTimeSpent,
    unsetTimeSpent,

    -- * Tags
    setTags,
    setTag,
    setOptionalTag,
    setTagIfAbsent,
    removeTag,
    clearTags,
    lookupTag,

    -- * Extra data
    setExtras,
    setExtra,
    setOptionalExtra,
    removeExtra,
    clearExtras,
    lookupExtra,

    -- * Contexts

    -- Generic contexts
    setContexts,
    setContext,
    setOptionalContext,
    setContextIfAbsent,
    removeContext,
    clearContexts,
    lookupContext,
    -- Custom context values
    setContextValues,
    setOptionalContextValues,
    setContextValue,
    setOptionalContextValue,
    removeContextValue,
    modifyContextValues,
    lookupContextValue,
    modifyExistingContextValue,
    alterContextValue,
    -- OS context
    setOsContext,
    setOptionalOsContext,
    modifyOsContext,
    modifyExistingOsContext,
    lookupOsContext,
    alterOsContext,
    -- App context
    setAppContext,
    setOptionalAppContext,
    modifyAppContext,
    modifyExistingAppContext,
    lookupAppContext,
    alterAppContext,
    -- Runtime context
    setRuntimeContext,
    setOptionalRuntimeContext,
    modifyRuntimeContext,
    modifyExistingRuntimeContext,
    lookupRuntimeContext,
    alterRuntimeContext,
    -- Browser context
    setBrowserContext,
    setOptionalBrowserContext,
    modifyBrowserContext,
    modifyExistingBrowserContext,
    lookupBrowserContext,
    alterBrowserContext,
    -- Device context
    setDeviceContext,
    setOptionalDeviceContext,
    modifyDeviceContext,
    modifyExistingDeviceContext,
    lookupDeviceContext,
    alterDeviceContext,
    -- Trace context
    setTraceContext,
    setOptionalTraceContext,
    modifyTraceContext,
    modifyExistingTraceContext,
    lookupTraceContext,
    alterTraceContext,

    -- * Modules
    setModules,
    setModule,
    setOptionalModule,
    removeModule,
    clearModules,
    lookupModule,

    -- * Fingerprint
    defaultFingerprintComponent,
    prependFingerprintComponent,
    ensureDefaultFingerprint,
    removeDefaultFingerprint,
    modifyFingerprint,
    setFingerprint,
    appendFingerprintComponent,
    clearFingerprint,
    findFingerprintComponent,
    filterFingerprint,

    -- * Processing errors
    setErrors,
    appendError,
    clearErrors,
    findError,
    filterErrors,

    -- * User
    setUser,
    setOptionalUser,
    modifyUser,
    modifyExistingUser,
    unsetUser,

    -- * Integration-populated payloads
    setBreadcrumbs,
    setOptionalBreadcrumbs,
    modifyBreadcrumbs,
    modifyExistingBreadcrumbs,
    unsetBreadcrumbs,
    setDebugMeta,
    setOptionalDebugMeta,
    unsetDebugMeta,
    setExceptionChain,
    setOptionalExceptionChain,
    modifyExceptionChain,
    modifyExistingExceptionChain,
    unsetExceptionChain,
    setLogentry,
    setOptionalLogentry,
    unsetLogentry,
    setRequest,
    setOptionalRequest,
    modifyRequest,
    modifyExistingRequest,
    unsetRequest,
    setSdk,
    setOptionalSdk,
    unsetSdk,
    setThreads,
    setOptionalThreads,
    unsetThreads,
    setTransactionInfo,
    setOptionalTransactionInfo,
    unsetTransactionInfo,

    -- * Whole-event builders
    fromException,
    fromExceptionWith,
    fromMessage,
    attachMechanism,
  )
where

import Control.Exception (SomeException)
import Data.Aeson qualified as Aeson
import Data.Foldable qualified as Foldable
import Data.Kind (Type)
import Data.List (unsnoc)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Patrol qualified
import Patrol.Type.Context qualified as Patrol.Context
import Patrol.Type.Event (Event (..), empty)
import Patrol.Type.Event qualified as Patrol.Event
import Patrol.Type.EventType qualified as Patrol.EventType
import Patrol.Type.Exception qualified as Patrol.Exception
import Patrol.Type.Exceptions qualified as Patrol.Exceptions
import Patrol.Type.Level qualified as Patrol.Level
import Patrol.Type.LogEntry qualified as Patrol.LogEntry
import Patrol.Type.Mechanism qualified as Patrol.Mechanism
import Sentry.AppContext qualified
import Sentry.Breadcrumb qualified
import Sentry.BrowserContext qualified
import Sentry.Context.Internal qualified
import Sentry.DeviceContext qualified
import Sentry.Exception qualified
import Sentry.Fingerprint.Internal (defaultFingerprintComponent)
import Sentry.Fingerprint.Internal qualified as Fingerprint
import Sentry.OsContext qualified
import Sentry.Request qualified
import Sentry.RuntimeContext qualified
import Sentry.TraceContext qualified
import Sentry.Update (Update (..))
import Sentry.Update qualified
import Sentry.User qualified
import Witch qualified

-- | A pending change to an 'Event'. See "Sentry.Update".
type EventUpdate :: Type
type EventUpdate = Update Event

-- | Build an update using the event's current value, including preceding
-- updates in the composition. Apply the resulting update to that same record.
--
-- @
-- Sentry.Event.with \\e -> Sentry.Event.setTag \"had_user\" (maybe \"no\" (const \"yes\") e.user)
-- @
--
-- For an optional field, use its optional setter with @fmap@ to skip absence:
-- @Sentry.Event.with \\event -> Sentry.Event.setOptionalLevel (fmap transformLevel event.level)@.
-- Here @transformLevel@ has type @Level -> Level@. A @Maybe Level -> Maybe Level@
-- function can additionally insert or remove the assignment.
with :: (Witch.From a EventUpdate) => (Event -> a) -> EventUpdate
with = Sentry.Update.with

-- * Text fields

-- | Assign the build distribution identifier.
setDist :: Text -> EventUpdate
setDist !assigned = Update \e -> e{Patrol.Event.dist = assigned}

-- | Assign the environment name.
setEnvironment :: Text -> EventUpdate
setEnvironment !assigned = Update \e -> e{Patrol.Event.environment = assigned}

-- | Assign the logger name.
setLogger :: Text -> EventUpdate
setLogger !assigned = Update \e -> e{Patrol.Event.logger = assigned}

-- | Assign the release version.
setRelease :: Text -> EventUpdate
setRelease !assigned = Update \e -> e{Patrol.Event.release = assigned}

-- | Assign the reporting server name.
setServerName :: Text -> EventUpdate
setServerName !assigned = Update \e -> e{Patrol.Event.serverName = assigned}

-- | Assign the transaction name.
setTransaction :: Text -> EventUpdate
setTransaction !assigned = Update \e -> e{Patrol.Event.transaction = assigned}

-- | Assign the protocol version.
setVersion :: Text -> EventUpdate
setVersion !assigned = Update \e -> e{Patrol.Event.version = assigned}

-- * Identity and severity

-- | Assign the event id. Capture generates one when this is left at
-- 'Patrol.Type.EventId.empty'.
setEventId :: Patrol.EventId -> EventUpdate
setEventId !assigned = Update \e -> e{Patrol.Event.eventId = assigned}

-- | Assign the severity level.
setLevel :: Patrol.Level -> EventUpdate
setLevel !assigned = Update \e -> e{Patrol.Event.level = Just assigned}

-- | Clear the severity level.
unsetLevel :: EventUpdate
unsetLevel = Update \e -> e{Patrol.Event.level = Nothing}

-- | Assign the event type.
setType :: Patrol.EventType -> EventUpdate
setType !assigned = Update \e -> e{Patrol.Event.type_ = Just assigned}

-- | Clear the event type.
unsetType :: EventUpdate
unsetType = Update \e -> e{Patrol.Event.type_ = Nothing}

-- | Assign the platform.
setPlatform :: Patrol.Platform -> EventUpdate
setPlatform !assigned = Update \e -> e{Patrol.Event.platform = Just assigned}

-- | Clear the platform.
unsetPlatform :: EventUpdate
unsetPlatform = Update \e -> e{Patrol.Event.platform = Nothing}

-- | Assign the timestamp. Capture defaults this to the current time when it is
-- absent.
setTimestamp :: UTCTime -> EventUpdate
setTimestamp !assigned = Update \e -> e{Patrol.Event.timestamp = Just assigned}

-- | Clear the timestamp.
unsetTimestamp :: EventUpdate
unsetTimestamp = Update \e -> e{Patrol.Event.timestamp = Nothing}

-- | Assign the time spent, in seconds.
setTimeSpent :: NominalDiffTime -> EventUpdate
setTimeSpent !assigned = Update \e -> e{Patrol.Event.timeSpent = Just assigned}

-- | Clear the time spent.
unsetTimeSpent :: EventUpdate
unsetTimeSpent = Update \e -> e{Patrol.Event.timeSpent = Nothing}

-- * Tags

-- | Replace every tag.
setTags :: Map Text Text -> EventUpdate
setTags !assigned = Update \e -> e{Patrol.Event.tags = assigned}

-- | Insert or overwrite one tag.
setTag :: Text -> Text -> EventUpdate
setTag !key !value = Update \e -> let !result = Map.insert key value e.tags in e{Patrol.Event.tags = result}

-- | Insert a tag only when its key is absent.
setTagIfAbsent :: Text -> Text -> EventUpdate
setTagIfAbsent !key value = Update \e ->
  if Map.member key e.tags
    then e
    else let !result = Map.insert key value e.tags in e{Patrol.Event.tags = result}

-- | Remove one tag; absent keys are ignored.
removeTag :: Text -> EventUpdate
removeTag !key = Update \e -> let !result = Map.delete key e.tags in e{Patrol.Event.tags = result}

-- | Remove every tag.
clearTags :: EventUpdate
clearTags = Update \e -> e{Patrol.Event.tags = Map.empty}

-- * Extra data

-- | Replace the whole @extra@ map.
setExtras :: Map Text Aeson.Value -> EventUpdate
setExtras !assigned = Update \e -> e{Patrol.Event.extra = assigned}

-- | Insert or overwrite one @extra@ key.
setExtra :: Text -> Aeson.Value -> EventUpdate
setExtra !key !value = Update \e -> let !result = Map.insert key value e.extra in e{Patrol.Event.extra = result}

-- | Remove one @extra@ key; absent keys are ignored.
removeExtra :: Text -> EventUpdate
removeExtra !key = Update \e -> let !result = Map.delete key e.extra in e{Patrol.Event.extra = result}

-- | Remove every @extra@ key.
clearExtras :: EventUpdate
clearExtras = Update \e -> e{Patrol.Event.extra = Map.empty}

-- * Contexts

-- | Replace every context.
setContexts :: Map Text Patrol.Context -> EventUpdate
setContexts !assigned = Update \e -> e{Patrol.Event.contexts = assigned}

-- | Insert or overwrite the context at one key. See "Sentry.Context" for the
-- variants.
setContext :: Text -> Patrol.Context -> EventUpdate
setContext !key !value = Update \e -> let !result = Map.insert key value e.contexts in e{Patrol.Event.contexts = result}

-- | Insert a context only when its key is absent.
setContextIfAbsent :: Text -> Patrol.Context -> EventUpdate
setContextIfAbsent !key value = Update \e ->
  if Map.member key e.contexts
    then e
    else let !result = Map.insert key value e.contexts in e{Patrol.Event.contexts = result}

-- | Remove the context from the supplied event, including merged scope metadata.
removeContext :: Text -> EventUpdate
removeContext !key = Update \e -> let !result = Map.delete key e.contexts in e{Patrol.Event.contexts = result}

-- | Remove every context.
clearContexts :: EventUpdate
clearContexts = Update \e -> e{Patrol.Event.contexts = Map.empty}

-- * Modules

-- | Replace the module\/version map.
setModules :: Map Text Text -> EventUpdate
setModules !assigned = Update \e -> e{Patrol.Event.modules = assigned}

-- | Insert or overwrite one module version.
setModule :: Text -> Text -> EventUpdate
setModule !key !value = Update \e -> let !result = Map.insert key value e.modules in e{Patrol.Event.modules = result}

-- | Remove one module; absent keys are ignored.
removeModule :: Text -> EventUpdate
removeModule !key = Update \e -> let !result = Map.delete key e.modules in e{Patrol.Event.modules = result}

-- | Remove every module.
clearModules :: EventUpdate
clearModules = Update \e -> e{Patrol.Event.modules = Map.empty}

-- * Fingerprint

-- | Replace the fingerprint.
setFingerprint :: [Text] -> EventUpdate
setFingerprint !assigned = Update \e -> e{Patrol.Event.fingerprint = assigned}

-- | Transform the complete list once.
modifyFingerprint :: ([Text] -> [Text]) -> EventUpdate
modifyFingerprint f = Update \e -> let !result = f e.fingerprint in e{Patrol.Event.fingerprint = result}

-- | Append a component, preserving order and duplicates; no default is added.
appendFingerprintComponent :: Text -> EventUpdate
appendFingerprintComponent !value = modifyFingerprint (Fingerprint.append value)

-- | Prepend a component, preserving duplicates; no default is added.
prependFingerprintComponent :: Text -> EventUpdate
prependFingerprintComponent !value = modifyFingerprint (Fingerprint.prepend value)

-- | Prepend the canonical default token if neither recognized spelling occurs.
-- Existing tokens keep their position and spelling.
ensureDefaultFingerprint :: EventUpdate
ensureDefaultFingerprint = modifyFingerprint Fingerprint.ensureDefault

-- | Remove every @{{ default }}@ and @{{default}}@ token.
removeDefaultFingerprint :: EventUpdate
removeDefaultFingerprint = modifyFingerprint Fingerprint.removeDefault

-- | Store an empty fingerprint. Scope grouping can still supply a default at
-- capture; clear in a processor after merging to force normal grouping.
clearFingerprint :: EventUpdate
clearFingerprint = setFingerprint []

-- * Processing errors

-- | Replace the processing-error list.
setErrors :: [Patrol.EventProcessingError] -> EventUpdate
setErrors !assigned = Update \e -> e{Patrol.Event.errors = assigned}

-- | Append one processing error.
appendError :: Patrol.EventProcessingError -> EventUpdate
appendError !value = Update \e -> let !result = e.errors <> [value] in e{Patrol.Event.errors = result}

-- | Remove every processing error.
clearErrors :: EventUpdate
clearErrors = Update \e -> e{Patrol.Event.errors = []}

-- * User

-- | Establish the event's user from an empty one, replacing any it already
-- had.
--
-- Accepts a 'Sentry.User.UserUpdate', a list of them, or a
-- 'Sentry.User.User' you already hold.
setUser :: (Witch.From a Sentry.User.UserUpdate) => a -> EventUpdate
setUser upd = Update \e ->
  let !u = Sentry.Update.run upd Sentry.User.empty in e{Patrol.Event.user = Just u}

-- | Modify the user, starting from empty when absent. The result is forced before storing.
modifyUser :: (Witch.From a Sentry.User.UserUpdate) => a -> EventUpdate
modifyUser upd = Update \e -> case e.user of
  Nothing -> Sentry.Update.run (setUser upd) e
  Just _ -> Sentry.Update.run (modifyExistingUser upd) e

-- | Modify an existing user. When absent, leave it absent without evaluating the update.
modifyExistingUser :: (Witch.From a Sentry.User.UserUpdate) => a -> EventUpdate
modifyExistingUser upd = Update \e -> case e.user of
  Nothing -> e
  Just u -> let !u' = Sentry.Update.run upd u in e{Patrol.Event.user = Just u'}

-- | Remove the user. The usual first move when scrubbing PII.
unsetUser :: EventUpdate
unsetUser = Update \e -> e{Patrol.Event.user = Nothing}

-- * Integration-populated payloads

-- | Replace breadcrumbs from a collection update, list of updates, or record.
setBreadcrumbs :: (Witch.From a Sentry.Breadcrumb.BreadcrumbsUpdate) => a -> EventUpdate
setBreadcrumbs upd = Update \e -> let !child = Sentry.Update.run upd Sentry.Breadcrumb.emptyCollection in e{Patrol.Event.breadcrumbs = Just child}

-- | Modify the payload, creating from empty when absent.
modifyBreadcrumbs :: (Witch.From a Sentry.Breadcrumb.BreadcrumbsUpdate) => a -> EventUpdate
modifyBreadcrumbs upd = Update \e -> case e.breadcrumbs of
  Nothing -> Sentry.Update.run (setBreadcrumbs upd) e
  Just _ -> Sentry.Update.run (modifyExistingBreadcrumbs upd) e

-- | Modify a present payload; absence skips the update.
modifyExistingBreadcrumbs :: (Witch.From a Sentry.Breadcrumb.BreadcrumbsUpdate) => a -> EventUpdate
modifyExistingBreadcrumbs upd = Update \e -> case e.breadcrumbs of
  Nothing -> e
  Just old -> let !child = Sentry.Update.run upd old in e{Patrol.Event.breadcrumbs = Just child}

-- | Remove the optional payload.
unsetBreadcrumbs :: EventUpdate
unsetBreadcrumbs = Update \e -> e{Patrol.Event.breadcrumbs = Nothing}

-- | Assign the debug metadata.
setDebugMeta :: Patrol.DebugMeta -> EventUpdate
setDebugMeta !assigned = Update \e -> e{Patrol.Event.debugMeta = Just assigned}

-- | Remove the debug metadata.
unsetDebugMeta :: EventUpdate
unsetDebugMeta = Update \e -> e{Patrol.Event.debugMeta = Nothing}

-- | Replace the exception chain from an update, list of updates, or record.
setExceptionChain :: (Witch.From a Sentry.Exception.ExceptionsUpdate) => a -> EventUpdate
setExceptionChain upd = Update \e -> let !child = Sentry.Update.run upd Sentry.Exception.emptyChain in e{Patrol.Event.exception = Just child}

-- | Modify the payload, creating from empty when absent.
modifyExceptionChain :: (Witch.From a Sentry.Exception.ExceptionsUpdate) => a -> EventUpdate
modifyExceptionChain upd = Update \e -> case e.exception of
  Nothing -> Sentry.Update.run (setExceptionChain upd) e
  Just _ -> Sentry.Update.run (modifyExistingExceptionChain upd) e

-- | Modify a present payload; absence skips the update.
modifyExistingExceptionChain :: (Witch.From a Sentry.Exception.ExceptionsUpdate) => a -> EventUpdate
modifyExistingExceptionChain upd = Update \e -> case e.exception of
  Nothing -> e
  Just old -> let !child = Sentry.Update.run upd old in e{Patrol.Event.exception = Just child}

-- | Remove the optional payload.
unsetExceptionChain :: EventUpdate
unsetExceptionChain = Update \e -> e{Patrol.Event.exception = Nothing}

-- | Assign the log entry (the message body).
setLogentry :: Patrol.LogEntry -> EventUpdate
setLogentry !assigned = Update \e -> e{Patrol.Event.logentry = Just assigned}

-- | Remove the log entry.
unsetLogentry :: EventUpdate
unsetLogentry = Update \e -> e{Patrol.Event.logentry = Nothing}

-- | Replace the HTTP request payload from an update, list, or record.
setRequest :: (Witch.From a Sentry.Request.RequestUpdate) => a -> EventUpdate
setRequest upd = Update \e -> let !r = Sentry.Update.run upd Sentry.Request.empty in e{Patrol.Event.request = Just r}

-- | Modify the request, starting from empty when absent. The result is forced before storing.
modifyRequest :: (Witch.From a Sentry.Request.RequestUpdate) => a -> EventUpdate
modifyRequest upd = Update \e -> case e.request of
  Nothing -> Sentry.Update.run (setRequest upd) e
  Just _ -> Sentry.Update.run (modifyExistingRequest upd) e

-- | Modify an existing request. When absent, leave it absent without evaluating the update.
modifyExistingRequest :: (Witch.From a Sentry.Request.RequestUpdate) => a -> EventUpdate
modifyExistingRequest upd = Update \e -> case e.request of
  Nothing -> e
  Just r -> let !result = Sentry.Update.run upd r in e{Patrol.Event.request = Just result}

-- | Remove the HTTP request payload.
unsetRequest :: EventUpdate
unsetRequest = Update \e -> e{Patrol.Event.request = Nothing}

-- | Assign the SDK metadata. Capture fills this in when it is absent.
setSdk :: Patrol.ClientSdkInfo -> EventUpdate
setSdk !assigned = Update \e -> e{Patrol.Event.sdk = Just assigned}

-- | Remove the SDK metadata.
unsetSdk :: EventUpdate
unsetSdk = Update \e -> e{Patrol.Event.sdk = Nothing}

-- | Assign the thread list.
setThreads :: Patrol.Threads -> EventUpdate
setThreads !assigned = Update \e -> e{Patrol.Event.threads = Just assigned}

-- | Remove the thread list.
unsetThreads :: EventUpdate
unsetThreads = Update \e -> e{Patrol.Event.threads = Nothing}

-- | Assign the transaction metadata.
setTransactionInfo :: Patrol.TransactionInfo -> EventUpdate
setTransactionInfo !assigned = Update \e -> e{Patrol.Event.transactionInfo = Just assigned}

-- | Remove the transaction metadata.
unsetTransactionInfo :: EventUpdate
unsetTransactionInfo = Update \e -> e{Patrol.Event.transactionInfo = Nothing}

-- * Whole-event builders

-- | Build a minimal 'Event' from a 'SomeException'. Attaches no
-- 'Patrol.Type.Mechanism.Mechanism' — use 'fromExceptionWith' for that.
--
-- Sets the exception, level, and type; leaves the event id, timestamp, SDK,
-- platform, and all option-derived fields (release, environment, server name,
-- dist) to sentinel values that 'Sentry.Capture.applyClientDefaults' can match
-- on and fill at capture time.
--
-- __NOTE__: This replaces @patrol@'s @fromSomeException@ as the SDK's own
-- exception event builder, leaving capture to apply configured defaults.
fromException :: SomeException -> Event
fromException = fromExceptionWith Nothing

-- | Like 'fromException', but attaches the given
-- 'Patrol.Type.Mechanism.Mechanism' to the exception.
fromExceptionWith :: Maybe Patrol.Mechanism -> SomeException -> Event
fromExceptionWith mMechanism e =
  let event =
        Patrol.Event.empty
          { Patrol.Event.exception = Just (Patrol.Exceptions.fromSomeException e),
            Patrol.Event.level = Just Patrol.Level.Error,
            Patrol.Event.type_ = Just Patrol.EventType.Default
          }
   in maybe event (`attachMechanism` event) mMechanism

-- | Build a minimal 'Event' for a plain text message at the given severity
-- level.
--
-- Sets the log entry and level; leaves the event id, timestamp, SDK, platform,
-- and all option-derived fields to sentinel values that
-- 'Sentry.Capture.applyClientDefaults' can match on and fill at capture time.
fromMessage :: Patrol.Level -> Text -> Event
fromMessage lvl msg =
  let logEntry =
        Patrol.LogEntry.empty
          { Patrol.LogEntry.formatted = msg,
            Patrol.LogEntry.message = msg
          }
   in Patrol.Event.empty
        { Patrol.Event.level = Just lvl,
          Patrol.Event.logentry = Just logEntry
        }

-- | Attach a 'Patrol.Type.Mechanism.Mechanism' to the /last/ entry in the
-- event's @exception.values@, which /should/ be the exception that is actually
-- unhandled.
--
-- __NOTE__: If the mechanism type is empty, it is normalized to
-- @\"generic\"@.
attachMechanism :: Patrol.Mechanism -> Event -> Event
attachMechanism mechanism event =
  case event.exception of
    Nothing -> event
    Just exceptions ->
      case unsnoc (Patrol.Exceptions.values exceptions) of
        Nothing -> event
        Just (rest, lastExc) ->
          let normalized =
                if Text.null mechanism.type_
                  then mechanism{Patrol.Mechanism.type_ = "generic"}
                  else mechanism
              updated = lastExc{Patrol.Exception.mechanism = Just normalized}
           in event
                { Patrol.Event.exception =
                    Just exceptions{Patrol.Exceptions.values = rest <> [updated]}
                }

-- | Apply a single builder, a list, or a replacement record to an existing
-- 'Event'.
apply :: (Witch.From a EventUpdate) => Event -> a -> Event
apply = flip Sentry.Update.run

-- | Replace a custom context; later duplicate keys win.
setContextValues :: Text -> [(Text, Aeson.Value)] -> EventUpdate
setContextValues k kvs = let !values = Map.fromList kvs in setContext k (Patrol.Context.Other values)

-- | Insert a custom field. Typed contexts are unchanged.
setContextValue :: Text -> Text -> Aeson.Value -> EventUpdate
setContextValue k key value = modifyContextValues k (Map.insert key value)

-- | Remove a field from the supplied event, including merged scope metadata.
removeContextValue :: Text -> Text -> EventUpdate
removeContextValue k key = Update \e -> let !result = Sentry.Context.Internal.removeValue k key e.contexts in e{Patrol.Event.contexts = result}

-- | Transform a custom payload, starting from empty when absent. Typed contexts
-- remain unchanged without evaluating the transformation.
modifyContextValues :: Text -> (Map Text Aeson.Value -> Map Text Aeson.Value) -> EventUpdate
modifyContextValues k f = Update \e -> let !result = Sentry.Context.Internal.modifyValues k f e.contexts in e{Patrol.Event.contexts = result}

-- | Replace the entire @"os"@ payload from an update, list, or record.
setOsContext :: (Witch.From a Sentry.OsContext.OsContextUpdate) => a -> EventUpdate
setOsContext upd = let !record = Sentry.Update.run upd Sentry.OsContext.empty in setContext "os" (Patrol.Context.Os record)

-- | Replace the entire @"app"@ payload from an update, list, or record.
setAppContext :: (Witch.From a Sentry.AppContext.AppContextUpdate) => a -> EventUpdate
setAppContext upd = let !record = Sentry.Update.run upd Sentry.AppContext.empty in setContext "app" (Patrol.Context.App record)

-- | Replace the entire @"runtime"@ payload from an update, list, or record.
setRuntimeContext :: (Witch.From a Sentry.RuntimeContext.RuntimeContextUpdate) => a -> EventUpdate
setRuntimeContext upd = let !record = Sentry.Update.run upd Sentry.RuntimeContext.empty in setContext "runtime" (Patrol.Context.Runtime record)

-- | Modify the OS context, starting from empty when absent.
-- Other context variants are unchanged without evaluating the update.
modifyOsContext :: (Witch.From a Sentry.OsContext.OsContextUpdate) => a -> EventUpdate
modifyOsContext upd = Update \e ->
  let !result = Sentry.Context.Internal.modifyTyped True "os" Sentry.OsContext.empty project Patrol.Context.Os (Sentry.Update.run upd) e.contexts
   in e{Patrol.Event.contexts = result}
  where
    project (Patrol.Context.Os record) = Just record
    project _ = Nothing

-- | Modify the OS context only when present; absent contexts skip the update.
-- Other context variants are unchanged without evaluating the update.
modifyExistingOsContext :: (Witch.From a Sentry.OsContext.OsContextUpdate) => a -> EventUpdate
modifyExistingOsContext upd = Update \e ->
  let !result = Sentry.Context.Internal.modifyTyped False "os" Sentry.OsContext.empty project Patrol.Context.Os (Sentry.Update.run upd) e.contexts
   in e{Patrol.Event.contexts = result}
  where
    project (Patrol.Context.Os record) = Just record
    project _ = Nothing

-- | Modify the app context, starting from empty when absent.
-- Other context variants are unchanged without evaluating the update.
modifyAppContext :: (Witch.From a Sentry.AppContext.AppContextUpdate) => a -> EventUpdate
modifyAppContext upd = Update \e ->
  let !result = Sentry.Context.Internal.modifyTyped True "app" Sentry.AppContext.empty project Patrol.Context.App (Sentry.Update.run upd) e.contexts
   in e{Patrol.Event.contexts = result}
  where
    project (Patrol.Context.App record) = Just record
    project _ = Nothing

-- | Modify the app context only when present; absent contexts skip the update.
-- Other context variants are unchanged without evaluating the update.
modifyExistingAppContext :: (Witch.From a Sentry.AppContext.AppContextUpdate) => a -> EventUpdate
modifyExistingAppContext upd = Update \e ->
  let !result = Sentry.Context.Internal.modifyTyped False "app" Sentry.AppContext.empty project Patrol.Context.App (Sentry.Update.run upd) e.contexts
   in e{Patrol.Event.contexts = result}
  where
    project (Patrol.Context.App record) = Just record
    project _ = Nothing

-- | Modify the runtime context, starting from empty when absent.
-- Other context variants are unchanged without evaluating the update.
modifyRuntimeContext :: (Witch.From a Sentry.RuntimeContext.RuntimeContextUpdate) => a -> EventUpdate
modifyRuntimeContext upd = Update \e ->
  let !result = Sentry.Context.Internal.modifyTyped True "runtime" Sentry.RuntimeContext.empty project Patrol.Context.Runtime (Sentry.Update.run upd) e.contexts
   in e{Patrol.Event.contexts = result}
  where
    project (Patrol.Context.Runtime record) = Just record
    project _ = Nothing

-- | Modify the runtime context only when present; absent contexts skip the update.
-- Other context variants are unchanged without evaluating the update.
modifyExistingRuntimeContext :: (Witch.From a Sentry.RuntimeContext.RuntimeContextUpdate) => a -> EventUpdate
modifyExistingRuntimeContext upd = Update \e ->
  let !result = Sentry.Context.Internal.modifyTyped False "runtime" Sentry.RuntimeContext.empty project Patrol.Context.Runtime (Sentry.Update.run upd) e.contexts
   in e{Patrol.Event.contexts = result}
  where
    project (Patrol.Context.Runtime record) = Just record
    project _ = Nothing

-- | Replace the entire @"browser"@ payload from an update, list, or record.
setBrowserContext :: (Witch.From a Sentry.BrowserContext.BrowserContextUpdate) => a -> EventUpdate
setBrowserContext upd = let !record = Sentry.Update.run upd Sentry.BrowserContext.empty in setContext "browser" (Patrol.Context.Browser record)

-- | Modify the browser context, starting from empty when absent.
-- Other context variants are unchanged without evaluating the update.
modifyBrowserContext :: (Witch.From a Sentry.BrowserContext.BrowserContextUpdate) => a -> EventUpdate
modifyBrowserContext upd = Update \e ->
  let !result = Sentry.Context.Internal.modifyTyped True "browser" Sentry.BrowserContext.empty project Patrol.Context.Browser (Sentry.Update.run upd) e.contexts
   in e{Patrol.Event.contexts = result}
  where
    project (Patrol.Context.Browser record) = Just record
    project _ = Nothing

-- | Modify the browser context only when present; absent contexts skip the update.
-- Other context variants are unchanged without evaluating the update.
modifyExistingBrowserContext :: (Witch.From a Sentry.BrowserContext.BrowserContextUpdate) => a -> EventUpdate
modifyExistingBrowserContext upd = Update \e ->
  let !result = Sentry.Context.Internal.modifyTyped False "browser" Sentry.BrowserContext.empty project Patrol.Context.Browser (Sentry.Update.run upd) e.contexts
   in e{Patrol.Event.contexts = result}
  where
    project (Patrol.Context.Browser record) = Just record
    project _ = Nothing

-- | Replace the entire @"device"@ payload from an update, list, or record.
setDeviceContext :: (Witch.From a Sentry.DeviceContext.DeviceContextUpdate) => a -> EventUpdate
setDeviceContext upd = let !record = Sentry.Update.run upd Sentry.DeviceContext.empty in setContext "device" (Patrol.Context.Device record)

-- | Modify the device context, starting from empty when absent.
-- Other context variants are unchanged without evaluating the update.
modifyDeviceContext :: (Witch.From a Sentry.DeviceContext.DeviceContextUpdate) => a -> EventUpdate
modifyDeviceContext upd = Update \e ->
  let !result = Sentry.Context.Internal.modifyTyped True "device" Sentry.DeviceContext.empty project Patrol.Context.Device (Sentry.Update.run upd) e.contexts
   in e{Patrol.Event.contexts = result}
  where
    project (Patrol.Context.Device record) = Just record
    project _ = Nothing

-- | Modify the device context only when present; absent contexts skip the update.
-- Other context variants are unchanged without evaluating the update.
modifyExistingDeviceContext :: (Witch.From a Sentry.DeviceContext.DeviceContextUpdate) => a -> EventUpdate
modifyExistingDeviceContext upd = Update \e ->
  let !result = Sentry.Context.Internal.modifyTyped False "device" Sentry.DeviceContext.empty project Patrol.Context.Device (Sentry.Update.run upd) e.contexts
   in e{Patrol.Event.contexts = result}
  where
    project (Patrol.Context.Device record) = Just record
    project _ = Nothing

-- | Replace the entire @"trace"@ payload from an update, list, or record.
setTraceContext :: (Witch.From a Sentry.TraceContext.TraceContextUpdate) => a -> EventUpdate
setTraceContext upd = let !record = Sentry.Update.run upd Sentry.TraceContext.empty in setContext "trace" (Patrol.Context.Trace record)

-- | Modify the trace context, starting from empty when absent.
-- Other context variants are unchanged without evaluating the update.
modifyTraceContext :: (Witch.From a Sentry.TraceContext.TraceContextUpdate) => a -> EventUpdate
modifyTraceContext upd = Update \e ->
  let !result = Sentry.Context.Internal.modifyTyped True "trace" Sentry.TraceContext.empty project Patrol.Context.Trace (Sentry.Update.run upd) e.contexts
   in e{Patrol.Event.contexts = result}
  where
    project (Patrol.Context.Trace record) = Just record
    project _ = Nothing

-- | Modify the trace context only when present; absent contexts skip the update.
-- Other context variants are unchanged without evaluating the update.
modifyExistingTraceContext :: (Witch.From a Sentry.TraceContext.TraceContextUpdate) => a -> EventUpdate
modifyExistingTraceContext upd = Update \e ->
  let !result = Sentry.Context.Internal.modifyTyped False "trace" Sentry.TraceContext.empty project Patrol.Context.Trace (Sentry.Update.run upd) e.contexts
   in e{Patrol.Event.contexts = result}
  where
    project (Patrol.Context.Trace record) = Just record
    project _ = Nothing

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalLevel :: Maybe Patrol.Level -> EventUpdate
setOptionalLevel = maybe unsetLevel setLevel

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalType :: Maybe Patrol.EventType -> EventUpdate
setOptionalType = maybe unsetType setType

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalPlatform :: Maybe Patrol.Platform -> EventUpdate
setOptionalPlatform = maybe unsetPlatform setPlatform

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalTimestamp :: Maybe UTCTime -> EventUpdate
setOptionalTimestamp = maybe unsetTimestamp setTimestamp

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalTimeSpent :: Maybe NominalDiffTime -> EventUpdate
setOptionalTimeSpent = maybe unsetTimeSpent setTimeSpent

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalTag :: Text -> Maybe Text -> EventUpdate
setOptionalTag key = maybe (removeTag key) (setTag key)

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalExtra :: Text -> Maybe Aeson.Value -> EventUpdate
setOptionalExtra key = maybe (removeExtra key) (setExtra key)

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalContext :: Text -> Maybe Patrol.Context -> EventUpdate
setOptionalContext key = maybe (removeContext key) (setContext key)

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalModule :: Text -> Maybe Text -> EventUpdate
setOptionalModule key = maybe (removeModule key) (setModule key)

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalUser :: Maybe Sentry.User.User -> EventUpdate
setOptionalUser = maybe unsetUser setUser

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalBreadcrumbs :: Maybe Sentry.Breadcrumb.Breadcrumbs -> EventUpdate
setOptionalBreadcrumbs = maybe unsetBreadcrumbs setBreadcrumbs

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalDebugMeta :: Maybe Patrol.DebugMeta -> EventUpdate
setOptionalDebugMeta = maybe unsetDebugMeta setDebugMeta

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalExceptionChain :: Maybe Sentry.Exception.Exceptions -> EventUpdate
setOptionalExceptionChain = maybe unsetExceptionChain setExceptionChain

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalLogentry :: Maybe Patrol.LogEntry -> EventUpdate
setOptionalLogentry = maybe unsetLogentry setLogentry

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalRequest :: Maybe Sentry.Request.Request -> EventUpdate
setOptionalRequest = maybe unsetRequest setRequest

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalSdk :: Maybe Patrol.ClientSdkInfo -> EventUpdate
setOptionalSdk = maybe unsetSdk setSdk

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalThreads :: Maybe Patrol.Threads -> EventUpdate
setOptionalThreads = maybe unsetThreads setThreads

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalTransactionInfo :: Maybe Patrol.TransactionInfo -> EventUpdate
setOptionalTransactionInfo = maybe unsetTransactionInfo setTransactionInfo

-- | Replace a named context with custom values, or remove it with 'Nothing'.
-- 'Just' an empty list retains a present empty custom context.
setOptionalContextValues :: Text -> Maybe [(Text, Aeson.Value)] -> EventUpdate
setOptionalContextValues key = maybe (removeContext key) (setContextValues key)

-- | Assign or remove a custom-context field. Typed payloads are unchanged.
-- Removal never creates a context; deleting its last field retains it.
setOptionalContextValue :: Text -> Text -> Maybe Aeson.Value -> EventUpdate
setOptionalContextValue key fieldKey = maybe (removeContextValue key fieldKey) (setContextValue key fieldKey)

-- | Replace the @"os"@ context with the supplied record, or remove the
-- entry with 'Nothing', regardless of its previous variant.
setOptionalOsContext :: Maybe Sentry.OsContext.OsContext -> EventUpdate
setOptionalOsContext = maybe (removeContext "os") setOsContext

-- | Replace the @"app"@ context with the supplied record, or remove the
-- entry with 'Nothing', regardless of its previous variant.
setOptionalAppContext :: Maybe Sentry.AppContext.AppContext -> EventUpdate
setOptionalAppContext = maybe (removeContext "app") setAppContext

-- | Replace the @"runtime"@ context with the supplied record, or remove the
-- entry with 'Nothing', regardless of its previous variant.
setOptionalRuntimeContext :: Maybe Sentry.RuntimeContext.RuntimeContext -> EventUpdate
setOptionalRuntimeContext = maybe (removeContext "runtime") setRuntimeContext

-- | Replace the @"browser"@ context with the supplied record, or remove the
-- entry with 'Nothing', regardless of its previous variant.
setOptionalBrowserContext :: Maybe Sentry.BrowserContext.BrowserContext -> EventUpdate
setOptionalBrowserContext = maybe (removeContext "browser") setBrowserContext

-- | Replace the @"device"@ context with the supplied record, or remove the
-- entry with 'Nothing', regardless of its previous variant.
setOptionalDeviceContext :: Maybe Sentry.DeviceContext.DeviceContext -> EventUpdate
setOptionalDeviceContext = maybe (removeContext "device") setDeviceContext

-- | Replace the @"trace"@ context with the supplied record, or remove the
-- entry with 'Nothing', regardless of its previous variant.
setOptionalTraceContext :: Maybe Sentry.TraceContext.TraceContext -> EventUpdate
setOptionalTraceContext = maybe (removeContext "trace") setTraceContext

-- | Look up a stored entry by key.
lookupTag :: Text -> Event -> Maybe Text
lookupTag key record = Map.lookup key record.tags

-- | Look up a stored entry by key.
lookupExtra :: Text -> Event -> Maybe Aeson.Value
lookupExtra key record = Map.lookup key record.extra

-- | Look up a stored entry by key.
lookupModule :: Text -> Event -> Maybe Text
lookupModule key record = Map.lookup key record.modules

-- | Look up a stored entry by key.
lookupContext :: Text -> Event -> Maybe Patrol.Context
lookupContext key record = Map.lookup key record.contexts

-- | Return the canonical typed context, or 'Nothing' for absence or a mismatch.
lookupAppContext :: Event -> Maybe Sentry.AppContext.AppContext
lookupAppContext record = case Map.lookup "app" record.contexts of
  Just (Patrol.Context.App value) -> Just value
  _ -> Nothing

-- | Alter an absent or matching context. Mismatches skip the callback;
-- use an explicit setter to replace a different variant.
alterAppContext :: (Maybe Sentry.AppContext.AppContext -> Maybe Sentry.AppContext.AppContext) -> EventUpdate
alterAppContext f = Update \record ->
  let !result = Sentry.Context.Internal.alterTyped "app" project Patrol.Context.App f record.contexts
   in record{Patrol.Event.contexts = result}
  where
    project (Patrol.Context.App value) = Just value
    project _ = Nothing

-- | Return the canonical typed context, or 'Nothing' for absence or a mismatch.
lookupOsContext :: Event -> Maybe Sentry.OsContext.OsContext
lookupOsContext record = case Map.lookup "os" record.contexts of
  Just (Patrol.Context.Os value) -> Just value
  _ -> Nothing

-- | Alter an absent or matching context. Mismatches skip the callback;
-- use an explicit setter to replace a different variant.
alterOsContext :: (Maybe Sentry.OsContext.OsContext -> Maybe Sentry.OsContext.OsContext) -> EventUpdate
alterOsContext f = Update \record ->
  let !result = Sentry.Context.Internal.alterTyped "os" project Patrol.Context.Os f record.contexts
   in record{Patrol.Event.contexts = result}
  where
    project (Patrol.Context.Os value) = Just value
    project _ = Nothing

-- | Return the canonical typed context, or 'Nothing' for absence or a mismatch.
lookupRuntimeContext :: Event -> Maybe Sentry.RuntimeContext.RuntimeContext
lookupRuntimeContext record = case Map.lookup "runtime" record.contexts of
  Just (Patrol.Context.Runtime value) -> Just value
  _ -> Nothing

-- | Alter an absent or matching context. Mismatches skip the callback;
-- use an explicit setter to replace a different variant.
alterRuntimeContext :: (Maybe Sentry.RuntimeContext.RuntimeContext -> Maybe Sentry.RuntimeContext.RuntimeContext) -> EventUpdate
alterRuntimeContext f = Update \record ->
  let !result = Sentry.Context.Internal.alterTyped "runtime" project Patrol.Context.Runtime f record.contexts
   in record{Patrol.Event.contexts = result}
  where
    project (Patrol.Context.Runtime value) = Just value
    project _ = Nothing

-- | Return the canonical typed context, or 'Nothing' for absence or a mismatch.
lookupBrowserContext :: Event -> Maybe Sentry.BrowserContext.BrowserContext
lookupBrowserContext record = case Map.lookup "browser" record.contexts of
  Just (Patrol.Context.Browser value) -> Just value
  _ -> Nothing

-- | Alter an absent or matching context. Mismatches skip the callback;
-- use an explicit setter to replace a different variant.
alterBrowserContext :: (Maybe Sentry.BrowserContext.BrowserContext -> Maybe Sentry.BrowserContext.BrowserContext) -> EventUpdate
alterBrowserContext f = Update \record ->
  let !result = Sentry.Context.Internal.alterTyped "browser" project Patrol.Context.Browser f record.contexts
   in record{Patrol.Event.contexts = result}
  where
    project (Patrol.Context.Browser value) = Just value
    project _ = Nothing

-- | Return the canonical typed context, or 'Nothing' for absence or a mismatch.
lookupDeviceContext :: Event -> Maybe Sentry.DeviceContext.DeviceContext
lookupDeviceContext record = case Map.lookup "device" record.contexts of
  Just (Patrol.Context.Device value) -> Just value
  _ -> Nothing

-- | Alter an absent or matching context. Mismatches skip the callback;
-- use an explicit setter to replace a different variant.
alterDeviceContext :: (Maybe Sentry.DeviceContext.DeviceContext -> Maybe Sentry.DeviceContext.DeviceContext) -> EventUpdate
alterDeviceContext f = Update \record ->
  let !result = Sentry.Context.Internal.alterTyped "device" project Patrol.Context.Device f record.contexts
   in record{Patrol.Event.contexts = result}
  where
    project (Patrol.Context.Device value) = Just value
    project _ = Nothing

-- | Return the canonical typed context, or 'Nothing' for absence or a mismatch.
lookupTraceContext :: Event -> Maybe Sentry.TraceContext.TraceContext
lookupTraceContext record = case Map.lookup "trace" record.contexts of
  Just (Patrol.Context.Trace value) -> Just value
  _ -> Nothing

-- | Alter an absent or matching context. Mismatches skip the callback;
-- use an explicit setter to replace a different variant.
alterTraceContext :: (Maybe Sentry.TraceContext.TraceContext -> Maybe Sentry.TraceContext.TraceContext) -> EventUpdate
alterTraceContext f = Update \record ->
  let !result = Sentry.Context.Internal.alterTyped "trace" project Patrol.Context.Trace f record.contexts
   in record{Patrol.Event.contexts = result}
  where
    project (Patrol.Context.Trace value) = Just value
    project _ = Nothing

-- | Look up a custom field; typed payloads have no custom fields.
lookupContextValue :: Text -> Text -> Event -> Maybe Aeson.Value
lookupContextValue key field record = Sentry.Context.Internal.lookupValue key field record.contexts

-- | Transform a present custom field. Missing fields and typed payloads skip the callback.
modifyExistingContextValue :: Text -> Text -> (Aeson.Value -> Aeson.Value) -> EventUpdate
modifyExistingContextValue key field f = alterContextValue key field (fmap f)

-- | Insert, replace, or remove a custom field. Typed payloads skip the callback.
-- Removing the final field retains an empty context; absent removals create nothing.
alterContextValue :: Text -> Text -> (Maybe Aeson.Value -> Maybe Aeson.Value) -> EventUpdate
alterContextValue key field f = Update \record ->
  let !result = Sentry.Context.Internal.alterValue key field f record.contexts
   in record{Patrol.Event.contexts = result}

-- | Return the first matching fingerprint component.
findFingerprintComponent :: (Text -> Bool) -> Event -> Maybe Text
findFingerprintComponent predicate record = Foldable.find predicate record.fingerprint

-- | Keep matching components in order, preserving duplicates.
filterFingerprint :: (Text -> Bool) -> EventUpdate
filterFingerprint predicate = modifyFingerprint (filter predicate)

-- | Return the first matching processing error.
findError :: (Patrol.EventProcessingError -> Bool) -> Event -> Maybe Patrol.EventProcessingError
findError predicate record = Foldable.find predicate record.errors

-- | Keep matching errors in order, preserving duplicates.
filterErrors :: (Patrol.EventProcessingError -> Bool) -> EventUpdate
filterErrors predicate = with \record -> setErrors (filter predicate record.errors)
