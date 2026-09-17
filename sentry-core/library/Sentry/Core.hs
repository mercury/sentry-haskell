-- |  This module re-exports the API that consumers will typically use
-- when instrumenting software.
--
-- Additional operations are available through qualified imports of
-- "Sentry.Scope", "Sentry.Transport", and "Sentry.Integration".
module Sentry.Core
  ( -- * Lifecycle
    init,
    close,
    withSentry,
    withScopedClient,
    ClientHandle,
    acquireClient,
    clientOf,
    ShutdownResponse (..),

    -- * Client
    Client,
    pattern NON_RECORDING_CLIENT,

    -- * Client Options
    ClientOptions (..),
    DsnSource (..),
    defaultClientOptions,
    TransportProvider (..),
    disableIntegration,

    -- * Stacktrace integrations
    AttachAnnotatedExceptionIntegration (..),
    AttachCallStackIntegration (..),
    AttachExceptionContextIntegration (..),
    ProcessStacktraceIntegration (..),

    -- * Capturing Events
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

    -- * Capture Overrides
    CaptureOverrides (..),

    -- * Captured Event
    CapturedEvent (..),

    -- * Scope
    Scope,
    ScopeData (..),
    ScopeType (..),
    withScope,
    withIsolationScope,
    withClient,
    resolveClient,
    resolveClientAt,
    getIsolationScope,
    getCurrentScope,
    readScopeRef,
    readMergedScope,

    -- * Scope propagation
    propagateScope,

    -- * Record types
    User,
    Geo,
    Breadcrumb,
    Event,
    OsContext,
    AppContext,
    Request,
    RuntimeContext,
    Mechanism,
    Level (..),
    BreadcrumbType,

    -- * Update types

    -- | Every pure metadata change is a value of one of these. They all share
    -- one composition idiom; see "Sentry.Update".
    ScopeUpdate,
    Update,
    UserUpdate,
    GeoUpdate,
    BreadcrumbUpdate,
    EventUpdate,
    OsContextUpdate,
    AppContextUpdate,
    RequestUpdate,
    RuntimeContextUpdate,
    MechanismUpdate,

    -- * Breadcrumbs
    addBreadcrumb,
    addBreadcrumbAt,
    addBreadcrumbs,
    addBreadcrumbsAt,
    clearBreadcrumbs,
    clearBreadcrumbsAt,

    -- * Scope metadata
    updateScope,
    setLevel,
    unsetLevel,
    setUser,
    setOptionalUser,
    setOptionalTraceContext,
    setOptionalDeviceContext,
    setOptionalBrowserContext,
    setOptionalAppContext,
    setOptionalOsContext,
    setOptionalContextValue,
    setOptionalContextValues,
    setOptionalRuntimeContext,
    setOptionalContext,
    setOptionalExtra,
    setOptionalTag,
    setOptionalTransaction,
    setOptionalFingerprint,
    setOptionalLevel,
    unsetUser,
    modifyUser,
    modifyExistingUser,
    setTag,
    removeTag,
    clearTags,
    setExtra,
    removeExtra,
    clearExtras,
    setContext,
    setContextValues,
    setContextValue,
    removeContextValue,
    modifyContextValues,
    setOsContext,
    setAppContext,
    setRuntimeContext,
    removeContext,
    clearContexts,
    setFingerprint,
    unsetFingerprint,
    setTransaction,
    unsetTransaction,
    configureGlobal,
    alterAppContext,
    alterOsContext,
    alterRuntimeContext,
    alterBrowserContext,
    alterDeviceContext,
    alterTraceContext,
    modifyExistingContextValue,
    alterContextValue,
    filterFingerprint,
    filterBreadcrumbs,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson qualified as Aeson
import Data.Map.Strict (Map)
import Data.Text (Text)
import Patrol qualified
import Sentry.AppContext (AppContext, AppContextUpdate)
import Sentry.Breadcrumb (Breadcrumb, BreadcrumbType, BreadcrumbUpdate)
import Sentry.BrowserContext qualified
import Sentry.Capture
  ( CaptureOverrides (..),
    captureEvent,
    captureEvent_,
    captureException,
    captureExceptionWith,
    captureExceptionWith_,
    captureException_,
    captureMessage,
    captureMessage_,
    captureUnhandledException,
    captureUnhandledException_,
  )
import Sentry.Client (Client, disableIntegration, pattern NON_RECORDING_CLIENT)
import Sentry.Client.Options (ClientOptions (..), DsnSource (..), TransportProvider (..), defaultClientOptions)
import Sentry.DeviceContext qualified
import Sentry.Event (Event, EventUpdate)
import Sentry.Event.Captured (CapturedEvent (..))
import Sentry.Geo (Geo, GeoUpdate)
import Sentry.Init (ClientHandle, acquireClient, clientOf, close, init, withScopedClient, withSentry)
import Sentry.Integration.Stacktrace
  ( AttachAnnotatedExceptionIntegration (..),
    AttachCallStackIntegration (..),
    AttachExceptionContextIntegration (..),
    ProcessStacktraceIntegration (..),
  )
import Sentry.Level (Level (..))
import Sentry.Mechanism (Mechanism, MechanismUpdate)
import Sentry.OsContext (OsContext, OsContextUpdate)
import Sentry.Request (Request, RequestUpdate)
import Sentry.RuntimeContext (RuntimeContext, RuntimeContextUpdate)
import Sentry.Scope.IO (withClient, withIsolationScope, withScope)
import Sentry.Scope.Operations
  ( Scope,
    ScopeData (..),
    ScopeType (..),
    addBreadcrumb,
    addBreadcrumbAt,
    addBreadcrumbs,
    addBreadcrumbsAt,
    clearBreadcrumbsAt,
    configureGlobal,
    getCurrentScope,
    getIsolationScope,
    propagateScope,
    readMergedScope,
    readScopeRef,
    resolveClient,
    resolveClientAt,
  )
import Sentry.Scope.Update (ScopeUpdate)
import Sentry.Scope.Update qualified as ScopeUpdate
import Sentry.TraceContext qualified
import Sentry.Transport (ShutdownResponse (..))
import Sentry.Update (Update)
import Sentry.User (User, UserUpdate)
import Witch qualified
import Prelude hiding (init)

-- | Apply a single scope builder, a list, or a composed bundle atomically,
-- from left to right. Later assignments win. Builders modify only local
-- metadata; capture merges the context-selected layers.
--
-- Under contention this may evaluate the builder more than once, so it must
-- stay a pure function of its input.
--
-- @
-- Sentry.updateScope scope
--   [ Scope.setUser [User.setId "42", User.setName "Alice"],
--     Scope.setTag "feature" "checkout"
--   ]
-- @
updateScope :: (MonadIO m, Witch.From a ScopeUpdate) => Scope -> a -> m ()
updateScope = ScopeUpdate.apply

ambientUpdate :: (MonadIO m) => m Scope -> ScopeUpdate -> m ()
ambientUpdate target upd = do
  client <- resolveClient
  case client of
    NON_RECORDING_CLIENT -> pure ()
    _ -> target >>= \scope -> ScopeUpdate.apply scope upd

-- | Apply 'ScopeUpdate.setLevel' to the active isolation scope.
setLevel :: (MonadIO m) => Patrol.Level -> m ()
setLevel level = ambientUpdate getIsolationScope (ScopeUpdate.setLevel level)

-- | Apply 'ScopeUpdate.unsetLevel' to the active isolation scope.
unsetLevel :: (MonadIO m) => m ()
unsetLevel = ambientUpdate getIsolationScope ScopeUpdate.unsetLevel

-- | Apply 'ScopeUpdate.setUser' to the active isolation scope.
setUser :: (MonadIO m, Witch.From a UserUpdate) => a -> m ()
setUser u = ambientUpdate getIsolationScope (ScopeUpdate.setUser u)

-- | Apply 'ScopeUpdate.setOptionalUser' to the active isolation scope.
setOptionalUser :: (MonadIO m) => Maybe User -> m ()
setOptionalUser user = ambientUpdate getIsolationScope (ScopeUpdate.setOptionalUser user)

-- | Apply 'ScopeUpdate.unsetUser' to the active isolation scope.
unsetUser :: (MonadIO m) => m ()
unsetUser = ambientUpdate getIsolationScope ScopeUpdate.unsetUser

-- | Apply 'ScopeUpdate.modifyUser' to the active isolation scope.
modifyUser :: (MonadIO m, Witch.From a UserUpdate) => a -> m ()
modifyUser upd = ambientUpdate getIsolationScope (ScopeUpdate.modifyUser upd)

-- | Apply 'ScopeUpdate.modifyExistingUser' to the active isolation scope.
modifyExistingUser :: (MonadIO m, Witch.From a UserUpdate) => a -> m ()
modifyExistingUser upd = ambientUpdate getIsolationScope (ScopeUpdate.modifyExistingUser upd)

-- | Apply 'ScopeUpdate.setTag' to the active isolation scope.
setTag :: (MonadIO m) => Text -> Text -> m ()
setTag k v = ambientUpdate getIsolationScope (ScopeUpdate.setTag k v)

-- | Apply 'ScopeUpdate.removeTag' to the active isolation scope.
removeTag :: (MonadIO m) => Text -> m ()
removeTag k = ambientUpdate getIsolationScope (ScopeUpdate.removeTag k)

-- | Apply 'ScopeUpdate.clearTags' to the active isolation scope.
clearTags :: (MonadIO m) => m ()
clearTags = ambientUpdate getIsolationScope ScopeUpdate.clearTags

-- | Apply 'ScopeUpdate.setExtra' to the active isolation scope.
setExtra :: (MonadIO m) => Text -> Aeson.Value -> m ()
setExtra k v = ambientUpdate getIsolationScope (ScopeUpdate.setExtra k v)

-- | Apply 'ScopeUpdate.removeExtra' to the active isolation scope.
removeExtra :: (MonadIO m) => Text -> m ()
removeExtra k = ambientUpdate getIsolationScope (ScopeUpdate.removeExtra k)

-- | Apply 'ScopeUpdate.clearExtras' to the active isolation scope.
clearExtras :: (MonadIO m) => m ()
clearExtras = ambientUpdate getIsolationScope ScopeUpdate.clearExtras

-- | Apply 'ScopeUpdate.setContext' to the active isolation scope.
setContext :: (MonadIO m) => Text -> Patrol.Context -> m ()
setContext k v = ambientUpdate getIsolationScope (ScopeUpdate.setContext k v)

-- | Apply 'ScopeUpdate.setContextValues' to the active isolation scope.
setContextValues :: (MonadIO m) => Text -> [(Text, Aeson.Value)] -> m ()
setContextValues k kvs = ambientUpdate getIsolationScope (ScopeUpdate.setContextValues k kvs)

-- | Apply 'ScopeUpdate.setContextValue' to the active isolation scope.
setContextValue :: (MonadIO m) => Text -> Text -> Aeson.Value -> m ()
setContextValue k key value = ambientUpdate getIsolationScope (ScopeUpdate.setContextValue k key value)

-- | Apply 'ScopeUpdate.removeContextValue' to the active isolation scope.
removeContextValue :: (MonadIO m) => Text -> Text -> m ()
removeContextValue k key = ambientUpdate getIsolationScope (ScopeUpdate.removeContextValue k key)

-- | Apply 'ScopeUpdate.modifyContextValues' to the active isolation scope.
modifyContextValues :: (MonadIO m) => Text -> (Map Text Aeson.Value -> Map Text Aeson.Value) -> m ()
modifyContextValues k f = ambientUpdate getIsolationScope (ScopeUpdate.modifyContextValues k f)

-- | Apply 'ScopeUpdate.setOsContext' to the active isolation scope.
setOsContext :: (MonadIO m, Witch.From a OsContextUpdate) => a -> m ()
setOsContext upd = ambientUpdate getIsolationScope (ScopeUpdate.setOsContext upd)

-- | Apply 'ScopeUpdate.setAppContext' to the active isolation scope.
setAppContext :: (MonadIO m, Witch.From a AppContextUpdate) => a -> m ()
setAppContext upd = ambientUpdate getIsolationScope (ScopeUpdate.setAppContext upd)

-- | Apply 'ScopeUpdate.setRuntimeContext' to the active isolation scope.
setRuntimeContext :: (MonadIO m, Witch.From a RuntimeContextUpdate) => a -> m ()
setRuntimeContext rc = ambientUpdate getIsolationScope (ScopeUpdate.setRuntimeContext rc)

-- | Apply 'ScopeUpdate.removeContext' to the active isolation scope.
removeContext :: (MonadIO m) => Text -> m ()
removeContext k = ambientUpdate getIsolationScope (ScopeUpdate.removeContext k)

-- | Apply 'ScopeUpdate.clearContexts' to the active isolation scope.
clearContexts :: (MonadIO m) => m ()
clearContexts = ambientUpdate getIsolationScope ScopeUpdate.clearContexts

-- | Apply 'ScopeUpdate.setFingerprint' to the active isolation scope.
setFingerprint :: (MonadIO m) => [Text] -> m ()
setFingerprint fp = ambientUpdate getIsolationScope (ScopeUpdate.setFingerprint fp)

-- | Apply 'ScopeUpdate.unsetFingerprint' to the active isolation scope.
unsetFingerprint :: (MonadIO m) => m ()
unsetFingerprint = ambientUpdate getIsolationScope ScopeUpdate.unsetFingerprint

-- | Apply 'ScopeUpdate.setTransaction' to the active isolation scope.
setTransaction :: (MonadIO m) => Text -> m ()
setTransaction t = ambientUpdate getCurrentScope (ScopeUpdate.setTransaction t)

-- | Apply 'ScopeUpdate.unsetTransaction' to the active isolation scope.
unsetTransaction :: (MonadIO m) => m ()
unsetTransaction = ambientUpdate getCurrentScope ScopeUpdate.unsetTransaction

-- | Apply 'ScopeUpdate.clearBreadcrumbs' to the active isolation scope.
clearBreadcrumbs :: (MonadIO m) => m ()
clearBreadcrumbs = ambientUpdate getIsolationScope ScopeUpdate.clearBreadcrumbs

-- | Apply 'ScopeUpdate.setOptionalLevel' to the active isolation scope.
setOptionalLevel :: (MonadIO m) => Maybe Patrol.Level -> m ()
setOptionalLevel value = ambientUpdate getIsolationScope (ScopeUpdate.setOptionalLevel value)

-- | Apply 'ScopeUpdate.setOptionalFingerprint' to the active isolation scope.
setOptionalFingerprint :: (MonadIO m) => Maybe [Text] -> m ()
setOptionalFingerprint value = ambientUpdate getIsolationScope (ScopeUpdate.setOptionalFingerprint value)

-- | Apply 'ScopeUpdate.setOptionalTransaction' to the active isolation scope.
setOptionalTransaction :: (MonadIO m) => Maybe Text -> m ()
setOptionalTransaction value = ambientUpdate getCurrentScope (ScopeUpdate.setOptionalTransaction value)

-- | Apply 'ScopeUpdate.setOptionalTag' to the active isolation scope.
setOptionalTag :: (MonadIO m) => Text -> Maybe Text -> m ()
setOptionalTag key value = ambientUpdate getIsolationScope (ScopeUpdate.setOptionalTag key value)

-- | Apply 'ScopeUpdate.setOptionalExtra' to the active isolation scope.
setOptionalExtra :: (MonadIO m) => Text -> Maybe Aeson.Value -> m ()
setOptionalExtra key value = ambientUpdate getIsolationScope (ScopeUpdate.setOptionalExtra key value)

-- | Apply 'ScopeUpdate.setOptionalContext' to the active isolation scope.
setOptionalContext :: (MonadIO m) => Text -> Maybe Patrol.Context -> m ()
setOptionalContext key value = ambientUpdate getIsolationScope (ScopeUpdate.setOptionalContext key value)

-- | Apply 'ScopeUpdate.setOptionalRuntimeContext' to the active isolation scope.
setOptionalRuntimeContext :: (MonadIO m) => Maybe RuntimeContext -> m ()
setOptionalRuntimeContext value = ambientUpdate getIsolationScope (ScopeUpdate.setOptionalRuntimeContext value)

-- | Apply 'ScopeUpdate.setOptionalContextValues' to the active isolation scope.
setOptionalContextValues :: (MonadIO m) => Text -> Maybe [(Text, Aeson.Value)] -> m ()
setOptionalContextValues key value = ambientUpdate getIsolationScope (ScopeUpdate.setOptionalContextValues key value)

-- | Apply 'ScopeUpdate.setOptionalContextValue' to the active isolation scope.
setOptionalContextValue :: (MonadIO m) => Text -> Text -> Maybe Aeson.Value -> m ()
setOptionalContextValue key fieldKey value = ambientUpdate getIsolationScope (ScopeUpdate.setOptionalContextValue key fieldKey value)

-- | Apply 'ScopeUpdate.setOptionalOsContext' to the active isolation scope.
setOptionalOsContext :: (MonadIO m) => Maybe Sentry.OsContext.OsContext -> m ()
setOptionalOsContext value = ambientUpdate getIsolationScope (ScopeUpdate.setOptionalOsContext value)

-- | Apply 'ScopeUpdate.setOptionalAppContext' to the active isolation scope.
setOptionalAppContext :: (MonadIO m) => Maybe Sentry.AppContext.AppContext -> m ()
setOptionalAppContext value = ambientUpdate getIsolationScope (ScopeUpdate.setOptionalAppContext value)

-- | Apply 'ScopeUpdate.setOptionalBrowserContext' to the active isolation scope.
setOptionalBrowserContext :: (MonadIO m) => Maybe Sentry.BrowserContext.BrowserContext -> m ()
setOptionalBrowserContext value = ambientUpdate getIsolationScope (ScopeUpdate.setOptionalBrowserContext value)

-- | Apply 'ScopeUpdate.setOptionalDeviceContext' to the active isolation scope.
setOptionalDeviceContext :: (MonadIO m) => Maybe Sentry.DeviceContext.DeviceContext -> m ()
setOptionalDeviceContext value = ambientUpdate getIsolationScope (ScopeUpdate.setOptionalDeviceContext value)

-- | Apply 'ScopeUpdate.setOptionalTraceContext' to the active isolation scope.
setOptionalTraceContext :: (MonadIO m) => Maybe Sentry.TraceContext.TraceContext -> m ()
setOptionalTraceContext value = ambientUpdate getIsolationScope (ScopeUpdate.setOptionalTraceContext value)

-- | Apply 'ScopeUpdate.alterAppContext' to the active isolation scope.
alterAppContext :: (MonadIO m) => (Maybe Sentry.AppContext.AppContext -> Maybe Sentry.AppContext.AppContext) -> m ()
alterAppContext f = ambientUpdate getIsolationScope (ScopeUpdate.alterAppContext f)

-- | Apply 'ScopeUpdate.alterOsContext' to the active isolation scope.
alterOsContext :: (MonadIO m) => (Maybe Sentry.OsContext.OsContext -> Maybe Sentry.OsContext.OsContext) -> m ()
alterOsContext f = ambientUpdate getIsolationScope (ScopeUpdate.alterOsContext f)

-- | Apply 'ScopeUpdate.alterRuntimeContext' to the active isolation scope.
alterRuntimeContext :: (MonadIO m) => (Maybe Sentry.RuntimeContext.RuntimeContext -> Maybe Sentry.RuntimeContext.RuntimeContext) -> m ()
alterRuntimeContext f = ambientUpdate getIsolationScope (ScopeUpdate.alterRuntimeContext f)

-- | Apply 'ScopeUpdate.alterBrowserContext' to the active isolation scope.
alterBrowserContext :: (MonadIO m) => (Maybe Sentry.BrowserContext.BrowserContext -> Maybe Sentry.BrowserContext.BrowserContext) -> m ()
alterBrowserContext f = ambientUpdate getIsolationScope (ScopeUpdate.alterBrowserContext f)

-- | Apply 'ScopeUpdate.alterDeviceContext' to the active isolation scope.
alterDeviceContext :: (MonadIO m) => (Maybe Sentry.DeviceContext.DeviceContext -> Maybe Sentry.DeviceContext.DeviceContext) -> m ()
alterDeviceContext f = ambientUpdate getIsolationScope (ScopeUpdate.alterDeviceContext f)

-- | Apply 'ScopeUpdate.alterTraceContext' to the active isolation scope.
alterTraceContext :: (MonadIO m) => (Maybe Sentry.TraceContext.TraceContext -> Maybe Sentry.TraceContext.TraceContext) -> m ()
alterTraceContext f = ambientUpdate getIsolationScope (ScopeUpdate.alterTraceContext f)

-- | Apply 'ScopeUpdate.modifyExistingContextValue' to the active isolation scope.
modifyExistingContextValue :: (MonadIO m) => Text -> Text -> (Aeson.Value -> Aeson.Value) -> m ()
modifyExistingContextValue key field f = ambientUpdate getIsolationScope (ScopeUpdate.modifyExistingContextValue key field f)

-- | Apply 'ScopeUpdate.alterContextValue' to the active isolation scope.
alterContextValue :: (MonadIO m) => Text -> Text -> (Maybe Aeson.Value -> Maybe Aeson.Value) -> m ()
alterContextValue key field f = ambientUpdate getIsolationScope (ScopeUpdate.alterContextValue key field f)

-- | Apply 'ScopeUpdate.filterFingerprint' to the active isolation scope.
filterFingerprint :: (MonadIO m) => (Text -> Bool) -> m ()
filterFingerprint predicate = ambientUpdate getIsolationScope (ScopeUpdate.filterFingerprint predicate)

-- | Apply 'ScopeUpdate.filterBreadcrumbs' to the active isolation scope.
filterBreadcrumbs :: (MonadIO m) => (Patrol.Breadcrumb -> Bool) -> m ()
filterBreadcrumbs predicate = ambientUpdate getIsolationScope (ScopeUpdate.filterBreadcrumbs predicate)
