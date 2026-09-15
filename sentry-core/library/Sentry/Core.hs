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
    -- $scope-metadata
    updateScope,
    setLevel,
    unsetLevel,
    setUser,
    setOptionalUser,
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
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson qualified as Aeson
import Data.Map.Strict (Map)
import Data.Text (Text)
import Patrol qualified
import Sentry.AppContext (AppContext, AppContextUpdate)
import Sentry.Breadcrumb (Breadcrumb, BreadcrumbType, BreadcrumbUpdate)
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
    readMergedScope,
    readScopeRef,
    resolveClient,
    resolveClientAt,
  )
import Sentry.Scope.Update (ScopeUpdate)
import Sentry.Scope.Update qualified as ScopeUpdate
import Sentry.Transport (ShutdownResponse (..))
import Sentry.Update (Update)
import Sentry.User (User, UserUpdate)
import Witch qualified
import Prelude hiding (init)

-- $scope-metadata
--
-- 'setUser' and 'setTag' select the isolation scope without a scope handle.
-- These automatic operations are called ambient operations. Transaction naming
-- selects the current scope instead.
--
-- Without a recording client, ambient operations leave metadata unchanged.
-- Explicit getters and 'updateScope' remain usable before initialization.
--
-- Establish a user with 'setUser', built from the field builders in
-- "Sentry.User", and refine it afterwards with 'modifyUser':
--
-- @
-- import Sentry qualified
-- import Sentry.User qualified
--
-- Sentry.setUser [Sentry.User.setId \"42\", Sentry.User.setName \"Alice\"]
-- Sentry.modifyUser (Sentry.User.setEmail \"alice\@example.com\")
-- @
--
-- Every one of these verbs accepts a single update, a list of them, or a whole
-- record; updates also compose with '<>', applying left to right, so later
-- assignments win.
--
-- Each scope operation applies one atomic update. 'modifyUser' starts from empty
-- when the scope has no user of its own, and never reaches through to a user
-- inherited from another scope.

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

-- Ambient routing checks the client before evaluating the update or acquiring state.
ambientUpdate :: (MonadIO m) => m Scope -> ScopeUpdate -> m ()
ambientUpdate target upd = do
  client <- resolveClient
  case client of
    NON_RECORDING_CLIENT -> pure ()
    _ -> target >>= \scope -> ScopeUpdate.apply scope upd

-- | This operation sets the isolation scope's level.
--
-- It leaves the scope unchanged when there is no recording client.
setLevel :: (MonadIO m) => Patrol.Level -> m ()
setLevel level = ambientUpdate getIsolationScope (ScopeUpdate.setLevel level)

-- | This operation removes the isolation scope's level assignment.
--
-- It leaves the scope unchanged when there is no recording client.
unsetLevel :: (MonadIO m) => m ()
unsetLevel = ambientUpdate getIsolationScope ScopeUpdate.unsetLevel

-- | This operation replaces the isolation scope's user with a supplied record
-- or a user built from empty.
--
-- It leaves the scope unchanged when there is no recording client.
setUser :: (MonadIO m, Witch.From a UserUpdate) => a -> m ()
setUser u = ambientUpdate getIsolationScope (ScopeUpdate.setUser u)

-- | This operation replaces the isolation scope's user with a supplied record,
-- or removes the local assignment when given 'Nothing'.
--
-- It leaves the scope unchanged when there is no recording client.
setOptionalUser :: (MonadIO m) => Maybe User -> m ()
setOptionalUser user = ambientUpdate getIsolationScope (ScopeUpdate.setOptionalUser user)

-- | This operation removes the isolation scope's local user, allowing a global
-- user to appear in captures.
--
-- It leaves the scope unchanged when there is no recording client.
unsetUser :: (MonadIO m) => m ()
unsetUser = ambientUpdate getIsolationScope ScopeUpdate.unsetUser

-- | This operation modifies the isolation scope's user, starting from an empty
-- user when none exists locally.
--
-- It leaves the scope unchanged when there is no recording client.
modifyUser :: (MonadIO m, Witch.From a UserUpdate) => a -> m ()
modifyUser upd = ambientUpdate getIsolationScope (ScopeUpdate.modifyUser upd)

-- | This operation modifies the isolation scope's user only when one exists
-- locally.
--
-- It leaves the scope unchanged when there is no recording client.
modifyExistingUser :: (MonadIO m, Witch.From a UserUpdate) => a -> m ()
modifyExistingUser upd = ambientUpdate getIsolationScope (ScopeUpdate.modifyExistingUser upd)

-- | This operation sets a tag on the isolation scope, replacing any local
-- value at that key.
--
-- It leaves the scope unchanged when there is no recording client.
setTag :: (MonadIO m) => Text -> Text -> m ()
setTag k v = ambientUpdate getIsolationScope (ScopeUpdate.setTag k v)

-- | This operation removes a tag from the isolation scope.
--
-- It leaves the scope unchanged when there is no recording client.
removeTag :: (MonadIO m) => Text -> m ()
removeTag k = ambientUpdate getIsolationScope (ScopeUpdate.removeTag k)

-- | This operation removes all tags from the isolation scope.
--
-- It leaves the scope unchanged when there is no recording client.
clearTags :: (MonadIO m) => m ()
clearTags = ambientUpdate getIsolationScope ScopeUpdate.clearTags

-- | This operation sets an extra value on the isolation scope, replacing any
-- local value at that key.
--
-- It leaves the scope unchanged when there is no recording client.
setExtra :: (MonadIO m) => Text -> Aeson.Value -> m ()
setExtra k v = ambientUpdate getIsolationScope (ScopeUpdate.setExtra k v)

-- | This operation removes an extra value from the isolation scope.
--
-- It leaves the scope unchanged when there is no recording client.
removeExtra :: (MonadIO m) => Text -> m ()
removeExtra k = ambientUpdate getIsolationScope (ScopeUpdate.removeExtra k)

-- | This operation removes all extra values from the isolation scope.
--
-- It leaves the scope unchanged when there is no recording client.
clearExtras :: (MonadIO m) => m ()
clearExtras = ambientUpdate getIsolationScope ScopeUpdate.clearExtras

-- | This operation replaces the entire context payload at the given name on
-- the isolation scope.
--
-- It leaves the scope unchanged when there is no recording client.
setContext :: (MonadIO m) => Text -> Patrol.Context -> m ()
setContext k v = ambientUpdate getIsolationScope (ScopeUpdate.setContext k v)

-- | This operation replaces the named context on the isolation scope with a
-- custom payload built from the supplied fields.
--
-- It leaves the scope unchanged when there is no recording client.
setContextValues :: (MonadIO m) => Text -> [(Text, Aeson.Value)] -> m ()
setContextValues k kvs = ambientUpdate getIsolationScope (ScopeUpdate.setContextValues k kvs)

-- | This operation sets a field in a custom context on the isolation scope,
-- creating the context when absent. Typed contexts remain unchanged.
--
-- It leaves the scope unchanged when there is no recording client.
setContextValue :: (MonadIO m) => Text -> Text -> Aeson.Value -> m ()
setContextValue k key value = ambientUpdate getIsolationScope (ScopeUpdate.setContextValue k key value)

-- | This operation removes a field from a local custom context on the
-- isolation scope. Absent and typed contexts remain unchanged; removing the
-- last field retains an empty context.
--
-- It leaves the scope unchanged when there is no recording client.
removeContextValue :: (MonadIO m) => Text -> Text -> m ()
removeContextValue k key = ambientUpdate getIsolationScope (ScopeUpdate.removeContextValue k key)

-- | This operation transforms a custom context's fields on the isolation
-- scope, starting from an empty map when absent. Typed contexts remain
-- unchanged.
--
-- It leaves the scope unchanged when there is no recording client.
modifyContextValues :: (MonadIO m) => Text -> (Map Text Aeson.Value -> Map Text Aeson.Value) -> m ()
modifyContextValues k f = ambientUpdate getIsolationScope (ScopeUpdate.modifyContextValues k f)

-- | This operation replaces the isolation scope's entire os context with an
-- operating system payload.
--
-- It leaves the scope unchanged when there is no recording client.
setOsContext :: (MonadIO m, Witch.From a OsContextUpdate) => a -> m ()
setOsContext upd = ambientUpdate getIsolationScope (ScopeUpdate.setOsContext upd)

-- | This operation replaces the isolation scope's entire app context with an
-- application payload.
--
-- It leaves the scope unchanged when there is no recording client.
setAppContext :: (MonadIO m, Witch.From a AppContextUpdate) => a -> m ()
setAppContext upd = ambientUpdate getIsolationScope (ScopeUpdate.setAppContext upd)

-- | This operation replaces the isolation scope's entire runtime context with
-- a runtime payload.
--
-- It leaves the scope unchanged when there is no recording client.
setRuntimeContext :: (MonadIO m, Witch.From a RuntimeContextUpdate) => a -> m ()
setRuntimeContext rc = ambientUpdate getIsolationScope (ScopeUpdate.setRuntimeContext rc)

-- | This operation removes the named context from the isolation scope.
--
-- It leaves the scope unchanged when there is no recording client.
removeContext :: (MonadIO m) => Text -> m ()
removeContext k = ambientUpdate getIsolationScope (ScopeUpdate.removeContext k)

-- | This operation removes all contexts from the isolation scope.
--
-- It leaves the scope unchanged when there is no recording client.
clearContexts :: (MonadIO m) => m ()
clearContexts = ambientUpdate getIsolationScope ScopeUpdate.clearContexts

-- | This operation replaces the isolation scope's fingerprint, including when
-- the supplied list is empty.
--
-- It leaves the scope unchanged when there is no recording client.
setFingerprint :: (MonadIO m) => [Text] -> m ()
setFingerprint fp = ambientUpdate getIsolationScope (ScopeUpdate.setFingerprint fp)

-- | This operation removes the isolation scope's fingerprint assignment.
--
-- It leaves the scope unchanged when there is no recording client.
unsetFingerprint :: (MonadIO m) => m ()
unsetFingerprint = ambientUpdate getIsolationScope ScopeUpdate.unsetFingerprint

-- | This operation sets the transaction name on the current scope.
--
-- It leaves the scope unchanged when there is no recording client.
setTransaction :: (MonadIO m) => Text -> m ()
setTransaction t = ambientUpdate getCurrentScope (ScopeUpdate.setTransaction t)

-- | This operation removes the current scope's transaction name assignment.
--
-- It leaves the scope unchanged when there is no recording client.
unsetTransaction :: (MonadIO m) => m ()
unsetTransaction = ambientUpdate getCurrentScope ScopeUpdate.unsetTransaction

-- | This operation removes all breadcrumbs from the isolation scope.
--
-- It leaves the scope unchanged when there is no recording client.
clearBreadcrumbs :: (MonadIO m) => m ()
clearBreadcrumbs = ambientUpdate getIsolationScope ScopeUpdate.clearBreadcrumbs
