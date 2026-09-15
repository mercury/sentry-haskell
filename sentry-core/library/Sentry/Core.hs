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
    clearBreadcrumbs,
    clearBreadcrumbsAt,
    clearContexts,
    clearExtras,
    clearTags,
    configureGlobal,
    modifyContextValues,
    modifyExistingUser,
    modifyUser,
    removeContext,
    removeContextValue,
    removeExtra,
    removeTag,
    resolveClient,
    resolveClientAt,
    setAppContext,
    setContext,
    setContextValue,
    setContextValues,
    setExtra,
    setFingerprint,
    setLevel,
    setOsContext,
    setRuntimeContext,
    setTag,
    setTransaction,
    setUser,
    unsetFingerprint,
    unsetLevel,
    unsetTransaction,
    unsetUser,
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
-- Establish a user with 'setUser', built from the field builders in
-- "Sentry.User", and refine it afterwards with 'modifyUser':
--
-- @
-- import Sentry qualified
-- import Sentry.User qualified
--
-- Sentry.setUser scope [Sentry.User.setId \"42\", Sentry.User.setName \"Alice\"]
-- Sentry.modifyUser scope (Sentry.User.setEmail \"alice\@example.com\")
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
