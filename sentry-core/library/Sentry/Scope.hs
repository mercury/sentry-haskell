-- | Pure builders for local scope metadata. Apply a single builder, a list,
-- or a composed bundle atomically with 'Sentry.updateScope'. Layers are merged
-- at capture time; removing a local override may reveal inherited metadata.
--
-- Breadcrumb builders are low-level: they do not default timestamps, filter,
-- or enforce retention. Use 'Sentry.addBreadcrumb' for those policies.
-- Explicit effectful setters and context-first operations live in
-- "Sentry.Scope.Operations".
module Sentry.Scope
  ( -- * Scope handles and snapshots
    Scope,
    ScopeType (..),
    ScopeData (..),

    -- * Construction and reading
    create,
    clone,
    readScopeRef,
    readAmbientScope,
    readScopeAt,

    -- * Client binding and resolution
    resolveClient,
    resolveClientAt,
    lookupClient,
    lookupClientAt,
    bindClient,

    -- * Context selection
    lookupCurrent,
    insertCurrent,
    removeCurrent,
    lookupIsolation,
    insertIsolation,
    removeIsolation,
    resolveMutationScope,
    resolveBreadcrumbScope,

    -- * Global scope
    getGlobal,
    configureGlobal,

    -- * Capture-time application
    applyToEvent,

    -- * Pure local metadata edits
    ScopeUpdate,
    setLevel,
    unsetLevel,
    setUser,
    unsetUser,
    modifyUser,
    modifyExistingUser,
    setFingerprint,
    unsetFingerprint,
    setTransaction,
    unsetTransaction,

    -- ** Tags
    setTag,
    removeTag,
    clearTags,

    -- ** Extras
    setExtra,
    removeExtra,
    clearExtras,

    -- ** Contexts
    setContext,
    setOsContext,
    modifyOsContext,
    modifyExistingOsContext,
    setAppContext,
    modifyAppContext,
    modifyExistingAppContext,
    setRuntimeContext,
    modifyRuntimeContext,
    modifyExistingRuntimeContext,
    setContextValues,
    setContextValue,
    removeContextValue,
    modifyContextValues,
    removeContext,
    clearContexts,

    -- ** Breadcrumbs (without policy)
    addBreadcrumb,
    addBreadcrumbs,
    clearBreadcrumbs,
    trimBreadcrumbs,

    -- ** Event processors
    setEventProcessor,
    addEventProcessor,
    unsetEventProcessor,
  )
where

import Sentry.Scope.Operations (Scope, ScopeData (..), ScopeType (..), applyToEvent, bindClient, clone, configureGlobal, create, getGlobal, insertCurrent, insertIsolation, lookupClient, lookupClientAt, lookupCurrent, lookupIsolation, readAmbientScope, readScopeAt, readScopeRef, removeCurrent, removeIsolation, resolveBreadcrumbScope, resolveClient, resolveClientAt, resolveMutationScope)
import Sentry.Scope.Update hiding (apply)
