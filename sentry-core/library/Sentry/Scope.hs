-- | Pure builders for local scope metadata.
--
-- Apply a single builder, a list, or a composed bundle atomically with
-- 'Sentry.updateScope'.
--
-- Breadcrumb builders are low-level: they do not default timestamps, filter,
-- or enforce retention; prefer 'Sentry.addBreadcrumb'.
--
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
    getIsolationScope,
    getCurrentScope,
    readScopeRef,
    readMergedScope,
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
    setOptionalUser,
    unsetUser,
    modifyUser,
    modifyExistingUser,
    defaultFingerprintComponent,
    prependFingerprintComponent,
    ensureDefaultFingerprint,
    removeDefaultFingerprint,
    modifyFingerprint,
    setFingerprint,
    unsetFingerprint,
    appendFingerprintComponent,
    clearFingerprint,
    modifyExistingFingerprint,
    setTransaction,
    unsetTransaction,

    -- ** Tags
    setTag,
    setTagIfAbsent,
    removeTag,
    clearTags,

    -- ** Extras
    setExtra,
    removeExtra,
    clearExtras,

    -- ** Contexts
    setContext,
    setContextIfAbsent,
    setBrowserContext,
    modifyBrowserContext,
    modifyExistingBrowserContext,
    setDeviceContext,
    modifyDeviceContext,
    modifyExistingDeviceContext,
    setTraceContext,
    modifyTraceContext,
    modifyExistingTraceContext,
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
    appendBreadcrumb,
    appendBreadcrumbs,
    prependBreadcrumb,
    firstBreadcrumb,
    lastBreadcrumb,
    eachBreadcrumb,
    setBreadcrumbs,
    modifyBreadcrumbs,
    clearBreadcrumbs,
    trimBreadcrumbs,

    -- ** Event processors
    setEventProcessor,
    addEventProcessor,
    unsetEventProcessor,
  )
where

import Sentry.Scope.Operations (Scope, ScopeData (..), ScopeType (..), applyToEvent, bindClient, clone, configureGlobal, create, getCurrentScope, getGlobal, getIsolationScope, insertCurrent, insertIsolation, lookupClient, lookupClientAt, lookupCurrent, lookupIsolation, readMergedScope, readScopeAt, readScopeRef, removeCurrent, removeIsolation, resolveBreadcrumbScope, resolveClient, resolveClientAt, resolveMutationScope)
import Sentry.Scope.Update hiding (apply)
