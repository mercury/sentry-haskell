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
    propagateScope,

    -- * Global scope
    getGlobal,
    configureGlobal,

    -- * Capture-time application
    applyToEvent,

    -- * Pure local metadata edits
    ScopeUpdate,

    -- ** Level
    setLevel,
    setOptionalLevel,
    unsetLevel,

    -- ** User
    setUser,
    setOptionalUser,
    unsetUser,
    modifyUser,
    modifyExistingUser,

    -- ** Fingerprint
    setFingerprint,
    setOptionalFingerprint,
    unsetFingerprint,
    clearFingerprint,
    defaultFingerprintComponent,
    prependFingerprintComponent,
    appendFingerprintComponent,
    ensureDefaultFingerprint,
    removeDefaultFingerprint,
    modifyFingerprint,
    modifyExistingFingerprint,
    findFingerprintComponent,
    filterFingerprint,

    -- ** Transaction
    setTransaction,
    setOptionalTransaction,
    unsetTransaction,

    -- ** Tags
    setTag,
    setOptionalTag,
    setTagIfAbsent,
    removeTag,
    clearTags,
    lookupTag,

    -- ** Extras
    setExtra,
    setOptionalExtra,
    removeExtra,
    clearExtras,
    lookupExtra,

    -- ** Contexts

    -- *** Generic contexts
    setContext,
    setOptionalContext,
    setContextIfAbsent,
    removeContext,
    clearContexts,
    lookupContext,

    -- *** Custom context values
    setContextValues,
    setOptionalContextValues,
    setContextValue,
    setOptionalContextValue,
    removeContextValue,
    modifyContextValues,
    lookupContextValue,
    modifyExistingContextValue,
    alterContextValue,

    -- *** OS context
    setOsContext,
    setOptionalOsContext,
    modifyOsContext,
    modifyExistingOsContext,
    lookupOsContext,
    alterOsContext,

    -- *** App context
    setAppContext,
    setOptionalAppContext,
    modifyAppContext,
    modifyExistingAppContext,
    lookupAppContext,
    alterAppContext,

    -- *** Runtime context
    setRuntimeContext,
    setOptionalRuntimeContext,
    modifyRuntimeContext,
    modifyExistingRuntimeContext,
    lookupRuntimeContext,
    alterRuntimeContext,

    -- *** Browser context
    setBrowserContext,
    setOptionalBrowserContext,
    modifyBrowserContext,
    modifyExistingBrowserContext,
    lookupBrowserContext,
    alterBrowserContext,

    -- *** Device context
    setDeviceContext,
    setOptionalDeviceContext,
    modifyDeviceContext,
    modifyExistingDeviceContext,
    lookupDeviceContext,
    alterDeviceContext,

    -- *** Trace context
    setTraceContext,
    setOptionalTraceContext,
    modifyTraceContext,
    modifyExistingTraceContext,
    lookupTraceContext,
    alterTraceContext,

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
    findBreadcrumb,
    filterBreadcrumbs,

    -- ** Event processors
    setEventProcessor,
    addEventProcessor,
    unsetEventProcessor,

    -- ** Inspection
    with,
  )
where

import Sentry.Scope.Operations (Scope, ScopeData (..), ScopeType (..), applyToEvent, bindClient, clone, configureGlobal, create, getCurrentScope, getGlobal, getIsolationScope, insertCurrent, insertIsolation, lookupClient, lookupClientAt, lookupCurrent, lookupIsolation, propagateScope, readMergedScope, readScopeAt, readScopeRef, removeCurrent, removeIsolation, resolveClient, resolveClientAt)
import Sentry.Scope.Update hiding (apply)
