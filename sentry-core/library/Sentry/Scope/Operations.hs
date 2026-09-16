{-# LANGUAGE ViewPatterns #-}

-- | Thread-local scope metadata management for tags, breadcrumbs, and user
-- information attached to captured events.
--
-- Sentry uses a three-layer scope model:
--
-- * __Global__ — process-wide metadata applied to every event ('global').
-- * __Isolation__ — per-request or per-task metadata (e.g. one per HTTP request).
-- * __Current__ — narrowly-scoped metadata within a single operation.
--
-- When an exception is captured, these layers are merged (global \< isolation
-- \< current) and attached as an annotation. Later values override earlier ones
-- for scalar fields; collection fields (breadcrumbs, tags, extras, contexts)
-- are combined.
--
-- Metadata setters such as 'setTag' take an explicit 'Scope'. Breadcrumb
-- append operations such as 'addBreadcrumb' use the ambient isolation scope.
-- Their @*At@ variants, such as 'setTagAt' and 'addBreadcrumbAt', take an
-- explicit OpenTelemetry 'Context'.
module Sentry.Scope.Operations
  ( -- * Scope

    -- ** Definition
    Scope,
    ScopeType (..),
    ScopeData (..),

    -- ** Construction
    create,
    clone,

    -- ** Access
    getIsolationScope,
    getCurrentScope,
    readScopeRef,
    readMergedScope,
    readScopeAt,

    -- ** Client resolution
    resolveClient,
    resolveClientAt,
    lookupClient,
    lookupClientAt,
    bindClient,

    -- ** Mutation

    -- *** Scalar fields
    setLevel,
    setLevelAt,
    unsetLevel,
    unsetLevelAt,
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
    setUserAt,
    setOptionalUserAt,
    setOptionalTraceContextAt,
    setOptionalDeviceContextAt,
    setOptionalBrowserContextAt,
    setOptionalAppContextAt,
    setOptionalOsContextAt,
    setOptionalContextValueAt,
    setOptionalContextValuesAt,
    setOptionalRuntimeContextAt,
    setOptionalContextAt,
    setOptionalExtraAt,
    setOptionalTagAt,
    setOptionalTransactionAt,
    setOptionalFingerprintAt,
    setOptionalLevelAt,
    unsetUser,
    unsetUserAt,
    modifyUser,
    modifyUserAt,
    modifyExistingUser,
    modifyExistingUserAt,
    setFingerprint,
    setFingerprintAt,
    unsetFingerprint,
    unsetFingerprintAt,
    setTransaction,
    setTransactionAt,
    unsetTransaction,
    unsetTransactionAt,

    -- *** Tags
    setTag,
    setTagAt,
    removeTag,
    removeTagAt,
    clearTags,
    clearTagsAt,

    -- *** Extras
    setExtra,
    setExtraAt,
    removeExtra,
    removeExtraAt,
    clearExtras,
    clearExtrasAt,

    -- *** Contexts
    setContext,
    setContextAt,
    setOsContext,
    setAppContext,
    setRuntimeContext,
    setOsContextAt,
    setAppContextAt,
    setRuntimeContextAt,
    setContextValues,
    setContextValuesAt,
    setContextValue,
    setContextValueAt,
    removeContextValue,
    removeContextValueAt,
    modifyContextValues,
    modifyContextValuesAt,
    removeContext,
    removeContextAt,
    clearContexts,
    clearContextsAt,

    -- *** Breadcrumbs
    addBreadcrumb,
    addBreadcrumbAt,
    addBreadcrumbs,
    addBreadcrumbsAt,
    clearBreadcrumbs,
    clearBreadcrumbsAt,

    -- ** Thread-local Context Manipulation
    lookupCurrent,
    insertCurrent,
    removeCurrent,
    lookupIsolation,
    insertIsolation,
    removeIsolation,

    -- ** Context-first operations
    resolveMutationScope,
    resolveBreadcrumbScope,
    updateAt,

    -- ** Global Scope
    getGlobal,
    configureGlobal,

    -- ** Event Processor
    setEventProcessor,
    setEventProcessorAt,
    addEventProcessor,
    addEventProcessorAt,
    unsetEventProcessor,
    unsetEventProcessorAt,

    -- ** Event Modification
    applyToEvent,
    alterAppContext,
    alterAppContextAt,
    alterOsContext,
    alterOsContextAt,
    alterRuntimeContext,
    alterRuntimeContextAt,
    alterBrowserContext,
    alterBrowserContextAt,
    alterDeviceContext,
    alterDeviceContextAt,
    alterTraceContext,
    alterTraceContextAt,
    modifyExistingContextValue,
    modifyExistingContextValueAt,
    alterContextValue,
    alterContextValueAt,
    filterFingerprint,
    filterFingerprintAt,
    filterBreadcrumbs,
    filterBreadcrumbsAt,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (mask_)
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson qualified as Aeson
import Data.Default (def)
import Data.Foldable (for_, toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import OpenTelemetry.Context (Context, Key)
import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Context.ThreadLocal qualified as ThreadLocal
import Patrol qualified
import Patrol.Type.Breadcrumb qualified as Patrol.Breadcrumb
import Patrol.Type.BreadcrumbType qualified as Patrol.BreadcrumbType
import Patrol.Type.Breadcrumbs qualified as Patrol.Breadcrumbs
import Patrol.Type.Event qualified as Patrol.Event
import Sentry.AppContext (AppContextUpdate)
import Sentry.AppContext qualified
import Sentry.Breadcrumb qualified
import Sentry.BrowserContext qualified
import Sentry.Client (Client, pattern NON_RECORDING_CLIENT)
import Sentry.Client.Options (ClientOptions (..))
import Sentry.DeviceContext qualified
import Sentry.Event.Captured (CapturedEvent (..))
import Sentry.Fingerprint.Internal qualified as Fingerprint
import Sentry.OsContext (OsContextUpdate)
import Sentry.OsContext qualified
import Sentry.RuntimeContext (RuntimeContextUpdate)
import Sentry.RuntimeContext qualified
import Sentry.Scope.Internal (Scope, ScopeData (..), ScopeType (..))
import Sentry.Scope.Internal qualified as Internal
import Sentry.Scope.Update qualified as Update
import Sentry.TraceContext qualified
import Sentry.Update qualified
import Sentry.User (User, UserUpdate)
import System.IO.Unsafe (unsafePerformIO)
import Witch qualified

-- | This operation reads metadata stored locally on the supplied 'Scope'.
-- It does not merge other scope layers.
readScopeRef :: (MonadIO m) => Scope -> m ScopeData
readScopeRef = liftIO . Internal.readScopeData

-- * Scalar setters

--
-- These are thin, immediate wrappers over the composable constructors in
-- "Sentry.Scope.Update"; the mutation logic lives there.

-- | Set the 'Patrol.Type.Level.Level' for the given 'Scope'.
setLevel :: (MonadIO m) => Scope -> Patrol.Level -> m ()
setLevel scope level = Update.apply scope (Update.setLevel level)

-- | Clear the 'Patrol.Type.Level.Level' from the given 'Scope'.
unsetLevel :: (MonadIO m) => Scope -> m ()
unsetLevel scope = Update.apply scope Update.unsetLevel

-- | Establish the user on the given 'Scope'. See 'Update.setUser'.
setUser :: (MonadIO m, Witch.From a UserUpdate) => Scope -> a -> m ()
setUser scope u = Update.apply scope (Update.setUser u)

-- | This operation replaces the supplied scope's local user, or removes the
-- assignment when given 'Nothing'. Removing it allows inherited users to
-- appear in captures.
setOptionalUser :: (MonadIO m) => Scope -> Maybe User -> m ()
setOptionalUser scope user = Update.apply scope (Update.setOptionalUser user)

-- | Clear the 'Patrol.Type.User.User' from the given 'Scope'.
unsetUser :: (MonadIO m) => Scope -> m ()
unsetUser scope = Update.apply scope Update.unsetUser

-- | Update the local user, starting from empty when absent. See
-- 'Update.modifyUser'.
modifyUser :: (MonadIO m, Witch.From a UserUpdate) => Scope -> a -> m ()
modifyUser scope upd = Update.apply scope (Update.modifyUser upd)

-- | Set the fingerprint for the given 'Scope'.
setFingerprint :: (MonadIO m) => Scope -> [Text] -> m ()
setFingerprint scope fp = Update.apply scope (Update.setFingerprint fp)

-- | Clear the fingerprint from the given 'Scope'.
unsetFingerprint :: (MonadIO m) => Scope -> m ()
unsetFingerprint scope = Update.apply scope Update.unsetFingerprint

-- | Set the transaction name for the given 'Scope'.
setTransaction :: (MonadIO m) => Scope -> Text -> m ()
setTransaction scope t = Update.apply scope (Update.setTransaction t)

-- | Clear the transaction name from the given 'Scope'.
unsetTransaction :: (MonadIO m) => Scope -> m ()
unsetTransaction scope = Update.apply scope Update.unsetTransaction

-- * Tag setters

-- | Insert (or overwrite) a tag at the given key.
setTag :: (MonadIO m) => Scope -> Text -> Text -> m ()
setTag scope k v = Update.apply scope (Update.setTag k v)

-- | Remove the tag at the given key, if present.
removeTag :: (MonadIO m) => Scope -> Text -> m ()
removeTag scope k = Update.apply scope (Update.removeTag k)

-- | Clear all tags from the given 'Scope'.
clearTags :: (MonadIO m) => Scope -> m ()
clearTags scope = Update.apply scope Update.clearTags

-- * Extra setters

-- | Insert (or overwrite) an extra value at the given key.
setExtra :: (MonadIO m) => Scope -> Text -> Aeson.Value -> m ()
setExtra scope k v = Update.apply scope (Update.setExtra k v)

-- | Remove the extra value at the given key, if present.
removeExtra :: (MonadIO m) => Scope -> Text -> m ()
removeExtra scope k = Update.apply scope (Update.removeExtra k)

-- | Clear all extras from the given 'Scope'.
clearExtras :: (MonadIO m) => Scope -> m ()
clearExtras scope = Update.apply scope Update.clearExtras

-- * Context setters

-- | Insert (or overwrite) a context at the given key.
setContext :: (MonadIO m) => Scope -> Text -> Patrol.Context -> m ()
setContext scope k v = Update.apply scope (Update.setContext k v)

-- | Replace the context at @\"runtime\"@ on the given 'Scope' with a runtime
-- context. See 'Update.setRuntimeContext'.
setRuntimeContext :: (MonadIO m, Witch.From a RuntimeContextUpdate) => Scope -> a -> m ()
setRuntimeContext scope rc = Update.apply scope (Update.setRuntimeContext rc)

-- | Replace the context at the given key with a custom context built from
-- key/value pairs. Later duplicate keys win. An empty list stores an empty
-- context; use 'removeContext' to remove the entry. See 'Update.setContextValues'.
setContextValues :: (MonadIO m) => Scope -> Text -> [(Text, Aeson.Value)] -> m ()
setContextValues scope k kvs = Update.apply scope (Update.setContextValues k kvs)

-- | Insert (or overwrite) a single value inside the custom context at the
-- given key, leaving its other entries alone. See 'Update.setContextValue'.
setContextValue :: (MonadIO m) => Scope -> Text -> Text -> Aeson.Value -> m ()
setContextValue scope k key value = Update.apply scope (Update.setContextValue k key value)

-- | Remove a single value from the custom context at the given key. See
-- 'Update.removeContextValue'.
removeContextValue :: (MonadIO m) => Scope -> Text -> Text -> m ()
removeContextValue scope k key = Update.apply scope (Update.removeContextValue k key)

-- | Transform the key/value payload of the custom context at the given key.
--
-- Creates the context when this scope has none at that key, and does nothing
-- when the entry is a typed variant. See 'Update.modifyContextValues'.
modifyContextValues :: (MonadIO m) => Scope -> Text -> (Map Text Aeson.Value -> Map Text Aeson.Value) -> m ()
modifyContextValues scope k f = Update.apply scope (Update.modifyContextValues k f)

-- | Remove the context at the given key, if present.
removeContext :: (MonadIO m) => Scope -> Text -> m ()
removeContext scope k = Update.apply scope (Update.removeContext k)

-- | Clear all contexts from the given 'Scope'.
clearContexts :: (MonadIO m) => Scope -> m ()
clearContexts scope = Update.apply scope Update.clearContexts

-- | Resolve the global 'Scope' visible on the calling thread.
--
-- Returns the thread-local override installed by 'Sentry.Test.withGlobalScope'
-- (or, at the lower level, 'Sentry.Scope.Internal.insertGlobal') when one
-- exists, falling back to the true process-wide singleton
-- ('Sentry.Scope.Internal.processGlobal') otherwise.
--
-- In production, where no override should ever installed, this is a single
-- context lookup that returns the process singleton immediately.
getGlobal :: (MonadIO m) => m Scope
getGlobal = liftIO $ fromMaybe Internal.processGlobal . Internal.lookupGlobal <$> ThreadLocal.getContext

-- | Apply a modification to the resolved global 'Scope'.
--
-- Equivalent to @'getGlobal' >>= f@ — fetches the thread-local override
-- (or the process singleton) and passes it to @f@. Useful for writing
-- process-wide defaults at startup:
--
-- @
-- configureGlobal \\scope -> setTag scope "release" version
-- @
configureGlobal :: (MonadIO m) => (Scope -> m a) -> m a
configureGlobal f = getGlobal >>= f

currentScopeKey :: Key Scope
currentScopeKey = unsafePerformIO $ Context.newKey "current_scope"
{-# NOINLINE currentScopeKey #-}

-- | Attempt to retrieve 'Current' scope if one exists on thread-local storage.
lookupCurrent :: Context -> Maybe Scope
lookupCurrent = Context.lookup currentScopeKey

-- | Insert the given 'Current' scope on thread-local storage.
insertCurrent :: Scope -> Context -> Context
insertCurrent = Context.insert currentScopeKey

-- | Remove any active 'Current' scope from thread-local storage.
removeCurrent :: Context -> Context
removeCurrent = Context.delete currentScopeKey

isolationScopeKey :: Key Scope
isolationScopeKey = unsafePerformIO $ Context.newKey "isolation_scope"
{-# NOINLINE isolationScopeKey #-}

-- | Attempt to retrieve 'Isolation' scope if one exists on thread-local storage.
lookupIsolation :: Context -> Maybe Scope
lookupIsolation = Context.lookup isolationScopeKey

-- | Insert the given 'Isolation' scope on thread-local storage.
insertIsolation :: Scope -> Context -> Context
insertIsolation = Context.insert isolationScopeKey

-- | Remove any active 'Isolation' scope from thread-local storage.
removeIsolation :: Context -> Context
removeIsolation = Context.delete isolationScopeKey

-- | Create a fresh 'Scope' of the given 'ScopeType'.
create :: (MonadIO m) => ScopeType -> m Scope
create (Just -> type_) = liftIO $ Internal.newScope (def{type_})

-- | Return an independent copy of the given 'Scope'. Mutations to the clone
-- do not affect the original, and vice versa. The effective client is copied
-- as an unmanaged binding; lifecycle ownership is not copied.
clone :: (MonadIO m) => Scope -> m Scope
clone scope = liftIO $ Internal.readScopeData scope >>= Internal.newScope

-- | Read the merged ambient 'ScopeData' visible at the call site:
-- global (process singleton or thread-local override) \<> isolation (from
-- thread-local) \<> current (from thread-local).
--
-- Missing layers contribute 'mempty'. This operation neither creates scopes nor
-- runs event processors.
--
-- Like 'readScopeAt', but reads the thread-local 'Context' instead of taking
-- one explicitly.
readMergedScope :: (MonadIO m) => m ScopeData
readMergedScope = liftIO ThreadLocal.getContext >>= readScopeAt

-- | Resolve the 'Client' visible at the call site by walking the scope chain
-- (current \> isolation \> global, most-specific wins), falling back to
-- 'NON_RECORDING_CLIENT' when no layer has a client bound.
--
-- This operation reads client bindings directly without constructing merged metadata.
resolveClient :: (MonadIO m) => m Client
resolveClient = liftIO ThreadLocal.getContext >>= resolveClientAt

-- | Like 'resolveClient', but reports the absence of a bound client as
-- 'Nothing' rather than substituting 'NON_RECORDING_CLIENT'.
lookupClient :: (MonadIO m) => m (Maybe Client)
lookupClient = liftIO ThreadLocal.getContext >>= lookupClientAt

-- | Bind (or clear, with 'Nothing') the 'Client' on a specific scope layer.
--
-- This deliberately replaces any managed initialization bindings on the scope;
-- stale handle releases cannot restore over it. It neither acquires nor closes
-- resources. 'Sentry.Scope.IO.withClient' uses it on a fresh isolation scope.
bindClient :: (MonadIO m) => Maybe Client -> Scope -> m ()
bindClient mc scope = liftIO $ Internal.bindUnmanaged scope mc

-- * Event processor setters

-- | Replace the 'Scope's event processor with the given function.
--
-- Return the resulting event, or 'Nothing' to drop it. Returning the input
-- event preserves it.
setEventProcessor :: (MonadIO m) => Scope -> (CapturedEvent -> Maybe Patrol.Event) -> m ()
setEventProcessor scope f = Update.apply scope (Update.setEventProcessor f)

-- | Chain a new processor after the existing one.
--
-- The existing processor runs first; its output is then passed to the new
-- processor. If the existing processor drops the event ('Nothing'), the new
-- processor is not called. Matches the left-to-right chaining of the
-- 'Semigroup' instance.
addEventProcessor :: (MonadIO m) => Scope -> (CapturedEvent -> Maybe Patrol.Event) -> m ()
addEventProcessor scope g = Update.apply scope (Update.addEventProcessor g)

-- | Reset the 'Scope's event processor to the default pass-through (no
-- filtering or mutation).
unsetEventProcessor :: (MonadIO m) => Scope -> m ()
unsetEventProcessor scope = Update.apply scope Update.unsetEventProcessor

-- | This operation applies a 'ScopeData' snapshot to the captured event.
--
-- Metadata is applied to the event using these precedence rules:
--
-- * A user assigned directly to the event wins, including an empty user record.
--   Otherwise, the scope's user is used.
--
-- * A nonempty transaction name on the event wins. An empty name defers to a
--   present scope transaction assignment.
--
-- * A present scope level overrides the event's level.
--
-- * A custom event fingerprint wins. An empty fingerprint or a singleton
--   @{{ default }}@ or @{{default}}@ defers to a present scope fingerprint.
--
-- * Tags, extras, and contexts combine by key, with scope values winning
--   collisions. A named context payload replaces the entire event payload.
--
-- * The event's breadcrumbs precede the scope's breadcrumbs. The field is
--   omitted when both are empty.
--
-- The scope's 'eventProcessor' then receives the 'CapturedEvent' containing
-- the merged event. It may modify that event or return 'Nothing' to drop it.
applyToEvent :: ScopeData -> CapturedEvent -> Maybe Patrol.Event
applyToEvent scope ce = scope.eventProcessor ce{event = merged}
  where
    event = ce.event
    merged =
      event
        { Patrol.Event.level = scope.level <|> event.level,
          Patrol.Event.fingerprint = Fingerprint.merge scope.fingerprint event.fingerprint,
          Patrol.Event.transaction = if event.transaction == "" then fromMaybe event.transaction scope.transaction else event.transaction,
          Patrol.Event.user = event.user <|> scope.user,
          Patrol.Event.tags = Map.union scope.tags event.tags,
          Patrol.Event.extra = Map.union scope.extras event.extra,
          Patrol.Event.contexts = Map.union scope.contexts event.contexts,
          Patrol.Event.breadcrumbs =
            let crumbs =
                  foldMap Patrol.Breadcrumbs.values event.breadcrumbs
                    <> toList scope.breadcrumbs
             in Patrol.Breadcrumbs.Breadcrumbs crumbs <$ guard (not $ null crumbs)
        }

-- * Breadcrumb writers

-- | Add a 'Patrol.Breadcrumb' to the active isolation scope.
--
-- Accepts a 'Sentry.Breadcrumb.BreadcrumbUpdate', a list of them, or a whole
-- 'Sentry.Breadcrumb.Breadcrumb'.
--
-- Defaults 'Patrol.Type.Breadcrumb.timestamp' to 'getCurrentTime' and
-- 'Patrol.Type.Breadcrumb.type_' to 'Patrol.Type.BreadcrumbType.Default' when
-- absent, enforces 'Sentry.Client.Options.ClientOptions.beforeBreadcrumb', and
-- trims the oldest entries to stay within
-- 'Sentry.Client.Options.ClientOptions.maxBreadcrumbs'.
--
-- This operation creates a thread-local isolation scope when a recording client
-- is available. This scope persists on the context, including across
-- 'Sentry.Scope.IO.withScope' exits. Use an isolation bracket at request/task
-- boundaries.
--
-- Without a recording client, it leaves metadata unchanged and skips breadcrumb
-- hooks.
--
-- Unlike 'addBreadcrumbAt', this convenience operation can establish missing
-- isolation state on the thread-local 'Context'.
addBreadcrumb :: (MonadIO m, Witch.From a Sentry.Breadcrumb.BreadcrumbUpdate) => a -> m ()
addBreadcrumb upd = do
  target <- ambientBreadcrumbTarget
  for_ target \(client, scope) ->
    addBreadcrumbToScope client.options scope (Sentry.Update.run upd Sentry.Breadcrumb.empty)

-- | Add multiple 'Patrol.Breadcrumb's to the active isolation scope in order.
--
-- This is equivalent to calling 'addBreadcrumb' on each element; each crumb is
-- independently filtered and trimmed.
--
-- This operation acquires isolation as 'addBreadcrumb' does. An empty batch
-- does not establish state. Explicit 'addBreadcrumbsAt' never establishes it.
addBreadcrumbs :: (MonadIO m) => [Patrol.Breadcrumb] -> m ()
addBreadcrumbs crumbs = do
  client <- resolveClient
  case client of
    NON_RECORDING_CLIENT -> pure ()
    _ -> case crumbs of
      [] -> pure ()
      _ -> do
        scope <- getIsolationScope
        for_ crumbs (addBreadcrumbToScope client.options scope)

-- | This helper acquires an isolation scope only for a recording client.
--
-- Disabled ambient calls never mutate existing scopes.
--
-- This operation can install a scope in the thread-local 'Context'. Explicit
-- context mutations only use scopes already attached to the supplied context;
-- see 'resolveMutationScope'.
ambientBreadcrumbTarget :: (MonadIO m) => m (Maybe (Client, Scope))
ambientBreadcrumbTarget = liftIO $ mask_ do
  context <- ThreadLocal.getContext
  client <- resolveClientAt context
  case client of
    NON_RECORDING_CLIENT -> pure Nothing
    _ -> do
      scope <- getIsolationScope
      pure $ Just (client, scope)

-- | This operation returns the existing isolation scope or creates and
-- installs an empty one when absent.
--
-- It works without a client, preserves other context keys, and does not copy
-- inherited metadata or bind a client.
getIsolationScope :: (MonadIO m) => m Scope
getIsolationScope = getNamedScope Isolation lookupIsolation insertIsolation

-- | This operation returns the existing current scope or creates and installs
-- an empty one when absent.
--
-- It works without a client, preserves other context keys, and does not copy
-- inherited metadata or bind a client.
getCurrentScope :: (MonadIO m) => m Scope
getCurrentScope = getNamedScope Current lookupCurrent insertCurrent

getNamedScope :: (MonadIO m) => ScopeType -> (Context -> Maybe Scope) -> (Scope -> Context -> Context) -> m Scope
getNamedScope kind lookupScope insertScope = liftIO $ mask_ do
  context <- ThreadLocal.getContext
  case lookupScope context of
    Just scope -> pure scope
    Nothing -> do
      scope <- create kind
      ThreadLocal.adjustContext (insertScope scope)
      pure scope

-- | Clear all breadcrumbs from the given 'Scope'.
clearBreadcrumbs :: (MonadIO m) => Scope -> m ()
clearBreadcrumbs scope = Update.apply scope Update.clearBreadcrumbs

-- * Context-first operations

-- $context-first
--
-- These mirror the 'Scope'-first and ambient operations above, but take an
-- explicit 'Context' argument instead of resolving one from thread-local
-- storage.
--
-- They exist for callers who are /handed/ a 'Context', which can happen when
-- writing OpenTelemetry processor code, and need to modify 'Scope' metadata
-- for /that/ context rather than the one attached to the thread they are
-- executing upon.
--
-- Two caveats:
--
-- 1. The 'Context' a hook receives is not guaranteed to be the ambient one.
-- 2. Mutations no-op when their target scope is absent. General metadata
--    targets isolation; transactions require current.

-- | Resolve the scope that general mutations target: 'Isolation' only.
--
-- This function returns 'Nothing' when isolation is absent.
--
-- The supplied 'Context' is immutable; this function never installs a scope.
resolveMutationScope :: Context -> Maybe Scope
resolveMutationScope = lookupIsolation

-- | Resolve the scope that breadcrumbs target: 'Isolation' only.
resolveBreadcrumbScope :: Context -> Maybe Scope
resolveBreadcrumbScope = lookupIsolation

-- | Apply a 'Update.ScopeUpdate' to the scope resolved by
-- 'resolveMutationScope' on the given 'Context'.
updateAt :: (MonadIO m) => Context -> Update.ScopeUpdate -> m ()
updateAt ctx upd = for_ (resolveMutationScope ctx) \scope -> Update.apply scope upd

-- | Like 'readMergedScope', but reads the given 'Context' instead of the
-- thread-local one.
readScopeAt :: (MonadIO m) => Context -> m ScopeData
readScopeAt context = liftIO do
  let globalScopeRef = fromMaybe Internal.processGlobal (Internal.lookupGlobal context)
  globalScope <- readScopeRef globalScopeRef
  isolationScope <- maybe (pure mempty) readScopeRef (lookupIsolation context)
  currentScope <- maybe (pure mempty) readScopeRef (lookupCurrent context)
  pure $ globalScope <> isolationScope <> currentScope

-- | Like 'resolveClient', but reads the given 'Context' instead of the
-- thread-local one.
resolveClientAt :: (MonadIO m) => Context -> m Client
resolveClientAt context = fromMaybe NON_RECORDING_CLIENT <$> lookupClientAt context

-- | Like 'lookupClient', but reads the given 'Context' instead of the
-- thread-local one.
lookupClientAt :: (MonadIO m) => Context -> m (Maybe Client)
lookupClientAt context = go [lookupCurrent context, lookupIsolation context, Just globalScope]
  where
    globalScope = fromMaybe Internal.processGlobal (Internal.lookupGlobal context)
    go [] = pure Nothing
    go (Nothing : rest) = go rest
    go (Just scope : rest) = do
      client <- (.client) <$> readScopeRef scope
      maybe (go rest) (pure . Just) client

-- | This operation sets the isolation scope's level.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
setLevelAt :: (MonadIO m) => Context -> Patrol.Level -> m ()
setLevelAt ctx level = updateAt ctx (Update.setLevel level)

-- | This operation removes the isolation scope's level assignment.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
unsetLevelAt :: (MonadIO m) => Context -> m ()
unsetLevelAt ctx = updateAt ctx Update.unsetLevel

-- | This operation replaces the isolation scope's user with a supplied record
-- or a user built from empty.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
setUserAt :: (MonadIO m, Witch.From a UserUpdate) => Context -> a -> m ()
setUserAt ctx u = updateAt ctx (Update.setUser u)

-- | This operation replaces the isolation scope's local user in the supplied
-- 'Context', or removes the assignment when given 'Nothing'.
--
-- It leaves the context unchanged when the isolation scope is absent.
setOptionalUserAt :: (MonadIO m) => Context -> Maybe User -> m ()
setOptionalUserAt ctx user = updateAt ctx (Update.setOptionalUser user)

-- | This operation removes the isolation scope's local user, allowing a global
-- user to appear in captures.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
unsetUserAt :: (MonadIO m) => Context -> m ()
unsetUserAt ctx = updateAt ctx Update.unsetUser

-- | This operation modifies the isolation scope's user, starting from an empty
-- user when none exists locally.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
modifyUserAt :: (MonadIO m, Witch.From a UserUpdate) => Context -> a -> m ()
modifyUserAt ctx upd = updateAt ctx (Update.modifyUser upd)

-- | This operation replaces the isolation scope's fingerprint, including when
-- the supplied list is empty.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
setFingerprintAt :: (MonadIO m) => Context -> [Text] -> m ()
setFingerprintAt ctx fp = updateAt ctx (Update.setFingerprint fp)

-- | This operation removes the isolation scope's fingerprint assignment.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
unsetFingerprintAt :: (MonadIO m) => Context -> m ()
unsetFingerprintAt ctx = updateAt ctx Update.unsetFingerprint

-- | This operation sets the transaction name on the current scope.
--
-- The target is the current scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
setTransactionAt :: (MonadIO m) => Context -> Text -> m ()
setTransactionAt ctx t = for_ (lookupCurrent ctx) \scope -> setTransaction scope t

-- | This operation removes the current scope's transaction name assignment.
--
-- The target is the current scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
unsetTransactionAt :: (MonadIO m) => Context -> m ()
unsetTransactionAt ctx = for_ (lookupCurrent ctx) unsetTransaction

-- | This operation sets a tag on the isolation scope, replacing any local
-- value at that key.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
setTagAt :: (MonadIO m) => Context -> Text -> Text -> m ()
setTagAt ctx k v = updateAt ctx (Update.setTag k v)

-- | This operation removes a tag from the isolation scope.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
removeTagAt :: (MonadIO m) => Context -> Text -> m ()
removeTagAt ctx k = updateAt ctx (Update.removeTag k)

-- | This operation removes all tags from the isolation scope.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
clearTagsAt :: (MonadIO m) => Context -> m ()
clearTagsAt ctx = updateAt ctx Update.clearTags

-- | This operation sets an extra value on the isolation scope, replacing any
-- local value at that key.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
setExtraAt :: (MonadIO m) => Context -> Text -> Aeson.Value -> m ()
setExtraAt ctx k v = updateAt ctx (Update.setExtra k v)

-- | This operation removes an extra value from the isolation scope.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
removeExtraAt :: (MonadIO m) => Context -> Text -> m ()
removeExtraAt ctx k = updateAt ctx (Update.removeExtra k)

-- | This operation removes all extra values from the isolation scope.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
clearExtrasAt :: (MonadIO m) => Context -> m ()
clearExtrasAt ctx = updateAt ctx Update.clearExtras

-- | This operation replaces the entire context payload at the given name on
-- the isolation scope.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
setContextAt :: (MonadIO m) => Context -> Text -> Patrol.Context -> m ()
setContextAt ctx k v = updateAt ctx (Update.setContext k v)

-- | This operation removes the named context from the isolation scope.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
removeContextAt :: (MonadIO m) => Context -> Text -> m ()
removeContextAt ctx k = updateAt ctx (Update.removeContext k)

-- | This operation removes all contexts from the isolation scope.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
clearContextsAt :: (MonadIO m) => Context -> m ()
clearContextsAt ctx = updateAt ctx Update.clearContexts

-- | This operation replaces the isolation scope's entire runtime context with
-- a runtime payload.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
setRuntimeContextAt :: (MonadIO m, Witch.From a RuntimeContextUpdate) => Context -> a -> m ()
setRuntimeContextAt ctx rc = updateAt ctx (Update.setRuntimeContext rc)

-- | This operation replaces the named context on the isolation scope with a
-- custom payload built from the supplied fields.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
setContextValuesAt :: (MonadIO m) => Context -> Text -> [(Text, Aeson.Value)] -> m ()
setContextValuesAt ctx k kvs = updateAt ctx (Update.setContextValues k kvs)

-- | This operation sets a field in a custom context on the isolation scope,
-- creating the context when absent. Typed contexts remain unchanged.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
setContextValueAt :: (MonadIO m) => Context -> Text -> Text -> Aeson.Value -> m ()
setContextValueAt ctx k key value = updateAt ctx (Update.setContextValue k key value)

-- | This operation removes a field from a local custom context on the
-- isolation scope. Absent and typed contexts remain unchanged; removing the
-- last field retains an empty context.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
removeContextValueAt :: (MonadIO m) => Context -> Text -> Text -> m ()
removeContextValueAt ctx k key = updateAt ctx (Update.removeContextValue k key)

-- | This operation transforms a custom context's fields on the isolation
-- scope, starting from an empty map when absent. Typed contexts remain
-- unchanged.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
modifyContextValuesAt :: (MonadIO m) => Context -> Text -> (Map Text Aeson.Value -> Map Text Aeson.Value) -> m ()
modifyContextValuesAt ctx k f = updateAt ctx (Update.modifyContextValues k f)

-- | This operation replaces the isolation scope's event processor.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
setEventProcessorAt :: (MonadIO m) => Context -> (CapturedEvent -> Maybe Patrol.Event) -> m ()
setEventProcessorAt ctx f = updateAt ctx (Update.setEventProcessor f)

-- | This operation appends an event processor to the isolation scope's
-- processor chain.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
addEventProcessorAt :: (MonadIO m) => Context -> (CapturedEvent -> Maybe Patrol.Event) -> m ()
addEventProcessorAt ctx g = updateAt ctx (Update.addEventProcessor g)

-- | This operation restores the isolation scope's event processor to the
-- identity processor.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
unsetEventProcessorAt :: (MonadIO m) => Context -> m ()
unsetEventProcessorAt ctx = updateAt ctx Update.unsetEventProcessor

-- | Like 'addBreadcrumb', but takes the 'Context' explicitly instead of
-- reading the thread-local one.
--
-- Targets 'resolveBreadcrumbScope' (isolation only) — no-ops when no
-- isolation scope is active on the given 'Context'.
addBreadcrumbAt :: (MonadIO m, Witch.From a Sentry.Breadcrumb.BreadcrumbUpdate) => Context -> a -> m ()
addBreadcrumbAt ctx upd = do
  let !crumb = Sentry.Update.run upd Sentry.Breadcrumb.empty
  client <- resolveClientAt ctx
  for_ (resolveBreadcrumbScope ctx) \scope ->
    addBreadcrumbToScope client.options scope crumb

-- | This operation adds breadcrumbs in order to the isolation scope in the
-- supplied 'Context'. It leaves the context unchanged when isolation is absent.
addBreadcrumbsAt :: (MonadIO m) => Context -> [Patrol.Breadcrumb] -> m ()
addBreadcrumbsAt ctx crumbs = do
  client <- resolveClientAt ctx
  for_ (resolveBreadcrumbScope ctx) \scope ->
    for_ crumbs (addBreadcrumbToScope client.options scope)

-- | This operation removes all breadcrumbs from the isolation scope.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
clearBreadcrumbsAt :: (MonadIO m) => Context -> m ()
clearBreadcrumbsAt ctx = for_ (resolveBreadcrumbScope ctx) \scope -> Update.apply scope Update.clearBreadcrumbs

-- | Enforce 'ClientOptions.beforeBreadcrumb' and trim to
-- 'ClientOptions.maxBreadcrumbs', then append to the scope.
addBreadcrumbToScope :: (MonadIO m) => ClientOptions -> Scope -> Patrol.Breadcrumb -> m ()
addBreadcrumbToScope opts scope crumb0 = liftIO do
  now <- getCurrentTime
  -- Default timestamp and type_ when absent.
  let crumb1 = crumb0{Patrol.Breadcrumb.timestamp = crumb0.timestamp <|> Just now}
      crumb2 = crumb1{Patrol.Breadcrumb.type_ = crumb1.type_ <|> Just Patrol.BreadcrumbType.Default}
      maxN = fromIntegral opts.maxBreadcrumbs :: Int
  -- Run beforeBreadcrumb; Nothing from the callback drops the crumb.
  case maybe (Just crumb2) ($ crumb2) opts.beforeBreadcrumb of
    Nothing -> pure ()
    Just !crumb3 ->
      Update.apply scope (Update.appendBreadcrumb crumb3 <> Update.trimBreadcrumbs maxN)

-- | Replace the local typed context. See 'Update.setOsContext'.
setOsContext :: (MonadIO m, Witch.From a OsContextUpdate) => Scope -> a -> m ()
setOsContext scope upd = Update.apply scope (Update.setOsContext upd)

-- | This operation replaces the isolation scope's entire os context with an
-- operating system payload.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
setOsContextAt :: (MonadIO m, Witch.From a OsContextUpdate) => Context -> a -> m ()
setOsContextAt ctx upd = updateAt ctx (Update.setOsContext upd)

-- | Replace the local typed context. See 'Update.setAppContext'.
setAppContext :: (MonadIO m, Witch.From a AppContextUpdate) => Scope -> a -> m ()
setAppContext scope upd = Update.apply scope (Update.setAppContext upd)

-- | This operation replaces the isolation scope's entire app context with an
-- application payload.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
setAppContextAt :: (MonadIO m, Witch.From a AppContextUpdate) => Context -> a -> m ()
setAppContextAt ctx upd = updateAt ctx (Update.setAppContext upd)

-- | This operation modifies the supplied scope's local user only when present.
modifyExistingUser :: (MonadIO m, Witch.From a UserUpdate) => Scope -> a -> m ()
modifyExistingUser scope upd = Update.apply scope (Update.modifyExistingUser upd)

-- | This operation modifies the isolation scope's user only when one exists
-- locally.
--
-- The target is the isolation scope in the supplied 'Context'. If that scope
-- is absent, this operation leaves the context unchanged.
modifyExistingUserAt :: (MonadIO m, Witch.From a UserUpdate) => Context -> a -> m ()
modifyExistingUserAt ctx upd = updateAt ctx (Update.modifyExistingUser upd)

-- | Replace the assignment, or remove it when given 'Nothing'.
setOptionalLevel :: (MonadIO m) => Scope -> Maybe Patrol.Level -> m ()
setOptionalLevel scope value = Update.apply scope (Update.setOptionalLevel value)

-- | Replace or remove the assignment on an existing target in this context.
setOptionalLevelAt :: (MonadIO m) => Context -> Maybe Patrol.Level -> m ()
setOptionalLevelAt ctx value = updateAt ctx (Update.setOptionalLevel value)

-- | Replace the assignment, or remove it when given 'Nothing'.
setOptionalFingerprint :: (MonadIO m) => Scope -> Maybe [Text] -> m ()
setOptionalFingerprint scope value = Update.apply scope (Update.setOptionalFingerprint value)

-- | Replace or remove the assignment on an existing target in this context.
setOptionalFingerprintAt :: (MonadIO m) => Context -> Maybe [Text] -> m ()
setOptionalFingerprintAt ctx value = updateAt ctx (Update.setOptionalFingerprint value)

-- | Replace the assignment, or remove it when given 'Nothing'.
setOptionalTransaction :: (MonadIO m) => Scope -> Maybe Text -> m ()
setOptionalTransaction scope value = Update.apply scope (Update.setOptionalTransaction value)

-- | Replace or remove the assignment on an existing target in this context.
setOptionalTransactionAt :: (MonadIO m) => Context -> Maybe Text -> m ()
setOptionalTransactionAt ctx value = for_ (lookupCurrent ctx) \scope -> setOptionalTransaction scope value

-- | Replace the assignment, or remove it when given 'Nothing'.
setOptionalTag :: (MonadIO m) => Scope -> Text -> Maybe Text -> m ()
setOptionalTag scope key value = Update.apply scope (Update.setOptionalTag key value)

-- | Replace or remove the assignment on an existing target in this context.
setOptionalTagAt :: (MonadIO m) => Context -> Text -> Maybe Text -> m ()
setOptionalTagAt ctx key value = updateAt ctx (Update.setOptionalTag key value)

-- | Replace the assignment, or remove it when given 'Nothing'.
setOptionalExtra :: (MonadIO m) => Scope -> Text -> Maybe Aeson.Value -> m ()
setOptionalExtra scope key value = Update.apply scope (Update.setOptionalExtra key value)

-- | Replace or remove the assignment on an existing target in this context.
setOptionalExtraAt :: (MonadIO m) => Context -> Text -> Maybe Aeson.Value -> m ()
setOptionalExtraAt ctx key value = updateAt ctx (Update.setOptionalExtra key value)

-- | Replace the assignment, or remove it when given 'Nothing'.
setOptionalContext :: (MonadIO m) => Scope -> Text -> Maybe Patrol.Context -> m ()
setOptionalContext scope key value = Update.apply scope (Update.setOptionalContext key value)

-- | Replace or remove the assignment on an existing target in this context.
setOptionalContextAt :: (MonadIO m) => Context -> Text -> Maybe Patrol.Context -> m ()
setOptionalContextAt ctx key value = updateAt ctx (Update.setOptionalContext key value)

-- | Replace the assignment, or remove it when given 'Nothing'.
setOptionalRuntimeContext :: (MonadIO m) => Scope -> Maybe Sentry.RuntimeContext.RuntimeContext -> m ()
setOptionalRuntimeContext scope value = Update.apply scope (Update.setOptionalRuntimeContext value)

-- | Replace or remove the assignment on an existing target in this context.
setOptionalRuntimeContextAt :: (MonadIO m) => Context -> Maybe Sentry.RuntimeContext.RuntimeContext -> m ()
setOptionalRuntimeContextAt ctx value = updateAt ctx (Update.setOptionalRuntimeContext value)

-- | Replace the assignment, or remove it when given 'Nothing'.
setOptionalContextValues :: (MonadIO m) => Scope -> Text -> Maybe [(Text, Aeson.Value)] -> m ()
setOptionalContextValues scope key value = Update.apply scope (Update.setOptionalContextValues key value)

-- | Replace or remove the assignment on an existing target in this context.
setOptionalContextValuesAt :: (MonadIO m) => Context -> Text -> Maybe [(Text, Aeson.Value)] -> m ()
setOptionalContextValuesAt ctx key value = updateAt ctx (Update.setOptionalContextValues key value)

-- | Replace the assignment, or remove it when given 'Nothing'.
setOptionalContextValue :: (MonadIO m) => Scope -> Text -> Text -> Maybe Aeson.Value -> m ()
setOptionalContextValue scope key fieldKey value = Update.apply scope (Update.setOptionalContextValue key fieldKey value)

-- | Replace or remove the assignment on an existing target in this context.
setOptionalContextValueAt :: (MonadIO m) => Context -> Text -> Text -> Maybe Aeson.Value -> m ()
setOptionalContextValueAt ctx key fieldKey value = updateAt ctx (Update.setOptionalContextValue key fieldKey value)

-- | Replace the assignment, or remove it when given 'Nothing'.
setOptionalOsContext :: (MonadIO m) => Scope -> Maybe Sentry.OsContext.OsContext -> m ()
setOptionalOsContext scope value = Update.apply scope (Update.setOptionalOsContext value)

-- | Replace or remove the assignment on an existing target in this context.
setOptionalOsContextAt :: (MonadIO m) => Context -> Maybe Sentry.OsContext.OsContext -> m ()
setOptionalOsContextAt ctx value = updateAt ctx (Update.setOptionalOsContext value)

-- | Replace the assignment, or remove it when given 'Nothing'.
setOptionalAppContext :: (MonadIO m) => Scope -> Maybe Sentry.AppContext.AppContext -> m ()
setOptionalAppContext scope value = Update.apply scope (Update.setOptionalAppContext value)

-- | Replace or remove the assignment on an existing target in this context.
setOptionalAppContextAt :: (MonadIO m) => Context -> Maybe Sentry.AppContext.AppContext -> m ()
setOptionalAppContextAt ctx value = updateAt ctx (Update.setOptionalAppContext value)

-- | Replace the assignment, or remove it when given 'Nothing'.
setOptionalBrowserContext :: (MonadIO m) => Scope -> Maybe Sentry.BrowserContext.BrowserContext -> m ()
setOptionalBrowserContext scope value = Update.apply scope (Update.setOptionalBrowserContext value)

-- | Replace or remove the assignment on an existing target in this context.
setOptionalBrowserContextAt :: (MonadIO m) => Context -> Maybe Sentry.BrowserContext.BrowserContext -> m ()
setOptionalBrowserContextAt ctx value = updateAt ctx (Update.setOptionalBrowserContext value)

-- | Replace the assignment, or remove it when given 'Nothing'.
setOptionalDeviceContext :: (MonadIO m) => Scope -> Maybe Sentry.DeviceContext.DeviceContext -> m ()
setOptionalDeviceContext scope value = Update.apply scope (Update.setOptionalDeviceContext value)

-- | Replace or remove the assignment on an existing target in this context.
setOptionalDeviceContextAt :: (MonadIO m) => Context -> Maybe Sentry.DeviceContext.DeviceContext -> m ()
setOptionalDeviceContextAt ctx value = updateAt ctx (Update.setOptionalDeviceContext value)

-- | Replace the assignment, or remove it when given 'Nothing'.
setOptionalTraceContext :: (MonadIO m) => Scope -> Maybe Sentry.TraceContext.TraceContext -> m ()
setOptionalTraceContext scope value = Update.apply scope (Update.setOptionalTraceContext value)

-- | Replace or remove the assignment on an existing target in this context.
setOptionalTraceContextAt :: (MonadIO m) => Context -> Maybe Sentry.TraceContext.TraceContext -> m ()
setOptionalTraceContextAt ctx value = updateAt ctx (Update.setOptionalTraceContext value)

-- | Apply 'Update.alterAppContext' to the explicit scope, without requiring initialization.
alterAppContext :: (MonadIO m) => Scope -> (Maybe Sentry.AppContext.AppContext -> Maybe Sentry.AppContext.AppContext) -> m ()
alterAppContext scope f = Update.apply scope (Update.alterAppContext f)

-- | Apply 'Update.alterAppContext' to isolation in the context; absent targets are unchanged.
alterAppContextAt :: (MonadIO m) => Context -> (Maybe Sentry.AppContext.AppContext -> Maybe Sentry.AppContext.AppContext) -> m ()
alterAppContextAt ctx f = updateAt ctx (Update.alterAppContext f)

-- | Apply 'Update.alterOsContext' to the explicit scope, without requiring initialization.
alterOsContext :: (MonadIO m) => Scope -> (Maybe Sentry.OsContext.OsContext -> Maybe Sentry.OsContext.OsContext) -> m ()
alterOsContext scope f = Update.apply scope (Update.alterOsContext f)

-- | Apply 'Update.alterOsContext' to isolation in the context; absent targets are unchanged.
alterOsContextAt :: (MonadIO m) => Context -> (Maybe Sentry.OsContext.OsContext -> Maybe Sentry.OsContext.OsContext) -> m ()
alterOsContextAt ctx f = updateAt ctx (Update.alterOsContext f)

-- | Apply 'Update.alterRuntimeContext' to the explicit scope, without requiring initialization.
alterRuntimeContext :: (MonadIO m) => Scope -> (Maybe Sentry.RuntimeContext.RuntimeContext -> Maybe Sentry.RuntimeContext.RuntimeContext) -> m ()
alterRuntimeContext scope f = Update.apply scope (Update.alterRuntimeContext f)

-- | Apply 'Update.alterRuntimeContext' to isolation in the context; absent targets are unchanged.
alterRuntimeContextAt :: (MonadIO m) => Context -> (Maybe Sentry.RuntimeContext.RuntimeContext -> Maybe Sentry.RuntimeContext.RuntimeContext) -> m ()
alterRuntimeContextAt ctx f = updateAt ctx (Update.alterRuntimeContext f)

-- | Apply 'Update.alterBrowserContext' to the explicit scope, without requiring initialization.
alterBrowserContext :: (MonadIO m) => Scope -> (Maybe Sentry.BrowserContext.BrowserContext -> Maybe Sentry.BrowserContext.BrowserContext) -> m ()
alterBrowserContext scope f = Update.apply scope (Update.alterBrowserContext f)

-- | Apply 'Update.alterBrowserContext' to isolation in the context; absent targets are unchanged.
alterBrowserContextAt :: (MonadIO m) => Context -> (Maybe Sentry.BrowserContext.BrowserContext -> Maybe Sentry.BrowserContext.BrowserContext) -> m ()
alterBrowserContextAt ctx f = updateAt ctx (Update.alterBrowserContext f)

-- | Apply 'Update.alterDeviceContext' to the explicit scope, without requiring initialization.
alterDeviceContext :: (MonadIO m) => Scope -> (Maybe Sentry.DeviceContext.DeviceContext -> Maybe Sentry.DeviceContext.DeviceContext) -> m ()
alterDeviceContext scope f = Update.apply scope (Update.alterDeviceContext f)

-- | Apply 'Update.alterDeviceContext' to isolation in the context; absent targets are unchanged.
alterDeviceContextAt :: (MonadIO m) => Context -> (Maybe Sentry.DeviceContext.DeviceContext -> Maybe Sentry.DeviceContext.DeviceContext) -> m ()
alterDeviceContextAt ctx f = updateAt ctx (Update.alterDeviceContext f)

-- | Apply 'Update.alterTraceContext' to the explicit scope, without requiring initialization.
alterTraceContext :: (MonadIO m) => Scope -> (Maybe Sentry.TraceContext.TraceContext -> Maybe Sentry.TraceContext.TraceContext) -> m ()
alterTraceContext scope f = Update.apply scope (Update.alterTraceContext f)

-- | Apply 'Update.alterTraceContext' to isolation in the context; absent targets are unchanged.
alterTraceContextAt :: (MonadIO m) => Context -> (Maybe Sentry.TraceContext.TraceContext -> Maybe Sentry.TraceContext.TraceContext) -> m ()
alterTraceContextAt ctx f = updateAt ctx (Update.alterTraceContext f)

-- | Apply 'Update.modifyExistingContextValue' to the explicit scope, without requiring initialization.
modifyExistingContextValue :: (MonadIO m) => Scope -> Text -> Text -> (Aeson.Value -> Aeson.Value) -> m ()
modifyExistingContextValue scope key field f = Update.apply scope (Update.modifyExistingContextValue key field f)

-- | Apply 'Update.modifyExistingContextValue' to isolation in the context; absent targets are unchanged.
modifyExistingContextValueAt :: (MonadIO m) => Context -> Text -> Text -> (Aeson.Value -> Aeson.Value) -> m ()
modifyExistingContextValueAt ctx key field f = updateAt ctx (Update.modifyExistingContextValue key field f)

-- | Apply 'Update.alterContextValue' to the explicit scope, without requiring initialization.
alterContextValue :: (MonadIO m) => Scope -> Text -> Text -> (Maybe Aeson.Value -> Maybe Aeson.Value) -> m ()
alterContextValue scope key field f = Update.apply scope (Update.alterContextValue key field f)

-- | Apply 'Update.alterContextValue' to isolation in the context; absent targets are unchanged.
alterContextValueAt :: (MonadIO m) => Context -> Text -> Text -> (Maybe Aeson.Value -> Maybe Aeson.Value) -> m ()
alterContextValueAt ctx key field f = updateAt ctx (Update.alterContextValue key field f)

-- | Apply 'Update.filterFingerprint' to the explicit scope, without requiring initialization.
filterFingerprint :: (MonadIO m) => Scope -> (Text -> Bool) -> m ()
filterFingerprint scope predicate = Update.apply scope (Update.filterFingerprint predicate)

-- | Apply 'Update.filterFingerprint' to isolation in the context; absent targets are unchanged.
filterFingerprintAt :: (MonadIO m) => Context -> (Text -> Bool) -> m ()
filterFingerprintAt ctx predicate = updateAt ctx (Update.filterFingerprint predicate)

-- | Apply 'Update.filterBreadcrumbs' to the explicit scope, without requiring initialization.
filterBreadcrumbs :: (MonadIO m) => Scope -> (Patrol.Breadcrumb -> Bool) -> m ()
filterBreadcrumbs scope predicate = Update.apply scope (Update.filterBreadcrumbs predicate)

-- | Apply 'Update.filterBreadcrumbs' to isolation in the context; absent targets are unchanged.
filterBreadcrumbsAt :: (MonadIO m) => Context -> (Patrol.Breadcrumb -> Bool) -> m ()
filterBreadcrumbsAt ctx predicate = updateAt ctx (Update.filterBreadcrumbs predicate)
