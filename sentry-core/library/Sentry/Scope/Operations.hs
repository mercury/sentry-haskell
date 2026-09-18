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

    -- *** Level
    setLevel,
    setLevelAt,
    setOptionalLevel,
    setOptionalLevelAt,
    unsetLevel,
    unsetLevelAt,

    -- *** User
    setUser,
    setUserAt,
    setOptionalUser,
    setOptionalUserAt,
    unsetUser,
    unsetUserAt,
    modifyUser,
    modifyUserAt,
    modifyExistingUser,
    modifyExistingUserAt,

    -- *** Fingerprint
    setFingerprint,
    setFingerprintAt,
    setOptionalFingerprint,
    setOptionalFingerprintAt,
    unsetFingerprint,
    unsetFingerprintAt,
    filterFingerprint,
    filterFingerprintAt,

    -- *** Transaction
    setTransaction,
    setTransactionAt,
    setOptionalTransaction,
    setOptionalTransactionAt,
    unsetTransaction,
    unsetTransactionAt,

    -- *** Tags
    setTag,
    setTagAt,
    setOptionalTag,
    setOptionalTagAt,
    removeTag,
    removeTagAt,
    clearTags,
    clearTagsAt,

    -- *** Extras
    setExtra,
    setExtraAt,
    setOptionalExtra,
    setOptionalExtraAt,
    removeExtra,
    removeExtraAt,
    clearExtras,
    clearExtrasAt,

    -- *** Contexts

    -- Generic contexts
    setContext,
    setContextAt,
    setOptionalContext,
    setOptionalContextAt,
    removeContext,
    removeContextAt,
    clearContexts,
    clearContextsAt,
    -- Custom context values
    setContextValues,
    setContextValuesAt,
    setOptionalContextValues,
    setOptionalContextValuesAt,
    setContextValue,
    setContextValueAt,
    setOptionalContextValue,
    setOptionalContextValueAt,
    removeContextValue,
    removeContextValueAt,
    modifyContextValues,
    modifyContextValuesAt,
    modifyExistingContextValue,
    modifyExistingContextValueAt,
    alterContextValue,
    alterContextValueAt,
    -- OS context
    setOsContext,
    setOsContextAt,
    setOptionalOsContext,
    setOptionalOsContextAt,
    alterOsContext,
    alterOsContextAt,
    -- App context
    setAppContext,
    setAppContextAt,
    setOptionalAppContext,
    setOptionalAppContextAt,
    alterAppContext,
    alterAppContextAt,
    -- Runtime context
    setRuntimeContext,
    setRuntimeContextAt,
    setOptionalRuntimeContext,
    setOptionalRuntimeContextAt,
    alterRuntimeContext,
    alterRuntimeContextAt,
    -- Browser context
    setOptionalBrowserContext,
    setOptionalBrowserContextAt,
    alterBrowserContext,
    alterBrowserContextAt,
    -- Device context
    setOptionalDeviceContext,
    setOptionalDeviceContextAt,
    alterDeviceContext,
    alterDeviceContextAt,
    -- Trace context
    setOptionalTraceContext,
    setOptionalTraceContextAt,
    alterTraceContext,
    alterTraceContextAt,

    -- *** Breadcrumbs
    addBreadcrumb,
    addBreadcrumbAt,
    addBreadcrumbs,
    addBreadcrumbsAt,
    clearBreadcrumbs,
    clearBreadcrumbsAt,
    filterBreadcrumbs,
    filterBreadcrumbsAt,

    -- ** Thread-local Context Manipulation
    lookupCurrent,
    insertCurrent,
    removeCurrent,
    lookupIsolation,
    insertIsolation,
    removeIsolation,
    acquireCurrent,
    releaseCurrent,

    -- ** Thread propagation
    propagateScope,

    -- ** Context-first operations
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
  )
where

import Control.Applicative ((<|>))
import Control.Exception (bracket, mask_)
import Control.Monad (guard, void)
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

-- | Clone the current layer and return its parent together with the child.
--
-- Creates a fresh current scope when none exists locally, otherwise clones
-- the existing one. The clone is installed on the thread-local 'Context'
-- immediately; call 'releaseCurrent' with the returned pair to restore the
-- parent.
acquireCurrent :: IO (Maybe Scope, Scope)
acquireCurrent = do
  context <- ThreadLocal.getContext
  scope <- case lookupCurrent context of
    Nothing -> create Current
    Just s -> clone s
  let parentScope = lookupCurrent context
  ThreadLocal.adjustContext (insertCurrent scope)
  pure (parentScope, scope)

-- | Restore only the current key in the latest context.
--
-- Reinstalls the parent captured by 'acquireCurrent', or removes the key
-- entirely when there was no parent.
releaseCurrent :: (Maybe Scope, Scope) -> IO ()
releaseCurrent (parentScope, _) =
  ThreadLocal.adjustContext \ctx ->
    maybe (removeCurrent ctx) (`insertCurrent` ctx) parentScope

-- | Clone the current scope layer around the given action, restoring the
-- prior current scope once it completes.
--
-- This is 'bracket' over 'acquireCurrent' and 'releaseCurrent' with no
-- exception annotation.
withClonedCurrentScope :: IO a -> IO a
withClonedCurrentScope action = bracket acquireCurrent releaseCurrent (const action)

-- | Capture the calling thread's 'OpenTelemetry.Context.Context' now, and
-- return a deferred action that, when run, attaches that captured context and
-- then clones the current scope on top of it.
propagateScope :: IO a -> IO (IO a)
propagateScope action = do
  context <- ThreadLocal.getContext
  pure do
    void (ThreadLocal.attachContext context)
    withClonedCurrentScope action

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
-- see 'lookupIsolation'.
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

-- | Apply a 'Update.ScopeUpdate' to the isolation scope on the given context.
updateAt :: (MonadIO m) => Context -> Update.ScopeUpdate -> m ()
updateAt ctx upd = for_ (lookupIsolation ctx) \scope -> Update.apply scope upd

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

-- | Apply 'Update.setLevel' to the isolation scope on the given context.
setLevelAt :: (MonadIO m) => Context -> Patrol.Level -> m ()
setLevelAt ctx level = updateAt ctx (Update.setLevel level)

-- | Apply 'Update.unsetLevel' to the isolation scope on the given context.
unsetLevelAt :: (MonadIO m) => Context -> m ()
unsetLevelAt ctx = updateAt ctx Update.unsetLevel

-- | Apply 'Update.setUser' to the isolation scope on the given context.
setUserAt :: (MonadIO m, Witch.From a UserUpdate) => Context -> a -> m ()
setUserAt ctx u = updateAt ctx (Update.setUser u)

-- | Apply 'Update.setOptionalUser' to the isolation scope on the given context.
setOptionalUserAt :: (MonadIO m) => Context -> Maybe User -> m ()
setOptionalUserAt ctx user = updateAt ctx (Update.setOptionalUser user)

-- | Apply 'Update.unsetUser' to the isolation scope on the given context.
unsetUserAt :: (MonadIO m) => Context -> m ()
unsetUserAt ctx = updateAt ctx Update.unsetUser

-- | Apply 'Update.modifyUser' to the isolation scope on the given context.
modifyUserAt :: (MonadIO m, Witch.From a UserUpdate) => Context -> a -> m ()
modifyUserAt ctx upd = updateAt ctx (Update.modifyUser upd)

-- | Apply 'Update.setFingerprint' to the isolation scope on the given
-- context.
setFingerprintAt :: (MonadIO m) => Context -> [Text] -> m ()
setFingerprintAt ctx fp = updateAt ctx (Update.setFingerprint fp)

-- | Apply 'Update.unsetFingerprint' to the isolation scope on the given
-- context.
unsetFingerprintAt :: (MonadIO m) => Context -> m ()
unsetFingerprintAt ctx = updateAt ctx Update.unsetFingerprint

-- | Apply 'Update.setTransaction' to the current scope on the given context.
setTransactionAt :: (MonadIO m) => Context -> Text -> m ()
setTransactionAt ctx t = for_ (lookupCurrent ctx) \scope -> setTransaction scope t

-- | Apply 'Update.unsetTransaction' to the current scope on the given
-- context.
unsetTransactionAt :: (MonadIO m) => Context -> m ()
unsetTransactionAt ctx = for_ (lookupCurrent ctx) unsetTransaction

-- | Apply 'Update.setTag' to the isolation scope on the given context.
setTagAt :: (MonadIO m) => Context -> Text -> Text -> m ()
setTagAt ctx k v = updateAt ctx (Update.setTag k v)

-- | Apply 'Update.removeTag' to the isolation scope on the given context.
removeTagAt :: (MonadIO m) => Context -> Text -> m ()
removeTagAt ctx k = updateAt ctx (Update.removeTag k)

-- | Apply 'Update.clearTags' to the isolation scope on the given context.
clearTagsAt :: (MonadIO m) => Context -> m ()
clearTagsAt ctx = updateAt ctx Update.clearTags

-- | Apply 'Update.setExtra' to the isolation scope on the given context.
setExtraAt :: (MonadIO m) => Context -> Text -> Aeson.Value -> m ()
setExtraAt ctx k v = updateAt ctx (Update.setExtra k v)

-- | Apply 'Update.removeExtra' to the isolation scope on the given context.
removeExtraAt :: (MonadIO m) => Context -> Text -> m ()
removeExtraAt ctx k = updateAt ctx (Update.removeExtra k)

-- | Apply 'Update.clearExtras' to the isolation scope on the given context.
clearExtrasAt :: (MonadIO m) => Context -> m ()
clearExtrasAt ctx = updateAt ctx Update.clearExtras

-- | Apply 'Update.setContext' to the isolation scope on the given context.
setContextAt :: (MonadIO m) => Context -> Text -> Patrol.Context -> m ()
setContextAt ctx k v = updateAt ctx (Update.setContext k v)

-- | Apply 'Update.removeContext' to the isolation scope on the given
-- context.
removeContextAt :: (MonadIO m) => Context -> Text -> m ()
removeContextAt ctx k = updateAt ctx (Update.removeContext k)

-- | Apply 'Update.clearContexts' to the isolation scope on the given
-- context.
clearContextsAt :: (MonadIO m) => Context -> m ()
clearContextsAt ctx = updateAt ctx Update.clearContexts

-- | Apply 'Update.setRuntimeContext' to the isolation scope on the given
-- context.
setRuntimeContextAt :: (MonadIO m, Witch.From a RuntimeContextUpdate) => Context -> a -> m ()
setRuntimeContextAt ctx rc = updateAt ctx (Update.setRuntimeContext rc)

-- | Apply 'Update.setContextValue' to the isolation scope on the given
-- context.
setContextValuesAt :: (MonadIO m) => Context -> Text -> [(Text, Aeson.Value)] -> m ()
setContextValuesAt ctx k kvs = updateAt ctx (Update.setContextValues k kvs)

-- | Apply 'Update.setContextValue' to the isolation scope on the given
-- context.
setContextValueAt :: (MonadIO m) => Context -> Text -> Text -> Aeson.Value -> m ()
setContextValueAt ctx k key value = updateAt ctx (Update.setContextValue k key value)

-- | Apply 'Update.removeContextValue' to the isolation scope on the given
-- context.
removeContextValueAt :: (MonadIO m) => Context -> Text -> Text -> m ()
removeContextValueAt ctx k key = updateAt ctx (Update.removeContextValue k key)

-- | Apply 'Update.modifyContextValues' to the isolation scope on the given
-- context.
modifyContextValuesAt :: (MonadIO m) => Context -> Text -> (Map Text Aeson.Value -> Map Text Aeson.Value) -> m ()
modifyContextValuesAt ctx k f = updateAt ctx (Update.modifyContextValues k f)

-- | Assign an event processor to the isolation scope on the given context,
-- replacing an event processor if one already existed.
setEventProcessorAt :: (MonadIO m) => Context -> (CapturedEvent -> Maybe Patrol.Event) -> m ()
setEventProcessorAt ctx f = updateAt ctx (Update.setEventProcessor f)

-- | Add an event processor to the isolation scope on the given context.
addEventProcessorAt :: (MonadIO m) => Context -> (CapturedEvent -> Maybe Patrol.Event) -> m ()
addEventProcessorAt ctx g = updateAt ctx (Update.addEventProcessor g)

-- | Remove the event processor from the isolation scope on the given context.
unsetEventProcessorAt :: (MonadIO m) => Context -> m ()
unsetEventProcessorAt ctx = updateAt ctx Update.unsetEventProcessor

-- | Add a single breadcrumb to the isolation scope on the given context.
addBreadcrumbAt :: (MonadIO m, Witch.From a Sentry.Breadcrumb.BreadcrumbUpdate) => Context -> a -> m ()
addBreadcrumbAt ctx upd = do
  let !crumb = Sentry.Update.run upd Sentry.Breadcrumb.empty
  client <- resolveClientAt ctx
  for_ (lookupIsolation ctx) \scope ->
    addBreadcrumbToScope client.options scope crumb

-- | Add a list of breadcrumbs to the isolation scope on the given context.
addBreadcrumbsAt :: (MonadIO m) => Context -> [Patrol.Breadcrumb] -> m ()
addBreadcrumbsAt ctx crumbs = do
  client <- resolveClientAt ctx
  for_ (lookupIsolation ctx) \scope ->
    for_ crumbs (addBreadcrumbToScope client.options scope)

-- | Remove all breadcrumbs from the isolation scope on the given context.
clearBreadcrumbsAt :: (MonadIO m) => Context -> m ()
clearBreadcrumbsAt ctx = for_ (lookupIsolation ctx) \scope -> Update.apply scope Update.clearBreadcrumbs

-- | Add a breadcrumb to the given scope, applying the 'beforeBreadcrumb' hook
-- and trim the list of breadcrumbs if it exceeds 'maxBreadcrumbs'.
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

-- | Apply 'Update.setOsContext' to the given scope.
setOsContext :: (MonadIO m, Witch.From a OsContextUpdate) => Scope -> a -> m ()
setOsContext scope upd = Update.apply scope (Update.setOsContext upd)

-- | Apply 'Update.setOsContext' to the isolation scope on the given context.
setOsContextAt :: (MonadIO m, Witch.From a OsContextUpdate) => Context -> a -> m ()
setOsContextAt ctx upd = updateAt ctx (Update.setOsContext upd)

-- | Apply 'Update.setAppContext' to the given scope.
setAppContext :: (MonadIO m, Witch.From a AppContextUpdate) => Scope -> a -> m ()
setAppContext scope upd = Update.apply scope (Update.setAppContext upd)

-- | Apply 'Update.setAppContext' to the isolation scope on the given context.
setAppContextAt :: (MonadIO m, Witch.From a AppContextUpdate) => Context -> a -> m ()
setAppContextAt ctx upd = updateAt ctx (Update.setAppContext upd)

-- | Apply 'Update.modifyExistingUser' to the given scope.
modifyExistingUser :: (MonadIO m, Witch.From a UserUpdate) => Scope -> a -> m ()
modifyExistingUser scope upd = Update.apply scope (Update.modifyExistingUser upd)

-- | Apply 'Update.modifyExistingUser' to the isolation scope on the given
-- context.
modifyExistingUserAt :: (MonadIO m, Witch.From a UserUpdate) => Context -> a -> m ()
modifyExistingUserAt ctx upd = updateAt ctx (Update.modifyExistingUser upd)

-- | Apply 'Update.setOptionalLevel' to the given scope.
setOptionalLevel :: (MonadIO m) => Scope -> Maybe Patrol.Level -> m ()
setOptionalLevel scope value = Update.apply scope (Update.setOptionalLevel value)

-- | Apply 'Update.setOptionalLevel' to the isolation scope on the given context.
setOptionalLevelAt :: (MonadIO m) => Context -> Maybe Patrol.Level -> m ()
setOptionalLevelAt ctx value = updateAt ctx (Update.setOptionalLevel value)

-- | Apply 'Update.setOptionalFingerprint' to the given scope.
setOptionalFingerprint :: (MonadIO m) => Scope -> Maybe [Text] -> m ()
setOptionalFingerprint scope value = Update.apply scope (Update.setOptionalFingerprint value)

-- | Apply 'Update.setOptionalFingerprint' to the isolation scope on the given
-- context.
setOptionalFingerprintAt :: (MonadIO m) => Context -> Maybe [Text] -> m ()
setOptionalFingerprintAt ctx value = updateAt ctx (Update.setOptionalFingerprint value)

-- | Apply 'Update.setOptionalTransaction' to the given scope.
setOptionalTransaction :: (MonadIO m) => Scope -> Maybe Text -> m ()
setOptionalTransaction scope value = Update.apply scope (Update.setOptionalTransaction value)

-- | Apply 'Update.setOptionalTransaction' to the isolation scope on the given
-- context.
setOptionalTransactionAt :: (MonadIO m) => Context -> Maybe Text -> m ()
setOptionalTransactionAt ctx value = for_ (lookupCurrent ctx) \scope -> setOptionalTransaction scope value

-- | Apply 'Update.setOptionalTag' to the given scope.
setOptionalTag :: (MonadIO m) => Scope -> Text -> Maybe Text -> m ()
setOptionalTag scope key value = Update.apply scope (Update.setOptionalTag key value)

-- | Apply 'Update.setOptionalTag' to the isolation scope on the given context.
setOptionalTagAt :: (MonadIO m) => Context -> Text -> Maybe Text -> m ()
setOptionalTagAt ctx key value = updateAt ctx (Update.setOptionalTag key value)

-- | Apply 'Update.setOptionalExtra' to the given scope.
setOptionalExtra :: (MonadIO m) => Scope -> Text -> Maybe Aeson.Value -> m ()
setOptionalExtra scope key value = Update.apply scope (Update.setOptionalExtra key value)

-- | Apply 'Update.setOptionalExtra' to the isolation scope on the given
-- context.
setOptionalExtraAt :: (MonadIO m) => Context -> Text -> Maybe Aeson.Value -> m ()
setOptionalExtraAt ctx key value = updateAt ctx (Update.setOptionalExtra key value)

-- | Apply 'Update.setOptionalContext' to the given scope.
setOptionalContext :: (MonadIO m) => Scope -> Text -> Maybe Patrol.Context -> m ()
setOptionalContext scope key value = Update.apply scope (Update.setOptionalContext key value)

-- | Apply 'Update.setOptionalContext' to the isolation scope on the given
-- context.
setOptionalContextAt :: (MonadIO m) => Context -> Text -> Maybe Patrol.Context -> m ()
setOptionalContextAt ctx key value = updateAt ctx (Update.setOptionalContext key value)

-- | Apply 'Update.setOptionalRuntimeContext' to the given scope.
setOptionalRuntimeContext :: (MonadIO m) => Scope -> Maybe Sentry.RuntimeContext.RuntimeContext -> m ()
setOptionalRuntimeContext scope value = Update.apply scope (Update.setOptionalRuntimeContext value)

-- | Apply 'Update.setOptionalRuntimeContext' to the isolation scope on the
-- given context.
setOptionalRuntimeContextAt :: (MonadIO m) => Context -> Maybe Sentry.RuntimeContext.RuntimeContext -> m ()
setOptionalRuntimeContextAt ctx value = updateAt ctx (Update.setOptionalRuntimeContext value)

-- | Apply 'Update.setOptionalContextValues' to the given scope.
setOptionalContextValues :: (MonadIO m) => Scope -> Text -> Maybe [(Text, Aeson.Value)] -> m ()
setOptionalContextValues scope key value = Update.apply scope (Update.setOptionalContextValues key value)

-- | Apply 'Update.setOptionalContextValues' to the isolation scope on the
-- given context.
setOptionalContextValuesAt :: (MonadIO m) => Context -> Text -> Maybe [(Text, Aeson.Value)] -> m ()
setOptionalContextValuesAt ctx key value = updateAt ctx (Update.setOptionalContextValues key value)

-- | Apply 'Update.setOptionalContextValue' to the given scope.
setOptionalContextValue :: (MonadIO m) => Scope -> Text -> Text -> Maybe Aeson.Value -> m ()
setOptionalContextValue scope key fieldKey value = Update.apply scope (Update.setOptionalContextValue key fieldKey value)

-- | Apply 'Update.setOptionalContextValue' to the isolation scope on the given
-- context.
setOptionalContextValueAt :: (MonadIO m) => Context -> Text -> Text -> Maybe Aeson.Value -> m ()
setOptionalContextValueAt ctx key fieldKey value = updateAt ctx (Update.setOptionalContextValue key fieldKey value)

-- | Apply 'Update.setOptionalOsContext' to the given scope.
setOptionalOsContext :: (MonadIO m) => Scope -> Maybe Sentry.OsContext.OsContext -> m ()
setOptionalOsContext scope value = Update.apply scope (Update.setOptionalOsContext value)

-- | Apply 'Update.setOptionalOsContext' to the isolation scope on the given
-- context.
setOptionalOsContextAt :: (MonadIO m) => Context -> Maybe Sentry.OsContext.OsContext -> m ()
setOptionalOsContextAt ctx value = updateAt ctx (Update.setOptionalOsContext value)

-- | Apply 'Update.setOptionalAppContext' to the given scope.
setOptionalAppContext :: (MonadIO m) => Scope -> Maybe Sentry.AppContext.AppContext -> m ()
setOptionalAppContext scope value = Update.apply scope (Update.setOptionalAppContext value)

-- | Apply 'Update.setOptionalAppContext' to the isolation scope on the given
-- context.
setOptionalAppContextAt :: (MonadIO m) => Context -> Maybe Sentry.AppContext.AppContext -> m ()
setOptionalAppContextAt ctx value = updateAt ctx (Update.setOptionalAppContext value)

-- | Apply 'Update.setOptionalBrowserContext' to the given scope.
setOptionalBrowserContext :: (MonadIO m) => Scope -> Maybe Sentry.BrowserContext.BrowserContext -> m ()
setOptionalBrowserContext scope value = Update.apply scope (Update.setOptionalBrowserContext value)

-- | Apply 'Update.setOptionalBrowserContext' to the isolation scope on the
-- given context.
setOptionalBrowserContextAt :: (MonadIO m) => Context -> Maybe Sentry.BrowserContext.BrowserContext -> m ()
setOptionalBrowserContextAt ctx value = updateAt ctx (Update.setOptionalBrowserContext value)

-- | Apply 'Update.setOptionalDeviceContext' to the given scope.
setOptionalDeviceContext :: (MonadIO m) => Scope -> Maybe Sentry.DeviceContext.DeviceContext -> m ()
setOptionalDeviceContext scope value = Update.apply scope (Update.setOptionalDeviceContext value)

-- | Apply 'Update.setOptionalDeviceContext' to the isolation scope on the given
-- context.
setOptionalDeviceContextAt :: (MonadIO m) => Context -> Maybe Sentry.DeviceContext.DeviceContext -> m ()
setOptionalDeviceContextAt ctx value = updateAt ctx (Update.setOptionalDeviceContext value)

-- | Apply 'Update.setOptionalTraceContext' to the given scope.
setOptionalTraceContext :: (MonadIO m) => Scope -> Maybe Sentry.TraceContext.TraceContext -> m ()
setOptionalTraceContext scope value = Update.apply scope (Update.setOptionalTraceContext value)

-- | Apply 'Update.setOptionalTraceContext' to the isolation scope on the given
-- context.
setOptionalTraceContextAt :: (MonadIO m) => Context -> Maybe Sentry.TraceContext.TraceContext -> m ()
setOptionalTraceContextAt ctx value = updateAt ctx (Update.setOptionalTraceContext value)

-- | Apply 'Update.alterAppContext' to the given scope.
alterAppContext :: (MonadIO m) => Scope -> (Maybe Sentry.AppContext.AppContext -> Maybe Sentry.AppContext.AppContext) -> m ()
alterAppContext scope f = Update.apply scope (Update.alterAppContext f)

-- | Apply 'Update.alterAppContext' to the isolation scope on the given context.
alterAppContextAt :: (MonadIO m) => Context -> (Maybe Sentry.AppContext.AppContext -> Maybe Sentry.AppContext.AppContext) -> m ()
alterAppContextAt ctx f = updateAt ctx (Update.alterAppContext f)

-- | Apply 'Update.alterOsContext' to the given scope.
alterOsContext :: (MonadIO m) => Scope -> (Maybe Sentry.OsContext.OsContext -> Maybe Sentry.OsContext.OsContext) -> m ()
alterOsContext scope f = Update.apply scope (Update.alterOsContext f)

-- | Apply 'Update.alterOsContext' to the isolation scope on the given context.
alterOsContextAt :: (MonadIO m) => Context -> (Maybe Sentry.OsContext.OsContext -> Maybe Sentry.OsContext.OsContext) -> m ()
alterOsContextAt ctx f = updateAt ctx (Update.alterOsContext f)

-- | Apply 'Update.alterRuntimeContext' to the given scope.
alterRuntimeContext :: (MonadIO m) => Scope -> (Maybe Sentry.RuntimeContext.RuntimeContext -> Maybe Sentry.RuntimeContext.RuntimeContext) -> m ()
alterRuntimeContext scope f = Update.apply scope (Update.alterRuntimeContext f)

-- | Apply 'Update.alterRuntimeContext' to the isolation scope on the given
-- context.
alterRuntimeContextAt :: (MonadIO m) => Context -> (Maybe Sentry.RuntimeContext.RuntimeContext -> Maybe Sentry.RuntimeContext.RuntimeContext) -> m ()
alterRuntimeContextAt ctx f = updateAt ctx (Update.alterRuntimeContext f)

-- | Apply 'Update.alterBrowserContext' to the given scope.
alterBrowserContext :: (MonadIO m) => Scope -> (Maybe Sentry.BrowserContext.BrowserContext -> Maybe Sentry.BrowserContext.BrowserContext) -> m ()
alterBrowserContext scope f = Update.apply scope (Update.alterBrowserContext f)

-- | Apply 'Update.alterBrowserContext' to the isolation scope on the given context.
alterBrowserContextAt :: (MonadIO m) => Context -> (Maybe Sentry.BrowserContext.BrowserContext -> Maybe Sentry.BrowserContext.BrowserContext) -> m ()
alterBrowserContextAt ctx f = updateAt ctx (Update.alterBrowserContext f)

-- | Apply 'Update.alterDeviceContext' to the given scope.
alterDeviceContext :: (MonadIO m) => Scope -> (Maybe Sentry.DeviceContext.DeviceContext -> Maybe Sentry.DeviceContext.DeviceContext) -> m ()
alterDeviceContext scope f = Update.apply scope (Update.alterDeviceContext f)

-- | Apply 'Update.alterDeviceContext' to the isolation scope on the given
-- context.
alterDeviceContextAt :: (MonadIO m) => Context -> (Maybe Sentry.DeviceContext.DeviceContext -> Maybe Sentry.DeviceContext.DeviceContext) -> m ()
alterDeviceContextAt ctx f = updateAt ctx (Update.alterDeviceContext f)

-- | Apply 'Update.alterTraceContext' to the given scope.
alterTraceContext :: (MonadIO m) => Scope -> (Maybe Sentry.TraceContext.TraceContext -> Maybe Sentry.TraceContext.TraceContext) -> m ()
alterTraceContext scope f = Update.apply scope (Update.alterTraceContext f)

-- | Apply 'Update.alterTraceContext' to the isolation scope on the given
-- context.
alterTraceContextAt :: (MonadIO m) => Context -> (Maybe Sentry.TraceContext.TraceContext -> Maybe Sentry.TraceContext.TraceContext) -> m ()
alterTraceContextAt ctx f = updateAt ctx (Update.alterTraceContext f)

-- | Apply 'Update.modifyExistingContextValue' to given scope.
modifyExistingContextValue :: (MonadIO m) => Scope -> Text -> Text -> (Aeson.Value -> Aeson.Value) -> m ()
modifyExistingContextValue scope key field f = Update.apply scope (Update.modifyExistingContextValue key field f)

-- | Apply 'Update.modifyExistingContextValue' to the isolation scope on the
-- given context.
modifyExistingContextValueAt :: (MonadIO m) => Context -> Text -> Text -> (Aeson.Value -> Aeson.Value) -> m ()
modifyExistingContextValueAt ctx key field f = updateAt ctx (Update.modifyExistingContextValue key field f)

-- | Apply 'Update.alterContextValue' to the given scope.
alterContextValue :: (MonadIO m) => Scope -> Text -> Text -> (Maybe Aeson.Value -> Maybe Aeson.Value) -> m ()
alterContextValue scope key field f = Update.apply scope (Update.alterContextValue key field f)

-- | Apply 'Update.alterContextValue' to the isolation scope on the given
-- context.
alterContextValueAt :: (MonadIO m) => Context -> Text -> Text -> (Maybe Aeson.Value -> Maybe Aeson.Value) -> m ()
alterContextValueAt ctx key field f = updateAt ctx (Update.alterContextValue key field f)

-- | Apply 'Update.filterFingerprint' to given scope.
filterFingerprint :: (MonadIO m) => Scope -> (Text -> Bool) -> m ()
filterFingerprint scope predicate = Update.apply scope (Update.filterFingerprint predicate)

-- | Apply 'Update.filterFingerprint' to the isolation scope on the given context.
filterFingerprintAt :: (MonadIO m) => Context -> (Text -> Bool) -> m ()
filterFingerprintAt ctx predicate = updateAt ctx (Update.filterFingerprint predicate)

-- | Apply 'Update.filterBreadcrumbs' to the given scope.
filterBreadcrumbs :: (MonadIO m) => Scope -> (Patrol.Breadcrumb -> Bool) -> m ()
filterBreadcrumbs scope predicate = Update.apply scope (Update.filterBreadcrumbs predicate)

-- | Apply 'Update.filterBreadcrumbs' to the isolation scope on the given context.
filterBreadcrumbsAt :: (MonadIO m) => Context -> (Patrol.Breadcrumb -> Bool) -> m ()
filterBreadcrumbsAt ctx predicate = updateAt ctx (Update.filterBreadcrumbs predicate)
