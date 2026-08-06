{-# LANGUAGE ViewPatterns #-}

-- | Thread-local scope management for enriching Sentry events with contextual
-- metadata like tags, breadcrumbs, and user info.
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
-- Every mutation is available in three calling conventions:
--
-- * __'Scope'-first__ (e.g. 'setTag'): takes an explicit 'Scope' handle,
--   typically one bound by 'Sentry.Scope.IO.withScope' or
--   'Sentry.Scope.IO.withIsolationScope'.
-- * __Ambient__ (e.g. 'addBreadcrumb'): resolves a 'Scope' from the
--   thread-local 'Context' via 'OpenTelemetry.Context.ThreadLocal.getContext'.
-- * __'Context'-first__ (e.g. 'setTagAt', 'addBreadcrumbAt'):
--   resolves a 'Scope' from an explicitly supplied 'Context' instead of the
--   thread-local one; see @$context-first@ below for who needs this and its
--   caveats.
module Sentry.Scope
  ( -- * Scope

    -- ** Definition
    Scope,
    ScopeType (..),
    ScopeData (..),

    -- ** Construction
    create,
    clone,

    -- ** Access
    readScopeRef,
    readAmbientScope,
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
    setUserAt,
    unsetUser,
    unsetUserAt,
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
    apply,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson qualified as Aeson
import Data.Default (def)
import Data.Foldable (for_, toList)
import Data.IORef (newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import OpenTelemetry.Context (Context, Key)
import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Context.ThreadLocal qualified as ThreadLocal
import Patrol qualified
import Patrol.Type.Breadcrumb qualified as Patrol.Breadcrumb
import Patrol.Type.BreadcrumbType qualified as Patrol.BreadcrumbType
import Patrol.Type.Breadcrumbs qualified as Patrol.Breadcrumbs
import Patrol.Type.Event qualified as Patrol.Event
import Sentry.Client (Client (..), pattern NON_RECORDING_CLIENT)
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Event (CapturedEvent (..))
import Sentry.Scope.Internal (Scope (..), ScopeData (..), ScopeType (..), modifyScopeData)
import Sentry.Scope.Internal qualified as Internal
import Sentry.Scope.Update qualified as Update
import System.IO.Unsafe (unsafePerformIO)

-- | Read the current state of the 'ScopeData' contained within the given
-- 'Scope' reference.
readScopeRef :: (MonadIO m) => Scope -> m ScopeData
readScopeRef (Scope ref) = liftIO $ readIORef ref

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

-- | Set the 'Patrol.Type.User.User' for the given 'Scope'.
setUser :: (MonadIO m) => Scope -> Patrol.User -> m ()
setUser scope u = Update.apply scope (Update.setUser u)

-- | Clear the 'Patrol.Type.User.User' from the given 'Scope'.
unsetUser :: (MonadIO m) => Scope -> m ()
unsetUser scope = Update.apply scope Update.unsetUser

-- | Set the fingerprint for the given 'Scope'.
setFingerprint :: (MonadIO m) => Scope -> Vector Text -> m ()
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
-- configureGlobal \\scope -> Sentry.Scope.setTag scope "release" version
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
create (Just -> type_) = Scope <$> (liftIO $ newIORef (def{type_}))

-- | Return an independent copy of the given 'Scope'. Mutations to the clone
-- do not affect the original, and vice versa.
clone :: (MonadIO m) => Scope -> m Scope
clone (Scope ref) = liftIO do
  scope <- readIORef ref
  Scope <$> newIORef scope

-- | Read the merged ambient 'ScopeData' visible at the call site:
-- global (process singleton or thread-local override) \<> isolation (from
-- thread-local) \<> current (from thread-local).
--
-- Missing layers contribute 'mempty'.
--
-- Like 'readScopeAt', but reads the thread-local 'Context' instead of taking
-- one explicitly.
readAmbientScope :: (MonadIO m) => m ScopeData
readAmbientScope = liftIO ThreadLocal.getContext >>= readScopeAt

-- | Resolve the 'Client' visible at the call site by walking the scope chain
-- (current \> isolation \> global, most-specific wins), falling back to
-- 'NON_RECORDING_CLIENT' when no layer has a client bound.
--
-- This reuses the same right-biased merge as 'readAmbientScope', so the client
-- is resolved exactly like every other piece of scope data.
resolveClient :: (MonadIO m) => m Client
resolveClient = liftIO ThreadLocal.getContext >>= resolveClientAt

-- | Like 'resolveClient', but reports the absence of a bound client as
-- 'Nothing' rather than substituting 'NON_RECORDING_CLIENT'.
lookupClient :: (MonadIO m) => m (Maybe Client)
lookupClient = liftIO ThreadLocal.getContext >>= lookupClientAt

-- | Bind (or clear, with 'Nothing') the 'Client' on a specific scope layer.
--
-- Used by 'Sentry.Init.init' to set the process-wide client on the 'global'
-- scope, and by 'Sentry.Scope.IO.withClient' to bind a client onto the
-- isolation scope for a dynamic extent.
bindClient :: (MonadIO m) => Maybe Client -> Scope -> m ()
bindClient mc scope = modifyScopeData scope \s -> s{client = mc}

-- * Event processor setters

-- | Replace the 'Scope's event processor with the given function.
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

-- | Apply a 'ScopeData' snapshot to a 'Patrol.Event', then run the scope's
-- 'eventProcessor' as the final step.
--
-- Field semantics:
--
-- * Scalar fields ('level', 'fingerprint', 'transaction', 'user'): scope wins
--   when it carries a value, otherwise the event's value is preserved. Scope
--   is treated as authoritative because it represents the most-specific
--   contextual state (whether ambient or attached as an annotation), and
--   because event constructors like 'Patrol.Type.Event.fromSomeException'
--   prefill defaults that scope is expected to override.
-- * Collection fields ('tags', 'extras', 'contexts'): unioned, with the
--   event's keys overriding the scope's on conflicts.
-- * 'breadcrumbs': the event's own breadcrumbs come first; scope breadcrumbs
--   are appended. The field is omitted entirely when both sides are empty.
--
-- Returns 'Nothing' when the scope's 'eventProcessor' drops the event.
apply :: ScopeData -> CapturedEvent -> Maybe Patrol.Event
apply scope ce = scope.eventProcessor ce{event = merged}
  where
    event = ce.event
    merged =
      event
        { Patrol.Event.level = scope.level <|> event.level,
          Patrol.Event.fingerprint = case scope.fingerprint of
            Just fp -> Vector.toList fp
            Nothing -> event.fingerprint,
          Patrol.Event.transaction = fromMaybe event.transaction scope.transaction,
          Patrol.Event.user = scope.user <|> event.user,
          Patrol.Event.tags = Map.union event.tags scope.tags,
          Patrol.Event.extra = Map.union event.extra scope.extras,
          Patrol.Event.contexts = Map.union event.contexts scope.contexts,
          Patrol.Event.breadcrumbs =
            let crumbs =
                  foldMap Patrol.Breadcrumbs.values event.breadcrumbs
                    <> toList scope.breadcrumbs
             in Patrol.Breadcrumbs.Breadcrumbs crumbs <$ guard (not $ null crumbs)
        }

-- * Breadcrumb writers

-- | Add a 'Patrol.Breadcrumb' to the active isolation scope.
--
-- Defaults 'Patrol.Type.Breadcrumb.timestamp' to 'getCurrentTime' and
-- 'Patrol.Type.Breadcrumb.type_' to 'Patrol.Type.BreadcrumbType.Default' when
-- absent, enforces 'Sentry.Client.Options.ClientOptions.beforeBreadcrumb', and
-- trims the oldest entries to stay within
-- 'Sentry.Client.Options.ClientOptions.maxBreadcrumbs'.
--
-- No-ops when no isolation scope is active.
--
-- Like 'addBreadcrumbAt', but reads the thread-local 'Context' instead of
-- taking one explicitly.
addBreadcrumb :: (MonadIO m) => Patrol.Breadcrumb -> m ()
addBreadcrumb crumb = liftIO ThreadLocal.getContext >>= \ctx -> addBreadcrumbAt ctx crumb

-- | Add multiple 'Patrol.Breadcrumb's to the active isolation scope in order.
--
-- Equivalent to calling 'addBreadcrumb' on each element; each crumb is
-- independently filtered and trimmed.
--
-- Like 'addBreadcrumbsAt', but reads the thread-local 'Context' instead of
-- taking one explicitly.
addBreadcrumbs :: (MonadIO m) => [Patrol.Breadcrumb] -> m ()
addBreadcrumbs crumbs = liftIO ThreadLocal.getContext >>= \ctx -> addBreadcrumbsAt ctx crumbs

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
-- for /that/ context rather than the one attached to they thread they are
-- executing upon.
--
-- Two caveats:
--
-- 1. The 'Context' a hook receives is not guaranteed to be the ambient one
-- 2. A 'Context' that never passed through 'Sentry.Scope.IO.withIsolationScope'
--    resolves to no scope, and every mutator below silently no-ops.

-- | Resolve the scope that general mutations target: 'Current', falling back
-- to 'Isolation'.
resolveMutationScope :: Context -> Maybe Scope
resolveMutationScope ctx = lookupCurrent ctx <|> lookupIsolation ctx

-- | Resolve the scope that breadcrumbs target: 'Isolation' only.
resolveBreadcrumbScope :: Context -> Maybe Scope
resolveBreadcrumbScope = lookupIsolation

-- | Apply a 'Update.ScopeUpdate' to the scope resolved by
-- 'resolveMutationScope' on the given 'Context'.
updateAt :: (MonadIO m) => Context -> Update.ScopeUpdate -> m ()
updateAt ctx upd = for_ (resolveMutationScope ctx) \scope -> Update.apply scope upd

-- | Like 'readAmbientScope', but reads the given 'Context' instead of the
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
resolveClientAt context = fromMaybe NON_RECORDING_CLIENT . (.client) <$> readScopeAt context

-- | Like 'lookupClient', but reads the given 'Context' instead of the
-- thread-local one.
lookupClientAt :: (MonadIO m) => Context -> m (Maybe Client)
lookupClientAt context = (.client) <$> readScopeAt context

-- | Like 'setLevel', but targets 'resolveMutationScope' on the given 'Context'.
setLevelAt :: (MonadIO m) => Context -> Patrol.Level -> m ()
setLevelAt ctx level = updateAt ctx (Update.setLevel level)

-- | Like 'unsetLevel', but targets 'resolveMutationScope' on the given 'Context'.
unsetLevelAt :: (MonadIO m) => Context -> m ()
unsetLevelAt ctx = updateAt ctx Update.unsetLevel

-- | Like 'setUser', but targets 'resolveMutationScope' on the given 'Context'.
setUserAt :: (MonadIO m) => Context -> Patrol.User -> m ()
setUserAt ctx u = updateAt ctx (Update.setUser u)

-- | Like 'unsetUser', but targets 'resolveMutationScope' on the given 'Context'.
unsetUserAt :: (MonadIO m) => Context -> m ()
unsetUserAt ctx = updateAt ctx Update.unsetUser

-- | Like 'setFingerprint', but targets 'resolveMutationScope' on the given
-- 'Context'.
setFingerprintAt :: (MonadIO m) => Context -> Vector Text -> m ()
setFingerprintAt ctx fp = updateAt ctx (Update.setFingerprint fp)

-- | Like 'unsetFingerprint', but targets 'resolveMutationScope' on the given
-- 'Context'.
unsetFingerprintAt :: (MonadIO m) => Context -> m ()
unsetFingerprintAt ctx = updateAt ctx Update.unsetFingerprint

-- | Like 'setTransaction', but targets 'resolveMutationScope' on the given
-- 'Context'.
setTransactionAt :: (MonadIO m) => Context -> Text -> m ()
setTransactionAt ctx t = updateAt ctx (Update.setTransaction t)

-- | Like 'unsetTransaction', but targets 'resolveMutationScope' on the given
-- 'Context'.
unsetTransactionAt :: (MonadIO m) => Context -> m ()
unsetTransactionAt ctx = updateAt ctx Update.unsetTransaction

-- | Like 'setTag', but targets 'resolveMutationScope' on the given 'Context'.
setTagAt :: (MonadIO m) => Context -> Text -> Text -> m ()
setTagAt ctx k v = updateAt ctx (Update.setTag k v)

-- | Like 'removeTag', but targets 'resolveMutationScope' on the given
-- 'Context'.
removeTagAt :: (MonadIO m) => Context -> Text -> m ()
removeTagAt ctx k = updateAt ctx (Update.removeTag k)

-- | Like 'clearTags', but targets 'resolveMutationScope' on the given
-- 'Context'.
clearTagsAt :: (MonadIO m) => Context -> m ()
clearTagsAt ctx = updateAt ctx Update.clearTags

-- | Like 'setExtra', but targets 'resolveMutationScope' on the given
-- 'Context'.
setExtraAt :: (MonadIO m) => Context -> Text -> Aeson.Value -> m ()
setExtraAt ctx k v = updateAt ctx (Update.setExtra k v)

-- | Like 'removeExtra', but targets 'resolveMutationScope' on the given
-- 'Context'.
removeExtraAt :: (MonadIO m) => Context -> Text -> m ()
removeExtraAt ctx k = updateAt ctx (Update.removeExtra k)

-- | Like 'clearExtras', but targets 'resolveMutationScope' on the given
-- 'Context'.
clearExtrasAt :: (MonadIO m) => Context -> m ()
clearExtrasAt ctx = updateAt ctx Update.clearExtras

-- | Like 'setContext', but targets 'resolveMutationScope' on the given
-- 'Context'.
setContextAt :: (MonadIO m) => Context -> Text -> Patrol.Context -> m ()
setContextAt ctx k v = updateAt ctx (Update.setContext k v)

-- | Like 'removeContext', but targets 'resolveMutationScope' on the given
-- 'Context'.
removeContextAt :: (MonadIO m) => Context -> Text -> m ()
removeContextAt ctx k = updateAt ctx (Update.removeContext k)

-- | Like 'clearContexts', but targets 'resolveMutationScope' on the given
-- 'Context'.
clearContextsAt :: (MonadIO m) => Context -> m ()
clearContextsAt ctx = updateAt ctx Update.clearContexts

-- | Like 'setEventProcessor', but targets 'resolveMutationScope' on the given
-- 'Context'.
setEventProcessorAt :: (MonadIO m) => Context -> (CapturedEvent -> Maybe Patrol.Event) -> m ()
setEventProcessorAt ctx f = updateAt ctx (Update.setEventProcessor f)

-- | Like 'addEventProcessor', but targets 'resolveMutationScope' on the given
-- 'Context'.
addEventProcessorAt :: (MonadIO m) => Context -> (CapturedEvent -> Maybe Patrol.Event) -> m ()
addEventProcessorAt ctx g = updateAt ctx (Update.addEventProcessor g)

-- | Like 'unsetEventProcessor', but targets 'resolveMutationScope' on the
-- given 'Context'.
unsetEventProcessorAt :: (MonadIO m) => Context -> m ()
unsetEventProcessorAt ctx = updateAt ctx Update.unsetEventProcessor

-- | Like 'addBreadcrumb', but takes the 'Context' explicitly instead of
-- reading the thread-local one.
--
-- Targets 'resolveBreadcrumbScope' (isolation only) — no-ops when no
-- isolation scope is active on the given 'Context'.
addBreadcrumbAt :: (MonadIO m) => Context -> Patrol.Breadcrumb -> m ()
addBreadcrumbAt ctx crumb = do
  client <- resolveClientAt ctx
  for_ (resolveBreadcrumbScope ctx) \scope ->
    addBreadcrumbToScope client.options scope crumb

-- | Like 'addBreadcrumbs', but takes the 'Context' explicitly instead of
-- reading the thread-local one.
addBreadcrumbsAt :: (MonadIO m) => Context -> [Patrol.Breadcrumb] -> m ()
addBreadcrumbsAt ctx crumbs = do
  client <- resolveClientAt ctx
  for_ (resolveBreadcrumbScope ctx) \scope ->
    for_ crumbs (addBreadcrumbToScope client.options scope)

-- | Like 'clearBreadcrumbs', but resolves the target 'Scope' from the given
-- 'Context' via 'resolveBreadcrumbScope' (isolation only) instead of taking
-- one explicitly.
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
    Just crumb3 -> Update.apply scope (Update.addBreadcrumb crumb3 <> Update.trimBreadcrumbs maxN)
