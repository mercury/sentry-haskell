{-# LANGUAGE ViewPatterns #-}

-- | Thread-local scope management for enriching Sentry events with contextual
-- metadata (tags, breadcrumbs, user info, etc.).
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

    -- ** Client resolution
    resolveClient,
    lookupClient,
    bindClient,

    -- ** Mutation

    -- *** Scalar fields
    setLevel,
    unsetLevel,
    setUser,
    unsetUser,
    setFingerprint,
    unsetFingerprint,
    setTransaction,
    unsetTransaction,

    -- *** Tags
    setTag,
    removeTag,
    clearTags,

    -- *** Extras
    setExtra,
    removeExtra,
    clearExtras,

    -- *** Contexts
    setContext,
    removeContext,
    clearContexts,

    -- *** Breadcrumbs
    addBreadcrumb,
    addBreadcrumbs,
    clearBreadcrumbs,

    -- ** Thread-local Context Manipulation
    lookupCurrent,
    insertCurrent,
    removeCurrent,
    lookupIsolation,
    insertIsolation,
    removeIsolation,

    -- ** Global Scope
    getGlobal,
    configureGlobal,

    -- ** Event Processor
    setEventProcessor,
    addEventProcessor,
    unsetEventProcessor,

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
readAmbientScope :: (MonadIO m) => m ScopeData
readAmbientScope = liftIO do
  context <- ThreadLocal.getContext
  let globalScopeRef = fromMaybe Internal.processGlobal (Internal.lookupGlobal context)
  globalScope <- readScopeRef globalScopeRef
  isolationScope <- maybe (pure mempty) readScopeRef (lookupIsolation context)
  currentScope <- maybe (pure mempty) readScopeRef (lookupCurrent context)
  pure $ globalScope <> isolationScope <> currentScope

-- | Resolve the 'Client' visible at the call site by walking the scope chain
-- (current \> isolation \> global, most-specific wins), falling back to
-- 'NON_RECORDING_CLIENT' when no layer has a client bound.
--
-- This reuses the same right-biased merge as 'readAmbientScope', so the client
-- is resolved exactly like every other piece of scope data.
resolveClient :: (MonadIO m) => m Client
resolveClient = fromMaybe NON_RECORDING_CLIENT . (.client) <$> readAmbientScope

-- | Like 'resolveClient', but reports the absence of a bound client as
-- 'Nothing' rather than substituting 'NON_RECORDING_CLIENT'.
lookupClient :: (MonadIO m) => m (Maybe Client)
lookupClient = (.client) <$> readAmbientScope

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
addBreadcrumb :: (MonadIO m) => Patrol.Breadcrumb -> m ()
addBreadcrumb crumb = do
  client <- resolveClient
  liftIO do
    ctx <- ThreadLocal.getContext
    for_ (lookupIsolation ctx) \scope ->
      addBreadcrumbToScope client.options scope crumb

-- | Add multiple 'Patrol.Breadcrumb's to the active isolation scope in order.
--
-- Equivalent to calling 'addBreadcrumb' on each element; each crumb is
-- independently filtered and trimmed.
addBreadcrumbs :: (MonadIO m) => [Patrol.Breadcrumb] -> m ()
addBreadcrumbs crumbs = do
  client <- resolveClient
  liftIO do
    ctx <- ThreadLocal.getContext
    for_ (lookupIsolation ctx) \scope ->
      for_ crumbs (addBreadcrumbToScope client.options scope)

-- | Clear all breadcrumbs from the given 'Scope'.
clearBreadcrumbs :: (MonadIO m) => Scope -> m ()
clearBreadcrumbs scope = Update.apply scope Update.clearBreadcrumbs

-- | Enforce 'ClientOptions.beforeBreadcrumb' and trim to
-- 'ClientOptions.maxBreadcrumbs', then append to the scope.
--
-- The append + trim mutation is expressed via "Sentry.Scope.Update"; only the
-- effectful policy (timestamp defaulting, 'beforeBreadcrumb', the bound) lives
-- here.
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
