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

    -- ** Thread-local Context Manipulation
    lookupCurrent,
    insertCurrent,
    removeCurrent,
    lookupIsolation,
    insertIsolation,
    removeIsolation,

    -- ** Global Scope
    global,

    -- ** Event Modification
    apply,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson qualified as Aeson
import Data.Default (def)
import Data.IORef (newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import OpenTelemetry.Context (Context, Key)
import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Context.ThreadLocal qualified as ThreadLocal
import Patrol qualified
import Patrol.Type.Event qualified as Patrol.Event
import Sentry.Scope.Internal (Scope (..), ScopeData (..), ScopeType (..), modifyScopeData)
import System.IO.Unsafe (unsafePerformIO)

-- | Read the current state of the 'ScopeData' contained within the given
-- 'Scope' reference.
readScopeRef :: (MonadIO m) => Scope -> m ScopeData
readScopeRef (Scope ref) = liftIO $ readIORef ref

-- * Scalar setters

-- | Set the 'Patrol.Type.Level.Level' for the given 'Scope'.
setLevel :: (MonadIO m) => Scope -> Patrol.Level -> m ()
setLevel scope level = modifyScopeData scope \s -> s{level = Just level}

-- | Clear the 'Patrol.Type.Level.Level' from the given 'Scope'.
unsetLevel :: (MonadIO m) => Scope -> m ()
unsetLevel scope = modifyScopeData scope \s -> s{level = Nothing}

-- | Set the 'Patrol.Type.User.User' for the given 'Scope'.
setUser :: (MonadIO m) => Scope -> Patrol.User -> m ()
setUser scope u = modifyScopeData scope \s -> s{user = Just u}

-- | Clear the 'Patrol.Type.User.User' from the given 'Scope'.
unsetUser :: (MonadIO m) => Scope -> m ()
unsetUser scope = modifyScopeData scope \s -> s{user = Nothing}

-- | Set the fingerprint for the given 'Scope'.
setFingerprint :: (MonadIO m) => Scope -> Vector Text -> m ()
setFingerprint scope fp = modifyScopeData scope \s -> s{fingerprint = Just fp}

-- | Clear the fingerprint from the given 'Scope'.
unsetFingerprint :: (MonadIO m) => Scope -> m ()
unsetFingerprint scope = modifyScopeData scope \s -> s{fingerprint = Nothing}

-- | Set the transaction name for the given 'Scope'.
setTransaction :: (MonadIO m) => Scope -> Text -> m ()
setTransaction scope t = modifyScopeData scope \s -> s{transaction = Just t}

-- | Clear the transaction name from the given 'Scope'.
unsetTransaction :: (MonadIO m) => Scope -> m ()
unsetTransaction scope = modifyScopeData scope \s -> s{transaction = Nothing}

-- * Tag setters

-- | Insert (or overwrite) a tag at the given key.
setTag :: (MonadIO m) => Scope -> Text -> Text -> m ()
setTag scope k v = modifyScopeData scope \s -> s{tags = Map.insert k v s.tags}

-- | Remove the tag at the given key, if present.
removeTag :: (MonadIO m) => Scope -> Text -> m ()
removeTag scope k = modifyScopeData scope \s -> s{tags = Map.delete k s.tags}

-- | Clear all tags from the given 'Scope'.
clearTags :: (MonadIO m) => Scope -> m ()
clearTags scope = modifyScopeData scope \s -> s{tags = Map.empty}

-- * Extra setters

-- | Insert (or overwrite) an extra value at the given key.
setExtra :: (MonadIO m) => Scope -> Text -> Aeson.Value -> m ()
setExtra scope k v = modifyScopeData scope \s -> s{extras = Map.insert k v s.extras}

-- | Remove the extra value at the given key, if present.
removeExtra :: (MonadIO m) => Scope -> Text -> m ()
removeExtra scope k = modifyScopeData scope \s -> s{extras = Map.delete k s.extras}

-- | Clear all extras from the given 'Scope'.
clearExtras :: (MonadIO m) => Scope -> m ()
clearExtras scope = modifyScopeData scope \s -> s{extras = Map.empty}

-- * Context setters

-- | Insert (or overwrite) a context at the given key.
setContext :: (MonadIO m) => Scope -> Text -> Patrol.Context -> m ()
setContext scope k v = modifyScopeData scope \s -> s{contexts = Map.insert k v s.contexts}

-- | Remove the context at the given key, if present.
removeContext :: (MonadIO m) => Scope -> Text -> m ()
removeContext scope k = modifyScopeData scope \s -> s{contexts = Map.delete k s.contexts}

-- | Clear all contexts from the given 'Scope'.
clearContexts :: (MonadIO m) => Scope -> m ()
clearContexts scope = modifyScopeData scope \s -> s{contexts = Map.empty}

-- | A globally accessible 'Scope' holding data that will be added to /all/
-- events sent by this process.
global :: Scope
global = Scope $ unsafePerformIO $ newIORef (def{type_ = Just Global})
{-# NOINLINE global #-}

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
-- 'global' \<> isolation (from thread-local) \<> current (from thread-local).
--
-- Missing layers contribute 'mempty'.
readAmbientScope :: (MonadIO m) => m ScopeData
readAmbientScope = liftIO do
  globalScope <- readScopeRef global
  context <- ThreadLocal.getContext
  isolationScope <- maybe (pure mempty) readScopeRef (lookupIsolation context)
  currentScope <- maybe (pure mempty) readScopeRef (lookupCurrent context)
  pure $ globalScope <> isolationScope <> currentScope

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
--
-- Returns 'Nothing' when the scope's 'eventProcessor' drops the event.
apply :: ScopeData -> Patrol.Event -> Maybe Patrol.Event
apply scope event = scope.eventProcessor merged
  where
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
          Patrol.Event.contexts = Map.union event.contexts scope.contexts
        }
