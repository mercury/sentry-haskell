-- | Internal plumbing for 'Sentry.Scope'.
--
-- /This module's API is unstable!/ "Sentry.Scope" and "Sentry.Scope.Update"
-- should be preferred.
module Sentry.Scope.Internal
  ( -- * Types
    Scope (..),
    ScopeType (..),
    ScopeData (..),
    defaultScopeData,
    newScope,
    readScopeData,

    -- * Privileged mutation
    modifyScopeData,
    bindUnmanaged,
    bindManaged,
    unbindManaged,

    -- * Process-global scope and thread-local override

    --
    -- $global-override
    processGlobal,
    globalKey,
    lookupGlobal,
    insertGlobal,
    removeGlobal,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson qualified as Aeson
import Data.Atomics (atomicModifyIORefCAS_)
import Data.Default (Default (def))
import Data.IORef (IORef, newIORef, readIORef)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Unique (Unique)
import Data.Vector (Vector)
import OpenTelemetry.Context (Context, Key)
import OpenTelemetry.Context qualified as Context
import Patrol qualified
import Sentry.Client (Client)
import Sentry.Event (CapturedEvent (..))
import System.IO.Unsafe (unsafePerformIO)

-- | A mutable reference to 'ScopeData' that lives on thread-local context.
--
-- Callers update the scope during execution (adding breadcrumbs, setting tags,
-- etc.).
--
-- When an exception is captured, the current value is read, merged with other
-- scope layers, and attached as an annotation.
type Scope :: Type
data Scope = Scope (IORef ScopeState)

-- Keep the effective client and its ownership in one atomic snapshot.
type ScopeState :: Type
data ScopeState = ScopeState
  { scopeData :: ScopeData,
    baseClient :: Maybe Client,
    bindings :: [(Unique, Client)]
  }

instance Show Scope where
  showsPrec d (Scope _) = showParen (d > 10) $ showString "Scope <<ioref>>"

-- | Identifies which layer of the scope hierarchy an 'ScopeData' belongs
-- to.
--
-- Scopes are merged (via their 'Semigroup' instance) when an exception is
-- captured, at which point the result is tagged 'Merged'.
type ScopeType :: Type
data ScopeType
  = Global
  | Isolation
  | Current
  | Merged
  deriving stock (Eq, Ord, Show)

-- | A pure, immutable snapshot of scope metadata. Values of this type are
-- stored behind the mutable 'IORef' inside a 'Scope' so they can be updated
-- incrementally.
type ScopeData :: Type
data ScopeData = ScopeData
  { type_ :: Maybe ScopeType,
    level :: Maybe Patrol.Level,
    fingerprint :: Maybe (Vector Text),
    transaction :: Maybe Text,
    breadcrumbs :: Seq Patrol.Breadcrumb,
    user :: Maybe Patrol.User,
    extras :: Map Text Aeson.Value,
    tags :: Map Text Text,
    contexts :: Map Text Patrol.Context,
    eventProcessor :: CapturedEvent -> Maybe Patrol.Event,
    client :: Maybe Client
  }

instance Show ScopeData where
  showsPrec p scope =
    showParen (p > 10) $
      showString "ScopeData {"
        . showString " type_ = "
        . shows scope.type_
        . showString ", level = "
        . shows scope.level
        . showString ", fingerprint = "
        . shows scope.fingerprint
        . showString ", transaction = "
        . shows scope.transaction
        . showString ", breadcrumbs = "
        . shows scope.breadcrumbs
        . showString ", user = "
        . shows scope.user
        . showString ", extras = "
        . shows scope.extras
        . showString ", tags = "
        . shows scope.tags
        . showString ", contexts = "
        . shows scope.contexts
        . showString ", eventProcessor = <<function>>"
        . showString ", client = <<client>>"
        . showString " }"

-- | Merge two scopes.
--
-- For scalar fields ('level', 'fingerprint', 'transaction',
-- 'user'), the right-hand (newer) value takes precedent over the left-hand
-- (older) value.
--
-- For collection fields ('breadcrumbs', 'extras', 'tags', 'contexts'), values
-- are combined.
--
-- Event processors are chained left-to-right (older processor runs first).
instance Semigroup ScopeData where
  old <> new =
    ScopeData
      { -- we are merging scopes, by definition, if `(<>)` is ever called.
        type_ = Just Merged,
        level = new.level <|> old.level,
        fingerprint = new.fingerprint <|> old.fingerprint,
        transaction = new.transaction <|> old.transaction,
        breadcrumbs = old.breadcrumbs <> new.breadcrumbs,
        user = new.user <|> old.user,
        extras = new.extras <> old.extras,
        tags = new.tags <> old.tags,
        contexts = new.contexts <> old.contexts,
        eventProcessor = \ce -> do
          event' <- old.eventProcessor ce
          new.eventProcessor ce{event = event'},
        client = new.client <|> old.client
      }

instance Monoid ScopeData where
  mempty = defaultScopeData

instance Default ScopeData where
  def = defaultScopeData

-- | An "empty" 'ScopeData' with no metadata and a pass-through event
-- processor.
defaultScopeData :: ScopeData
defaultScopeData =
  ScopeData
    { type_ = Nothing,
      level = Nothing,
      fingerprint = Nothing,
      transaction = Nothing,
      breadcrumbs = mempty,
      user = Nothing,
      extras = mempty,
      tags = mempty,
      contexts = mempty,
      eventProcessor = \ce -> Just ce.event,
      client = Nothing
    }

-- | Apply a pure modification to the 'ScopeData' inside a 'Scope'.
--
-- /Internal:/ Callers should use the field-specific setters from "Sentry.Scope"
-- or compose updates via "Sentry.Scope.Update"; writing ad-hoc
-- 'ScopeData -> ScopeData' modifiers should be avoided. The client field is
-- owned by the binding operations and cannot be changed by metadata updates.
modifyScopeData :: (MonadIO m) => Scope -> (ScopeData -> ScopeData) -> m ()
modifyScopeData (Scope ref) f = liftIO $ atomicModifyIORefCAS_ ref \s ->
  s{scopeData = (f s.scopeData){client = s.scopeData.client}}

-- | Create an independent, unmanaged scope from a metadata snapshot.
newScope :: ScopeData -> IO Scope
newScope scopeData = Scope <$> newIORef ScopeState{scopeData, baseClient = scopeData.client, bindings = []}

-- | Read the effective metadata, without exposing lifecycle ownership.
readScopeData :: Scope -> IO ScopeData
readScopeData (Scope ref) = (.scopeData) <$> readIORef ref

-- | Deliberately replace all managed bindings with an unmanaged binding.
bindUnmanaged :: Scope -> Maybe Client -> IO ()
bindUnmanaged (Scope ref) client = atomicModifyIORefCAS_ ref \s ->
  ScopeState{scopeData = s.scopeData{client}, baseClient = client, bindings = []}

-- | Install a binding with a token allocated by its owner before acquisition.
bindManaged :: Scope -> Unique -> Client -> IO ()
bindManaged (Scope ref) token client = atomicModifyIORefCAS_ ref \s ->
  s{scopeData = s.scopeData{client = Just client}, bindings = (token, client) : s.bindings}

-- | Remove an owned binding.  Stale releases are harmless.
unbindManaged :: Scope -> Unique -> IO ()
unbindManaged (Scope ref) token = atomicModifyIORefCAS_ ref \s ->
  let bindings = filter ((/= token) . fst) s.bindings
      client = case bindings of
        (_, newest) : _ -> Just newest
        [] -> s.baseClient
   in s{scopeData = s.scopeData{client}, bindings}

-- $global-override
--
-- 'processGlobal' is the true process-wide singleton.
--
-- It is intentionally not exported from "Sentry.Scope", with the intention
-- that callers should go through 'Sentry.Scope.getGlobal' which first checks
-- for a thread-local override installed by 'insertGlobal'.
--
-- This override mechanism exists primarily for test isolation (see
-- 'Sentry.Test'); in production, no override is installed, so every thread
-- resolves straight to 'processGlobal' with no overhead.

-- | The true process-wide global scope singleton.
--
-- All reads and writes should go through 'Sentry.Scope.getGlobalScope' rather
-- than using this reference directly.
processGlobal :: Scope
processGlobal = unsafePerformIO $ newScope (def{type_ = Just Global})
{-# NOINLINE processGlobal #-}

-- | Thread-local context key for a per-thread global scope override.
globalKey :: Key Scope
globalKey = unsafePerformIO $ Context.newKey "global_scope"
{-# NOINLINE globalKey #-}

-- | Retrieve the per-thread global scope override from the given 'Context', if
-- one has been installed.
lookupGlobal :: Context -> Maybe Scope
lookupGlobal = Context.lookup globalKey

-- | Install a per-thread global scope override on the given 'Context'.
insertGlobal :: Scope -> Context -> Context
insertGlobal = Context.insert globalKey

-- | Remove any per-thread global scope override from the given 'Context',
-- reverting global resolution to 'processGlobalScope'.
removeGlobal :: Context -> Context
removeGlobal = Context.delete globalKey
