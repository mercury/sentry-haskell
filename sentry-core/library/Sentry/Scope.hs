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

    -- ** Access,
    readScopeRef,

    -- ** Context Manipulation
    lookupCurrent,
    insertCurrent,
    removeCurrent,
    lookupIsolation,
    insertIsolation,
    removeIsolation,

    -- ** Global Scope
    global,
  ) where

import Control.Applicative ((<|>))
import Control.Exception.Annotated (Annotation (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson qualified as Aeson
import Data.Default (Default (def))
import Data.IORef (IORef, newIORef, readIORef)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (Vector)
import OpenTelemetry.Context (Context, Key)
import OpenTelemetry.Context qualified as Context
import Patrol qualified
import Sentry.Internal (BeforeCallback)
import System.IO.Unsafe (unsafePerformIO)

-- | A mutable reference to 'ScopeData' that lives on thread-local context.
--
-- Callers update the scope during execution (adding breadcrumbs, setting tags,
-- etc.). When an exception is captured, the current value is read, merged with
-- other scope layers, and attached as an annotation.
type Scope :: Type
data Scope = Scope (IORef ScopeData)

instance Show Scope where
  showsPrec d (Scope _) = showParen (d > 10) $ showString "Scope <<ioref>>"

-- | Read the current state of the 'ScopeData' contained within the given
-- 'Scope' reference.
readScopeRef :: (MonadIO m) => Scope -> m ScopeData
readScopeRef (Scope ref) = liftIO $ readIORef ref

-- | Identifies which layer of the scope hierarchy an 'ScopeData' belongs
-- to. Scopes are merged (via their 'Semigroup' instance) when an exception is
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
    breadcrumbs :: Seq Text,
    user :: Maybe Patrol.User,
    extras :: Map Text Aeson.Value,
    tags :: Map Text Text,
    contexts :: Map Text Patrol.Context,
    -- | Pipeline of callbacks that can modify or suppress an event before it is
    -- sent. Returning 'Nothing' drops the event entirely.
    eventProcessor :: BeforeCallback Patrol.Event
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
        . showString " }"

-- | Merge two scopes. For scalar fields ('level', 'fingerprint', 'transaction',
-- 'user') the right-hand (newer) value wins. For collection fields
-- ('breadcrumbs', 'extras', 'tags', 'contexts') values are combined. Event
-- processors are chained left-to-right (older processor runs first).
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
        eventProcessor = \event -> do
          event' <- old.eventProcessor event
          new.eventProcessor event'
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
      eventProcessor = Just
    }

-- | A globally accessible 'Scope' holding data that will be added to /all/
-- events sent by this process.
--
-- Implemented via 'unsafePerformIO' (the standard top-level mutable state
-- pattern); the @NOINLINE@ pragma ensures the 'IORef' is allocated exactly
-- once.
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
