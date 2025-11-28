{-# LANGUAGE ViewPatterns #-}

module Sentry.Scope
  ( -- * Scope

    -- ** Construction
    create,
    clone,

    -- ** Context Manipulation
    lookupCurrent,
    insertCurrent,
    removeCurrent,
    lookupIsolation,
    insertIsolation,
    removeIsolation,

    -- ** Global Scope
    global,

    -- ** Types
    Scope,
    ScopeType (..),
    ImmutableScope (..),

    -- ** Internal
    unsafeReadScopeRef,
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

-- | TODO: Documentation.
type Scope :: Type
data Scope = Scope (IORef ImmutableScope)

-- | TODO: Documentation.
unsafeReadScopeRef :: (MonadIO m) => Scope -> m ImmutableScope
unsafeReadScopeRef (Scope ref) = liftIO $ readIORef ref

instance Show Scope where
  showsPrec d (Scope _) = showParen (d > 10) $ showString "Scope <<ioref>>"

-- | TODO: Documentation.
type ScopeType :: Type
data ScopeType
  = Global
  | Isolation
  | Current
  | Merged
  deriving stock (Eq, Ord, Show)

-- | TODO: Documentation.
type ImmutableScope :: Type
data ImmutableScope = ImmutableScope
  { type_ :: Maybe ScopeType,
    level :: Maybe Patrol.Level,
    fingerprint :: Maybe (Vector Text),
    transaction :: Maybe Text,
    breadcrumbs :: Seq Text,
    user :: Maybe Patrol.User,
    extras :: Map Text Aeson.Value,
    tags :: Map Text Text,
    contexts :: Map Text Patrol.Context,
    eventProcessor :: BeforeCallback Patrol.Event
  }

instance Show ImmutableScope where
  showsPrec p scope =
    showParen (p > 10) $
      showString "ImmutableScope {"
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

instance Semigroup ImmutableScope where
  old <> new =
    ImmutableScope
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

instance Default ImmutableScope where
  def = defaultImmutableScope

-- | TODO: Documentation
defaultImmutableScope :: ImmutableScope
defaultImmutableScope =
  ImmutableScope
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

-- | A globally accessible 'Scope' holding data that will be added to *all*
-- 'Patrol.Type.Event.Event's sent by this process.
global :: Scope
global = Scope $ unsafePerformIO $ newIORef (def{type_ = Just Global})
{-# NOINLINE global #-}

-- | TODO: Documentation
currentScopeKey :: Key Scope
currentScopeKey = unsafePerformIO $ Context.newKey "current_scope"
{-# NOINLINE currentScopeKey #-}

-- | TODO: Documentation
lookupCurrent :: Context -> Maybe Scope
lookupCurrent = Context.lookup currentScopeKey

-- | TODO: Documentation
insertCurrent :: Scope -> Context -> Context
insertCurrent = Context.insert currentScopeKey

-- | TODO: Documentation
removeCurrent :: Context -> Context
removeCurrent = Context.delete currentScopeKey

-- | TODO: Documentation
isolationScopeKey :: Key Scope
isolationScopeKey = unsafePerformIO $ Context.newKey "isolation_scope"
{-# NOINLINE isolationScopeKey #-}

-- | TODO: Documentation
lookupIsolation :: Context -> Maybe Scope
lookupIsolation = Context.lookup isolationScopeKey

-- | TODO: Documentation
insertIsolation :: Scope -> Context -> Context
insertIsolation = Context.insert isolationScopeKey

-- | TODO: Documentation
removeIsolation :: Context -> Context
removeIsolation = Context.delete isolationScopeKey

-- | TODO: Documentation
create :: (MonadIO m) => ScopeType -> m Scope
create (Just -> type_) = Scope <$> (liftIO $ newIORef (def{type_}))

-- | TODO: Documentation
clone :: (MonadIO m) => Scope -> m Scope
clone (Scope ref) = liftIO do
  scope <- readIORef ref
  Scope <$> newIORef scope
