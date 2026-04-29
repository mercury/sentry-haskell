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

    -- * Privileged mutation
    modifyScopeData,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson qualified as Aeson
import Data.Atomics (atomicModifyIORefCAS_)
import Data.Default (Default (def))
import Data.IORef (IORef)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (Vector)
import Patrol qualified
import Sentry.CapturedEvent (CapturedEvent (..))

-- | A mutable reference to 'ScopeData' that lives on thread-local context.
--
-- Callers update the scope during execution (adding breadcrumbs, setting tags,
-- etc.).
--
-- When an exception is captured, the current value is read, merged with other
-- scope layers, and attached as an annotation.
type Scope :: Type
data Scope = Scope (IORef ScopeData)

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
    breadcrumbs :: Seq Text,
    user :: Maybe Patrol.User,
    extras :: Map Text Aeson.Value,
    tags :: Map Text Text,
    contexts :: Map Text Patrol.Context,
    eventProcessor :: CapturedEvent -> Maybe Patrol.Event
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
          new.eventProcessor ce{event = event'}
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
      eventProcessor = \ce -> Just ce.event
    }

-- | Apply a pure modification to the 'ScopeData' inside a 'Scope'.
--
-- /Internal:/ Callers should use the field-specific setters from "Sentry.Scope"
-- or compose updates via "Sentry.Scope.Update"; writing ad-hoc
-- 'ScopeData -> ScopeData' modifiers should be avoided.
modifyScopeData :: (MonadIO m) => Scope -> (ScopeData -> ScopeData) -> m ()
modifyScopeData (Scope ref) f = liftIO $ atomicModifyIORefCAS_ ref f
