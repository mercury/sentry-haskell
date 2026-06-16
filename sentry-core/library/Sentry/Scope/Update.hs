{-# LANGUAGE DerivingVia #-}

-- | Apply updates to 'Sentry.Scope.Internal.ScopeData'; may be used either in
-- terms of the 'Semigroup' instance for 'ScopeUpdate' methods or via 'QualifiedDo':
--
-- @
-- import Sentry.Scope.Update qualified as Update
--
-- -- Monoid composition
-- Update.apply scope $
--   Update.setLevel Warning
--     <> Update.setTag \"env\" \"prod\"
--     <> Update.setUser u
-- @
--
-- @
-- {-\# LANGUAGE QualifiedDo \#-}
-- import Sentry.Scope.Update qualified as Update
--
-- Update.apply scope Update.do
--   Update.setLevel Warning
--   Update.setTag \"env\" \"prod\"
--   Update.setUser u
-- @
--
-- 'ScopeData' edits bundles can also be factored out and reused:
--
-- @
-- let stagingTags = Update.setTag \"env\" \"staging\" <> Update.setTag \"tier\" \"free\"
-- scope `Update.apply` (stagingTags <> Update.setUser u)
-- @
module Sentry.Scope.Update
  ( -- * Type
    ScopeUpdate,

    -- * Application
    apply,

    -- * Smart constructors

    -- ** Scalar fields
    setLevel,
    unsetLevel,
    setUser,
    unsetUser,
    setFingerprint,
    unsetFingerprint,
    setTransaction,
    unsetTransaction,

    -- ** Tags
    setTag,
    removeTag,
    clearTags,

    -- ** Extras
    setExtra,
    removeExtra,
    clearExtras,

    -- ** Contexts
    setContext,
    removeContext,
    clearContexts,

    -- * QualifiedDo support
    (>>),
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson qualified as Aeson
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Monoid (Dual (..), Endo (..))
import Data.Text (Text)
import Data.Vector (Vector)
import Patrol qualified
import Sentry.Scope.Internal (Scope, ScopeData (..), modifyScopeData)
import Prelude hiding ((>>))

-- | A pending modification to a 'Scope'.
--
-- Updates can be chained with '<>' or via `QualifiedDo`.
type ScopeUpdate :: Type
newtype ScopeUpdate = ScopeUpdate (ScopeData -> ScopeData)
  deriving (Semigroup, Monoid) via (Dual (Endo ScopeData))

-- | Apply a 'ScopeUpdate' to a 'Scope' as a single atomic 'IORef' update.
apply :: (MonadIO m) => Scope -> ScopeUpdate -> m ()
apply scope (ScopeUpdate f) = modifyScopeData scope f

-- | Wrap a pure 'ScopeData' modification in a 'ScopeUpdate'. Internal helper
-- used by every smart constructor; not exported so the public surface stays
-- the curated set of named operations.
edit :: (ScopeData -> ScopeData) -> ScopeUpdate
edit = ScopeUpdate

-- | 'QualifiedDo' hook. The body of an @Update.do@ block desugars
-- @Update.do { a; b }@ to @a Update.>> b@; we point that at '(<>)' so the
-- block accumulates updates without introducing a real monad.
(>>) :: ScopeUpdate -> ScopeUpdate -> ScopeUpdate
(>>) = (<>)

-- * Scalar field updates

-- | Set the 'Patrol.Type.Level.Level'.
setLevel :: Patrol.Level -> ScopeUpdate
setLevel l = edit \s -> s{level = Just l}

-- | Clear the 'Patrol.Type.Level.Level'.
unsetLevel :: ScopeUpdate
unsetLevel = edit \s -> s{level = Nothing}

-- | Set the 'Patrol.Type.User.User'.
setUser :: Patrol.User -> ScopeUpdate
setUser u = edit \s -> s{user = Just u}

-- | Clear the 'Patrol.Type.User.User'.
unsetUser :: ScopeUpdate
unsetUser = edit \s -> s{user = Nothing}

-- | Set the fingerprint.
setFingerprint :: Vector Text -> ScopeUpdate
setFingerprint fp = edit \s -> s{fingerprint = Just fp}

-- | Clear the fingerprint.
unsetFingerprint :: ScopeUpdate
unsetFingerprint = edit \s -> s{fingerprint = Nothing}

-- | Set the transaction name.
setTransaction :: Text -> ScopeUpdate
setTransaction t = edit \s -> s{transaction = Just t}

-- | Clear the transaction name.
unsetTransaction :: ScopeUpdate
unsetTransaction = edit \s -> s{transaction = Nothing}

-- * Tag updates

-- | Insert (or overwrite) a tag at the given key.
setTag :: Text -> Text -> ScopeUpdate
setTag k v = edit \s -> s{tags = Map.insert k v s.tags}

-- | Remove the tag at the given key, if present.
removeTag :: Text -> ScopeUpdate
removeTag k = edit \s -> s{tags = Map.delete k s.tags}

-- | Clear all tags.
clearTags :: ScopeUpdate
clearTags = edit \s -> s{tags = Map.empty}

-- * Extra updates

-- | Insert (or overwrite) an extra value at the given key.
setExtra :: Text -> Aeson.Value -> ScopeUpdate
setExtra k v = edit \s -> s{extras = Map.insert k v s.extras}

-- | Remove the extra value at the given key, if present.
removeExtra :: Text -> ScopeUpdate
removeExtra k = edit \s -> s{extras = Map.delete k s.extras}

-- | Clear all extras.
clearExtras :: ScopeUpdate
clearExtras = edit \s -> s{extras = Map.empty}

-- * Context updates

-- | Insert (or overwrite) a context at the given key.
setContext :: Text -> Patrol.Context -> ScopeUpdate
setContext k v = edit \s -> s{contexts = Map.insert k v s.contexts}

-- | Remove the context at the given key, if present.
removeContext :: Text -> ScopeUpdate
removeContext k = edit \s -> s{contexts = Map.delete k s.contexts}

-- | Clear all contexts.
clearContexts :: ScopeUpdate
clearContexts = edit \s -> s{contexts = Map.empty}
