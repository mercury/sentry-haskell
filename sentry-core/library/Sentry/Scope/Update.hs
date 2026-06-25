{-# LANGUAGE DerivingVia #-}

-- | Apply updates to 'Sentry.Scope.Internal.ScopeData' via the composable
-- 'ScopeUpdate' value.
--
-- 'ScopeUpdate's are built from the named smart constructors below and combined
-- with '<>'; applying one to a 'Scope' is a single atomic update. The left
-- operand of '<>' is applied first, so later updates win on conflicting scalar
-- fields:
--
-- @
-- import Sentry.Scope.Update qualified as Update
--
-- Update.apply scope $
--   Update.setLevel Warning
--     <> Update.setTag \"env\" \"prod\"
--     <> Update.setUser u
-- @
--
-- Update bundles can be factored out and reused:
--
-- @
-- let stagingTags = Update.setTag \"env\" \"staging\" <> Update.setTag \"tier\" \"free\"
-- scope `Update.apply` (stagingTags <> Update.setUser u)
-- @
module Sentry.Scope.Update
  ( -- * Type
    ScopeUpdate (..),

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

    -- ** Breadcrumbs
    addBreadcrumb,
    addBreadcrumbs,
    clearBreadcrumbs,
    trimBreadcrumbs,

    -- ** Event processors
    setEventProcessor,
    addEventProcessor,
    unsetEventProcessor,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson qualified as Aeson
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Monoid (Dual (..), Endo (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Vector (Vector)
import Patrol qualified
import Sentry.Event (CapturedEvent (..))
import Sentry.Scope.Internal (Scope, ScopeData (..), modifyScopeData)

-- | A pending modification to a 'Scope'.
--
-- Updates can be chained with '<>'; the left operand is applied first, so later
-- updates win on conflicting scalar fields. Build one with a smart constructor
-- below or the 'ScopeUpdate' constructor directly; recover the wrapped
-- 'ScopeData -> ScopeData' with 'runScopeUpdate' (e.g. @'runScopeUpdate' upd
-- 'mempty'@ yields the 'ScopeData' the update produces against an empty scope).
type ScopeUpdate :: Type
newtype ScopeUpdate = ScopeUpdate {runScopeUpdate :: ScopeData -> ScopeData}
  deriving (Semigroup, Monoid) via (Dual (Endo ScopeData))

-- | Apply a 'ScopeUpdate' to a 'Scope' as a single atomic 'IORef' update.
apply :: (MonadIO m) => Scope -> ScopeUpdate -> m ()
apply scope = modifyScopeData scope . runScopeUpdate

-- | Internal builder shared by the smart constructors below.
edit :: (ScopeData -> ScopeData) -> ScopeUpdate
edit = ScopeUpdate

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

-- * Breadcrumb updates

-- | Append a 'Patrol.Type.Breadcrumb.Breadcrumb' verbatim.
--
-- This is the pure mutation primitive: it does /not/ default the timestamp, run
-- 'Sentry.Client.Options.ClientOptions.beforeBreadcrumb', or trim to
-- 'Sentry.Client.Options.ClientOptions.maxBreadcrumbs'. Use
-- 'Sentry.Scope.addBreadcrumb' for the policy-applying, timestamp-defaulting
-- entry point.
addBreadcrumb :: Patrol.Breadcrumb -> ScopeUpdate
addBreadcrumb crumb = edit \s -> s{breadcrumbs = s.breadcrumbs Seq.|> crumb}

-- | Append several 'Patrol.Type.Breadcrumb.Breadcrumb's verbatim, in order. See
-- 'addBreadcrumb' for the caveats around policy and defaulting.
addBreadcrumbs :: (Foldable f) => f Patrol.Breadcrumb -> ScopeUpdate
addBreadcrumbs crumbs = edit \s -> s{breadcrumbs = s.breadcrumbs <> Seq.fromList (toList crumbs)}

-- | Clear all breadcrumbs.
clearBreadcrumbs :: ScopeUpdate
clearBreadcrumbs = edit \s -> s{breadcrumbs = mempty}

-- | Drop the oldest breadcrumbs so that at most @n@ remain. A non-positive @n@
-- clears them entirely.
trimBreadcrumbs :: Int -> ScopeUpdate
trimBreadcrumbs n = edit \s ->
  let len = Seq.length s.breadcrumbs
   in if len > n then s{breadcrumbs = Seq.drop (len - n) s.breadcrumbs} else s

-- * Event-processor updates

-- | Replace the scope's event processor with the given function.
setEventProcessor :: (CapturedEvent -> Maybe Patrol.Event) -> ScopeUpdate
setEventProcessor f = edit \s -> s{eventProcessor = f}

-- | Chain a new processor after the existing one.
--
-- The existing processor runs first; its output is then passed to the new
-- processor. If the existing processor drops the event ('Nothing'), the new
-- processor is not called. Matches the left-to-right chaining of the 'ScopeData'
-- 'Semigroup'.
addEventProcessor :: (CapturedEvent -> Maybe Patrol.Event) -> ScopeUpdate
addEventProcessor g = edit \s ->
  s{eventProcessor = \ce -> s.eventProcessor ce >>= \ev -> g ce{event = ev}}

-- | Reset the scope's event processor to the default pass-through (no filtering
-- or mutation).
unsetEventProcessor :: ScopeUpdate
unsetEventProcessor = edit \s -> s{eventProcessor = \ce -> Just ce.event}
