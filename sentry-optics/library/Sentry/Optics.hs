-- | The optics-flavored Sentry entry point, intended to be imported qualified
-- __in place of__ "Sentry":
--
-- @
-- import Sentry.Optics qualified as Sentry
-- import Sentry.Optics.Prelude
--
-- f = Sentry.withIsolationScope \\scope ->
--       Sentry.editScope scope do
--         #level ?= #warning
--         #tags % at \"env\" ?= \"prod\"
--         #user ?= (Sentry.emptyUser & #email .~ \"a\@b.com\")
-- @
--
-- It re-exports the entire "Sentry" surface and adds the optics conveniences:
-- 
-- * 'editScope' for editing scopes with optics
-- * 'apply' \/ 'runScopeUpdate' \/ 'ScopeUpdate' for working with 'ScopeUpdate'
--   values directly
-- * @empty@-prefixed record values for building entries with labels
module Sentry.Optics
  ( -- * The Sentry SDK surface
    module Sentry,

    -- * Authoring scope updates
    editScope,
    apply,
    runScopeUpdate,
    ScopeUpdate,

    -- * Empty record values
    emptyUser,
    emptyBreadcrumb,
    emptyRequest,
    emptyEvent,
  )
where

import Patrol.Type.Breadcrumb qualified as Breadcrumb
import Patrol.Type.Event qualified as Event
import Patrol.Type.Request qualified as Request
import Patrol.Type.User qualified as User
import Sentry
import Sentry.Optics.Internal (editScope)
import Sentry.Scope.Update (ScopeUpdate, apply, runScopeUpdate)

-- | An empty 'Patrol.Type.User.User' to build from with labels:
-- @emptyUser & #email .~ \"a\@b.com\"@.
emptyUser :: User.User
emptyUser = User.empty

-- | An empty 'Patrol.Type.Breadcrumb.Breadcrumb'.
emptyBreadcrumb :: Breadcrumb.Breadcrumb
emptyBreadcrumb = Breadcrumb.empty

-- | An empty 'Patrol.Type.Request.Request'.
emptyRequest :: Request.Request
emptyRequest = Request.empty

-- | An empty 'Patrol.Type.Event.Event'.
emptyEvent :: Event.Event
emptyEvent = Event.empty
