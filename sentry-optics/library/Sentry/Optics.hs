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
-- 'editScope' (the optics counterpart to the effectful @Sentry.Scope@ setters),
-- the 'apply' \/ 'runScopeUpdate' \/ 'ScopeUpdate' trio for working with
-- 'ScopeUpdate' values directly, and @empty@-prefixed record seeds ('emptyUser',
-- …) for building values with labels.
--
-- The optics operators and vocabulary (@?=@, @%@, @^.@, @at@, …) live in
-- "Sentry.Optics.Prelude" (imported unqualified), which also brings the enum
-- /value/ labels (@#warning :: Level@, @#navigation :: BreadcrumbType@; see
-- "Sentry.Optics.Values"). Field and constructor labels come into scope with
-- either import. Outside an optic-determined position, name enum values with the
-- qualified "Sentry.Level" \/ "Sentry.BreadcrumbType" constructors.
module Sentry.Optics
  ( -- * The Sentry SDK surface
    module Sentry,

    -- * Authoring scope updates
    editScope,
    apply,
    runScopeUpdate,
    ScopeUpdate,

    -- * Empty record seeds
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
