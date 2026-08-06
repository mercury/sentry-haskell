-- | The "batteries-included" optics-based entry point: re-exports "Sentry"
-- (the @sentry@ package's default-transport 'init' \/ 'withSentry', plus
-- everything from "Sentry.Core") and adds the optics conveniences from
-- "Sentry.Core.Optics" — 'editScope', the 'apply' \/ 'runScopeUpdate' \/
-- 'ScopeUpdate' trio, and the @empty@-prefixed record values.
--
-- @
-- import Sentry.Optics qualified as Sentry
-- import Sentry.Optics.Prelude
-- @
--
-- Depend on "Sentry.Core.Optics" (from @sentry-core-optics@) directly instead
-- if you don't want the HTTP transport dependency.
module Sentry.Optics
  ( -- * The Sentry SDK surface (with default transport)
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

import Sentry
import Sentry.Core.Optics
  ( ScopeUpdate,
    apply,
    editScope,
    emptyBreadcrumb,
    emptyEvent,
    emptyRequest,
    emptyUser,
    runScopeUpdate,
  )
