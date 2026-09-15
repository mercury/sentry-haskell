-- | The 'Patrol.Type.Context.Context' type, re-exported under the @Sentry@
-- namespace for qualified import:
--
-- @
-- import Sentry.Context qualified as Context
--
-- Sentry.setContext \"runtime\" (Context.Runtime RuntimeContext.empty)
-- @
module Sentry.Context (module Patrol.Type.Context) where

import Patrol.Type.Context
