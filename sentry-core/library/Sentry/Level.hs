-- | The 'Patrol.Type.Level.Level' severity type and its constructors,
-- re-exported under the @Sentry@ namespace for qualified import:
--
-- @
-- import Sentry.Level qualified as Level
--
-- Sentry.captureMessage Level.Error \"boom\"
-- Scope.setLevel scope Level.Warning
-- @
module Sentry.Level (module Patrol.Type.Level) where

import Patrol.Type.Level
