-- | The Sentry runtime context and builders for its fields.
--
-- @
-- import Sentry qualified
-- import Sentry.RuntimeContext qualified
--
-- Sentry.setRuntimeContext
--   [ Sentry.RuntimeContext.setName \"ghc\",
--     Sentry.RuntimeContext.setVersion \"9.10.3\"
--   ]
-- @
--
-- See "Sentry.Update" for details on our builder mechanism.
module Sentry.RuntimeContext
  ( -- * Record
    module Patrol.Type.RuntimeContext,

    -- * Updates
    RuntimeContextUpdate,
    with,

    -- * Fields
    setName,
    setVersion,
    setBuild,
    setRawDescription,
  )
where

import Data.Kind (Type)
import Data.Text (Text)
import Patrol.Type.RuntimeContext
import Patrol.Type.RuntimeContext qualified as Patrol.RuntimeContext
import Sentry.Update (Update (..))
import Sentry.Update qualified
import Witch qualified

-- | A pending change to a 'RuntimeContext'. See "Sentry.Update".
type RuntimeContextUpdate :: Type
type RuntimeContextUpdate = Update RuntimeContext

-- | Build an update using the runtime context's current value, including preceding
-- updates in the composition. Apply the resulting update to that same record.
with :: (Witch.From a RuntimeContextUpdate) => (RuntimeContext -> a) -> RuntimeContextUpdate
with = Sentry.Update.with

-- | Assign the runtime name, e.g. @\"ghc\"@.
setName :: Text -> RuntimeContextUpdate
setName !assigned = Update \rc -> rc{Patrol.RuntimeContext.name = assigned}

-- | Assign the runtime version.
setVersion :: Text -> RuntimeContextUpdate
setVersion !assigned = Update \rc -> rc{Patrol.RuntimeContext.version = assigned}

-- | Assign the build identifier.
setBuild :: Text -> RuntimeContextUpdate
setBuild !assigned = Update \rc -> rc{Patrol.RuntimeContext.build = assigned}

-- | Assign the unparsed description the other fields were derived from.
setRawDescription :: Text -> RuntimeContextUpdate
setRawDescription !assigned = Update \rc -> rc{Patrol.RuntimeContext.rawDescription = assigned}
