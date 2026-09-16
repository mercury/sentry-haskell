-- | The OsContext record and composable field builders.
--
-- See "Sentry.Update" for details on our builder mechanism.
module Sentry.OsContext
  ( module Patrol.Type.OsContext,
    OsContextUpdate,
    with,
    setBuild,
    setKernelVersion,
    setName,
    setRawDescription,
    setVersion,
    setRooted,
    setOptionalRooted,
    unsetRooted,
  )
where

import Data.Kind (Type)
import Data.Text (Text)
import Patrol.Type.OsContext
import Sentry.Update (Update (..))
import Sentry.Update qualified
import Witch qualified

-- | A pending record change.
type OsContextUpdate :: Type
type OsContextUpdate = Update OsContext

-- | Observe preceding updates and apply the resulting update to the same record.
with :: (Witch.From a OsContextUpdate) => (OsContext -> a) -> OsContextUpdate
with = Sentry.Update.with

-- | Assign build. Text fields can be cleared with empty text.
setBuild :: Text -> OsContextUpdate
setBuild !assigned = Update \r -> r{Patrol.Type.OsContext.build = assigned}

-- | Assign kernelVersion. Text fields can be cleared with empty text.
setKernelVersion :: Text -> OsContextUpdate
setKernelVersion !assigned = Update \r -> r{Patrol.Type.OsContext.kernelVersion = assigned}

-- | Assign name. Text fields can be cleared with empty text.
setName :: Text -> OsContextUpdate
setName !assigned = Update \r -> r{Patrol.Type.OsContext.name = assigned}

-- | Assign rawDescription. Text fields can be cleared with empty text.
setRawDescription :: Text -> OsContextUpdate
setRawDescription !assigned = Update \r -> r{Patrol.Type.OsContext.rawDescription = assigned}

-- | Assign version. Text fields can be cleared with empty text.
setVersion :: Text -> OsContextUpdate
setVersion !assigned = Update \r -> r{Patrol.Type.OsContext.version = assigned}

-- | Assign rooted.
setRooted :: Bool -> OsContextUpdate
setRooted !assigned = Update \r -> r{Patrol.Type.OsContext.rooted = Just assigned}

-- | Clear rooted.
unsetRooted :: OsContextUpdate
unsetRooted = Update \r -> r{Patrol.Type.OsContext.rooted = Nothing}

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalRooted :: Maybe Bool -> OsContextUpdate
setOptionalRooted = maybe unsetRooted setRooted
