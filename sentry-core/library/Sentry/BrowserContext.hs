-- | The BrowserContext record and composable field builders. See "Sentry.Update".
module Sentry.BrowserContext
  ( module Patrol.Type.BrowserContext,
    BrowserContextUpdate,
    with,
    setName,
    setVersion,
  )
where

import Data.Kind (Type)
import Data.Text (Text)
import Patrol.Type.BrowserContext
import Sentry.Update (Update (..))
import Sentry.Update qualified
import Witch qualified

-- | A pending record change.
type BrowserContextUpdate :: Type
type BrowserContextUpdate = Update BrowserContext

-- | Observe preceding updates and apply the resulting update to the same record.
with :: (Witch.From a BrowserContextUpdate) => (BrowserContext -> a) -> BrowserContextUpdate
with = Sentry.Update.with

-- | Assign name. Text fields can be cleared with empty text.
setName :: Text -> BrowserContextUpdate
setName !assigned = Update \r -> r{Patrol.Type.BrowserContext.name = assigned}

-- | Assign version. Text fields can be cleared with empty text.
setVersion :: Text -> BrowserContextUpdate
setVersion !assigned = Update \r -> r{Patrol.Type.BrowserContext.version = assigned}
