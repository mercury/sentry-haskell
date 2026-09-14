-- | Breadcrumbs, and builders for their fields.
--
-- See "Sentry.Update" for the composition rules every builder module shares.
module Sentry.Breadcrumb
  ( -- * Record
    module Patrol.Type.Breadcrumb,
    BreadcrumbType (..),

    -- * Updates
    BreadcrumbUpdate,
    with,
    apply,

    -- * Fields
    setMessage,
    setCategory,
    setLevel,
    unsetLevel,
    setType,
    unsetType,
    setTimestamp,
    unsetTimestamp,
    setEventId,
    unsetEventId,

    -- * Keyed data
    setData,
    removeData,
    clearData,
  )
where

import Data.Aeson qualified as Aeson
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Patrol qualified
import Patrol.Type.Breadcrumb
import Patrol.Type.Breadcrumb qualified as Patrol.Breadcrumb
import Patrol.Type.BreadcrumbType (BreadcrumbType (..))
import Sentry.Update (Update (..))
import Sentry.Update qualified
import Witch qualified

-- | A pending change to a 'Breadcrumb'. See "Sentry.Update".
type BreadcrumbUpdate :: Type
type BreadcrumbUpdate = Update Breadcrumb

-- | Build an update using the breadcrumb's current value, including preceding
-- updates in the composition. Apply the resulting update to that same record.
with :: (Witch.From a BreadcrumbUpdate) => (Breadcrumb -> a) -> BreadcrumbUpdate
with = Sentry.Update.with

-- | Assign the human-readable message.
setMessage :: Text -> BreadcrumbUpdate
setMessage !assigned = Update \c -> c{Patrol.Breadcrumb.message = assigned}

-- | Assign the dotted category, e.g. @\"ui.click\"@ or @\"http\"@.
setCategory :: Text -> BreadcrumbUpdate
setCategory !assigned = Update \c -> c{Patrol.Breadcrumb.category = assigned}

-- | Assign the severity level.
setLevel :: Patrol.Level -> BreadcrumbUpdate
setLevel !assigned = Update \c -> c{Patrol.Breadcrumb.level = Just assigned}

-- | Clear the severity level.
unsetLevel :: BreadcrumbUpdate
unsetLevel = Update \c -> c{Patrol.Breadcrumb.level = Nothing}

-- | Assign the breadcrumb type.
setType :: BreadcrumbType -> BreadcrumbUpdate
setType !assigned = Update \c -> c{Patrol.Breadcrumb.type_ = Just assigned}

-- | Clear the breadcrumb type, restoring the capture-time default.
unsetType :: BreadcrumbUpdate
unsetType = Update \c -> c{Patrol.Breadcrumb.type_ = Nothing}

-- | Assign the timestamp.
setTimestamp :: UTCTime -> BreadcrumbUpdate
setTimestamp !assigned = Update \c -> c{Patrol.Breadcrumb.timestamp = Just assigned}

-- | Clear the timestamp, restoring the capture-time default.
unsetTimestamp :: BreadcrumbUpdate
unsetTimestamp = Update \c -> c{Patrol.Breadcrumb.timestamp = Nothing}

-- | Assign the id of a related event.
setEventId :: Patrol.EventId -> BreadcrumbUpdate
setEventId !assigned = Update \c -> c{Patrol.Breadcrumb.eventId = Just assigned}

-- | Clear the related event id.
unsetEventId :: BreadcrumbUpdate
unsetEventId = Update \c -> c{Patrol.Breadcrumb.eventId = Nothing}

-- | Insert or overwrite one data key.
setData :: Text -> Aeson.Value -> BreadcrumbUpdate
setData !key !value = Update \c -> let !result = Map.insert key value c.data_ in c{Patrol.Breadcrumb.data_ = result}

-- | Remove one key; absent keys are ignored.
removeData :: Text -> BreadcrumbUpdate
removeData !key = Update \c -> let !result = Map.delete key c.data_ in c{Patrol.Breadcrumb.data_ = result}

-- | Remove all data keys.
clearData :: BreadcrumbUpdate
clearData = Update \c -> c{Patrol.Breadcrumb.data_ = Map.empty}

-- | Apply a single builder, a list, or a replacement record to an existing
-- 'Breadcrumb'.
apply :: (Witch.From a BreadcrumbUpdate) => Breadcrumb -> a -> Breadcrumb
apply = flip Sentry.Update.run
