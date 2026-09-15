-- | Breadcrumbs, and builders for their fields.
--
-- See "Sentry.Update" for the composition rules every builder module shares.
module Sentry.Breadcrumb
  ( -- * Record
    Breadcrumb (..),
    empty,
    Breadcrumbs (..),
    BreadcrumbsUpdate,
    emptyCollection,
    withCollection,
    singleton,
    setValues,
    clearValues,
    appendBreadcrumb,
    prependBreadcrumb,
    firstBreadcrumb,
    lastBreadcrumb,
    eachBreadcrumb,
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
import Patrol.Type.Breadcrumb (Breadcrumb (..), empty)
import Patrol.Type.Breadcrumb qualified as Patrol.Breadcrumb
import Patrol.Type.BreadcrumbType (BreadcrumbType (..))
import Patrol.Type.Breadcrumbs (Breadcrumbs (..))
import Sentry.Collection.Internal (mapWHNF)
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

-- | Updates to the collection; lists of child updates describe one child.
type BreadcrumbsUpdate :: Type
type BreadcrumbsUpdate = Update Breadcrumbs

-- | An empty collection wrapper.
emptyCollection :: Breadcrumbs
emptyCollection = Breadcrumbs []

-- | Inspect the collection after preceding edits.
withCollection :: (Witch.From a BreadcrumbsUpdate) => (Breadcrumbs -> a) -> BreadcrumbsUpdate
withCollection = Sentry.Update.with

-- | Construct one child from an update, update list, or record.
singleton :: (Witch.From a BreadcrumbUpdate) => a -> Breadcrumbs
singleton upd = let !child = Sentry.Update.run upd empty in Breadcrumbs [child]

-- | Replace the values.
setValues :: (Witch.From a BreadcrumbUpdate) => [a] -> BreadcrumbsUpdate
setValues upds = Update \_ ->
  let !result = mapWHNF (\upd -> Sentry.Update.run upd empty) upds
   in Breadcrumbs result

-- | Empty the wrapper without removing it from its parent.
clearValues :: BreadcrumbsUpdate
clearValues = Update (const emptyCollection)

-- | Append a child constructed from empty.
appendBreadcrumb :: (Witch.From a BreadcrumbUpdate) => a -> BreadcrumbsUpdate
appendBreadcrumb upd = Update \(Breadcrumbs xs) ->
  let !child = Sentry.Update.run upd empty in Breadcrumbs (xs <> [child])

-- | Prepend a child constructed from empty.
prependBreadcrumb :: (Witch.From a BreadcrumbUpdate) => a -> BreadcrumbsUpdate
prependBreadcrumb upd = Update \(Breadcrumbs xs) ->
  let !child = Sentry.Update.run upd empty in Breadcrumbs (child : xs)

-- | Edit the first entry; an empty selection skips the update.
firstBreadcrumb :: (Witch.From a BreadcrumbUpdate) => a -> BreadcrumbsUpdate
firstBreadcrumb upd = Update \collection@(Breadcrumbs xs) -> case xs of
  [] -> collection
  x : rest -> let !child = Sentry.Update.run upd x in Breadcrumbs (child : rest)

-- | Edit the last entry without forcing unselected records.
lastBreadcrumb :: (Witch.From a BreadcrumbUpdate) => a -> BreadcrumbsUpdate
lastBreadcrumb upd = Update \(Breadcrumbs xs) -> let !result = go xs in Breadcrumbs result
  where
    go [] = []
    go [x] = let !child = Sentry.Update.run upd x in [child]
    go (x : xs) = let !rest = go xs in x : rest

-- | Edit every entry independently.
eachBreadcrumb :: (Witch.From a BreadcrumbUpdate) => a -> BreadcrumbsUpdate
eachBreadcrumb upd = Update \(Breadcrumbs xs) ->
  let !result = mapWHNF (Sentry.Update.run upd) xs
   in Breadcrumbs result
