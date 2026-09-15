-- | The user attached to a scope or an event, and builders for its fields.
--
-- @
-- import Sentry qualified
-- import Sentry.Geo qualified
-- import Sentry.User qualified
--
-- Sentry.setUser
--   [ Sentry.User.setId \"user-42\",
--     Sentry.User.setName \"Alice\",
--     Sentry.User.setGeo [Sentry.Geo.setCity \"Detroit\", Sentry.Geo.setCountryCode \"US\"]
--   ]
-- @
--
-- Sentry needs at least one of @id@, @username@, @email@ or @ipAddress@ to
-- count a user toward an issue's affected-users tally.
--
-- Import this module qualified to use its builders and record-dot fields.
--
-- See "Sentry.Update" for the composition rules every builder module shares.
module Sentry.User
  ( -- * Record
    module Patrol.Type.User,

    -- * Updates
    UserUpdate,
    with,

    -- * Identity fields
    setId,
    setName,
    setEmail,
    setIpAddress,
    setSegment,
    setUsername,

    -- * Keyed data
    setData,
    removeData,
    clearData,

    -- * Nested geo
    setGeo,
    modifyGeo,
    modifyExistingGeo,
    unsetGeo,
  )
where

import Data.Aeson qualified as Aeson
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Patrol.Type.User
import Patrol.Type.User qualified as Patrol.User
import Sentry.Geo qualified
import Sentry.Update (Update (..))
import Sentry.Update qualified
import Witch qualified

-- | A pending change to a 'User'. See "Sentry.Update".
type UserUpdate :: Type
type UserUpdate = Update User

-- | Build an update using the user's current value, including preceding
-- updates in the composition. Apply the resulting update to that same record.
--
-- @
-- Sentry.User.with \\u -> Sentry.User.setData \"display_name\" (Aeson.String u.name)
-- @
--
-- Note that 'User' text fields are 'Text', not @Maybe Text@, so an unset field
-- reads as @\"\"@ and cannot be distinguished from one explicitly assigned the
-- empty string. Branch accordingly.
with :: (Witch.From a UserUpdate) => (User -> a) -> UserUpdate
with = Sentry.Update.with

-- | Assign the id.
setId :: Text -> UserUpdate
setId !assigned = Update \u -> u{Patrol.User.id = assigned}

-- | Assign the display name.
setName :: Text -> UserUpdate
setName !assigned = Update \u -> u{Patrol.User.name = assigned}

-- | Assign the email address.
setEmail :: Text -> UserUpdate
setEmail !assigned = Update \u -> u{Patrol.User.email = assigned}

-- | Assign the IP address.
--
-- Sentry treats the literal @\"{{auto}}\"@ as a request to infer the address
-- from the incoming connection.
setIpAddress :: Text -> UserUpdate
setIpAddress !assigned = Update \u -> u{Patrol.User.ipAddress = assigned}

-- | Assign the segment.
--
-- Sentry no longer documents @segment@ as part of the user payload; prefer a
-- tag via 'Sentry.setTag' for cohort labelling.
setSegment :: Text -> UserUpdate
setSegment !assigned = Update \u -> u{Patrol.User.segment = assigned}

-- | Assign the username.
setUsername :: Text -> UserUpdate
setUsername !assigned = Update \u -> u{Patrol.User.username = assigned}

-- | Insert or overwrite one user data key.
setData :: Text -> Aeson.Value -> UserUpdate
setData !key !value = Update \u -> let !result = Map.insert key value u.data_ in u{Patrol.User.data_ = result}

-- | Remove one key; absent keys are ignored.
removeData :: Text -> UserUpdate
removeData !key = Update \u -> let !result = Map.delete key u.data_ in u{Patrol.User.data_ = result}

-- | Remove all user data keys.
clearData :: UserUpdate
clearData = Update \u -> u{Patrol.User.data_ = Map.empty}

-- | Establish a geo on the user, replacing any it already had.
--
-- Accepts a 'Sentry.Geo.GeoUpdate', a list of them, or a 'Sentry.Geo.Geo' you
-- already hold.
setGeo :: (Witch.From a Sentry.Geo.GeoUpdate) => a -> UserUpdate
setGeo upd = Update \u ->
  let !g = Sentry.Update.run upd Sentry.Geo.empty in u{Patrol.User.geo = Just g}

-- | Modify the geo, starting from empty when absent. The result is forced before storing.
modifyGeo :: (Witch.From a Sentry.Geo.GeoUpdate) => a -> UserUpdate
modifyGeo upd = Update \u -> case u.geo of
  Nothing -> Sentry.Update.run (setGeo upd) u
  Just _ -> Sentry.Update.run (modifyExistingGeo upd) u

-- | Modify an existing geo. When absent, leave it absent without evaluating the update.
modifyExistingGeo :: (Witch.From a Sentry.Geo.GeoUpdate) => a -> UserUpdate
modifyExistingGeo upd = Update \u -> case u.geo of
  Nothing -> u
  Just g -> let !g' = Sentry.Update.run upd g in u{Patrol.User.geo = Just g'}

-- | Remove the geo from the user.
unsetGeo :: UserUpdate
unsetGeo = Update \u -> u{Patrol.User.geo = Nothing}
