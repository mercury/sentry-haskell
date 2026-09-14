-- | The geographic sub-record of a 'Patrol.Type.User.User', and builders for
-- its fields.
--
-- @
-- import Sentry.Geo qualified
-- import Sentry.User qualified
--
-- Sentry.User.setGeo [Sentry.Geo.setCity \"Detroit\", Sentry.Geo.setCountryCode \"US\"]
-- @
--
-- See "Sentry.Update" for the composition rules every builder module shares.
module Sentry.Geo
  ( -- * Record
    module Patrol.Type.Geo,

    -- * Updates
    GeoUpdate,
    with,

    -- * Fields
    setCity,
    setCountryCode,
    setRegion,
  )
where

import Data.Kind (Type)
import Data.Text (Text)
import Patrol.Type.Geo
import Patrol.Type.Geo qualified as Patrol.Geo
import Sentry.Update (Update (..))
import Sentry.Update qualified
import Witch qualified

-- | A pending change to a 'Geo'. See "Sentry.Update".
type GeoUpdate :: Type
type GeoUpdate = Update Geo

-- | Build an update using the geo's current value, including preceding
-- updates in the composition. Apply the resulting update to that same record.
--
-- Note that 'Geo' fields are 'Text', not @Maybe Text@, so an unset field reads
-- as @\"\"@ and cannot be distinguished from one explicitly assigned the empty
-- string. Branch accordingly.
with :: (Witch.From a GeoUpdate) => (Geo -> a) -> GeoUpdate
with = Sentry.Update.with

-- | Assign the city.
setCity :: Text -> GeoUpdate
setCity !assigned = Update \g -> g{Patrol.Geo.city = assigned}

-- | Assign the ISO 3166-1 alpha-2 country code.
setCountryCode :: Text -> GeoUpdate
setCountryCode !assigned = Update \g -> g{Patrol.Geo.countryCode = assigned}

-- | Assign the region.
setRegion :: Text -> GeoUpdate
setRegion !assigned = Update \g -> g{Patrol.Geo.region = assigned}
