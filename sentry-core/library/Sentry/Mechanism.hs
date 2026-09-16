-- | The mechanism attached to a captured exception, builders for its fields,
-- and the mechanisms this SDK attaches itself.
--
-- @
-- import Sentry qualified
-- import Sentry.Mechanism qualified
--
-- Sentry.captureExceptionWith def{mechanismOverride = Just Sentry.Mechanism.generic} e
-- Sentry.captureUnhandledException \"warp.onException\" e
-- @
--
-- Per Sentry's convention, the mechanism type is a short, lowercase,
-- dot-separated identifier naming the source of the capture; free-form context
-- belongs in its data instead.
--
-- See "Sentry.Update" for details on our builder mechanism.
module Sentry.Mechanism
  ( -- * Record
    module Patrol.Type.Mechanism,

    -- * Updates
    MechanismUpdate,
    with,

    -- * SDK mechanisms
    generic,
    unhandled,

    -- * Fields
    setType,
    setDescription,
    setHelpLink,
    setHandled,
    setOptionalHandled,
    unsetHandled,
    setSynthetic,
    setOptionalSynthetic,
    unsetSynthetic,
    setMeta,
    setOptionalMeta,
    unsetMeta,

    -- * Keyed data
    setData,
    setOptionalData,
    removeData,
    clearData,
    lookupData,
  )
where

import Data.Aeson qualified as Aeson
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Patrol qualified
import Patrol.Type.Mechanism
import Patrol.Type.Mechanism qualified as Patrol.Mechanism
import Sentry.Update (Update (..))
import Sentry.Update qualified
import Witch qualified

-- | A pending change to a 'Mechanism'. See "Sentry.Update".
type MechanismUpdate :: Type
type MechanismUpdate = Update Mechanism

-- | Build an update using the mechanism's current value, including preceding
-- updates in the composition. Apply the resulting update to that same record.
with :: (Witch.From a MechanismUpdate) => (Mechanism -> a) -> MechanismUpdate
with = Sentry.Update.with

-- | The generic mechanism that 'Sentry.captureException' attaches to every
-- event it constructs.
generic :: Mechanism
generic = Witch.from [setType "generic", setHandled True]

-- | A mechanism for an exception that escaped to a boundary of last resort,
-- taking the type that names that boundary.
unhandled :: Text -> Mechanism
unhandled ty = Witch.from [setType ty, setHandled False]

-- | Assign the mechanism type: short, lowercase, dot-separated.
setType :: Text -> MechanismUpdate
setType !assigned = Update \m -> m{Patrol.Mechanism.type_ = assigned}

-- | Assign the human-readable description.
setDescription :: Text -> MechanismUpdate
setDescription !assigned = Update \m -> m{Patrol.Mechanism.description = assigned}

-- | Assign a link to documentation explaining this mechanism.
setHelpLink :: Text -> MechanismUpdate
setHelpLink !assigned = Update \m -> m{Patrol.Mechanism.helpLink = assigned}

-- | Record whether the exception was caught by application code.
setHandled :: Bool -> MechanismUpdate
setHandled !assigned = Update \m -> m{Patrol.Mechanism.handled = Just assigned}

-- | Clear the handled flag.
unsetHandled :: MechanismUpdate
unsetHandled = Update \m -> m{Patrol.Mechanism.handled = Nothing}

-- | Record whether the exception was synthesized by the SDK rather than
-- thrown.
setSynthetic :: Bool -> MechanismUpdate
setSynthetic !assigned = Update \m -> m{Patrol.Mechanism.synthetic = Just assigned}

-- | Clear the synthetic flag.
unsetSynthetic :: MechanismUpdate
unsetSynthetic = Update \m -> m{Patrol.Mechanism.synthetic = Nothing}

-- | Assign the OS-level error metadata.
setMeta :: Patrol.MechanismMeta -> MechanismUpdate
setMeta !assigned = Update \m -> m{Patrol.Mechanism.meta = Just assigned}

-- | Clear the OS-level error metadata.
unsetMeta :: MechanismUpdate
unsetMeta = Update \m -> m{Patrol.Mechanism.meta = Nothing}

-- | Insert or overwrite one data key.
setData :: Text -> Aeson.Value -> MechanismUpdate
setData !key !value = Update \m -> let !result = Map.insert key value m.data_ in m{Patrol.Mechanism.data_ = result}

-- | Remove one key; absent keys are ignored.
removeData :: Text -> MechanismUpdate
removeData !key = Update \m -> let !result = Map.delete key m.data_ in m{Patrol.Mechanism.data_ = result}

-- | Remove all data keys.
clearData :: MechanismUpdate
clearData = Update \m -> m{Patrol.Mechanism.data_ = Map.empty}

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalHandled :: Maybe Bool -> MechanismUpdate
setOptionalHandled = maybe unsetHandled setHandled

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalSynthetic :: Maybe Bool -> MechanismUpdate
setOptionalSynthetic = maybe unsetSynthetic setSynthetic

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalMeta :: Maybe Patrol.MechanismMeta -> MechanismUpdate
setOptionalMeta = maybe unsetMeta setMeta

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalData :: Text -> Maybe Aeson.Value -> MechanismUpdate
setOptionalData key = maybe (removeData key) (setData key)

-- | Look up a stored entry by key.
lookupData :: Text -> Mechanism -> Maybe Aeson.Value
lookupData key record = Map.lookup key record.data_
