-- | The AppContext record and composable field builders. See "Sentry.Update".
module Sentry.AppContext
  ( module Patrol.Type.AppContext,
    AppContextUpdate,
    with,
    setAppBuild,
    setAppIdentifier,
    setAppName,
    setAppVersion,
    setBuildType,
    setDeviceAppHash,
    setAppMemory,
    unsetAppMemory,
    setAppStartTime,
    unsetAppStartTime,
  )
where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Patrol.Type.AppContext
import Sentry.Update (Update (..))
import Sentry.Update qualified
import Witch qualified

-- | A pending record change.
type AppContextUpdate :: Type
type AppContextUpdate = Update AppContext

-- | Observe preceding updates and apply the resulting update to the same record.
with :: (Witch.From a AppContextUpdate) => (AppContext -> a) -> AppContextUpdate
with = Sentry.Update.with

-- | Assign appBuild. Text fields can be cleared with empty text.
setAppBuild :: Text -> AppContextUpdate
setAppBuild !assigned = Update \r -> r{Patrol.Type.AppContext.appBuild = assigned}

-- | Assign appIdentifier. Text fields can be cleared with empty text.
setAppIdentifier :: Text -> AppContextUpdate
setAppIdentifier !assigned = Update \r -> r{Patrol.Type.AppContext.appIdentifier = assigned}

-- | Assign appName. Text fields can be cleared with empty text.
setAppName :: Text -> AppContextUpdate
setAppName !assigned = Update \r -> r{Patrol.Type.AppContext.appName = assigned}

-- | Assign appVersion. Text fields can be cleared with empty text.
setAppVersion :: Text -> AppContextUpdate
setAppVersion !assigned = Update \r -> r{Patrol.Type.AppContext.appVersion = assigned}

-- | Assign buildType. Text fields can be cleared with empty text.
setBuildType :: Text -> AppContextUpdate
setBuildType !assigned = Update \r -> r{Patrol.Type.AppContext.buildType = assigned}

-- | Assign deviceAppHash. Text fields can be cleared with empty text.
setDeviceAppHash :: Text -> AppContextUpdate
setDeviceAppHash !assigned = Update \r -> r{Patrol.Type.AppContext.deviceAppHash = assigned}

-- | Assign appMemory.
setAppMemory :: Int -> AppContextUpdate
setAppMemory !assigned = Update \r -> r{Patrol.Type.AppContext.appMemory = Just assigned}

-- | Clear appMemory.
unsetAppMemory :: AppContextUpdate
unsetAppMemory = Update \r -> r{Patrol.Type.AppContext.appMemory = Nothing}

-- | Assign appStartTime.
setAppStartTime :: UTCTime -> AppContextUpdate
setAppStartTime !assigned = Update \r -> r{Patrol.Type.AppContext.appStartTime = Just assigned}

-- | Clear appStartTime.
unsetAppStartTime :: AppContextUpdate
unsetAppStartTime = Update \r -> r{Patrol.Type.AppContext.appStartTime = Nothing}
