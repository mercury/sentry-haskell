-- | The DeviceContext record and composable field builders. See "Sentry.Update".
module Sentry.DeviceContext
  ( module Patrol.Type.DeviceContext,
    DeviceContextUpdate,
    with,
    setArch,
    setBatteryLevel,
    setOptionalBatteryLevel,
    unsetBatteryLevel,
    setBatteryStatus,
    setBootTime,
    setOptionalBootTime,
    unsetBootTime,
    setBrand,
    setCharging,
    setOptionalCharging,
    unsetCharging,
    setCpuDescription,
    setDeviceType,
    setDeviceUniqueIdentifier,
    setExternalFreeStorage,
    setOptionalExternalFreeStorage,
    unsetExternalFreeStorage,
    setExternalStorageSize,
    setOptionalExternalStorageSize,
    unsetExternalStorageSize,
    setFamily,
    setFreeMemory,
    setOptionalFreeMemory,
    unsetFreeMemory,
    setFreeStorage,
    setOptionalFreeStorage,
    unsetFreeStorage,
    setLowMemory,
    setOptionalLowMemory,
    unsetLowMemory,
    setManufacturer,
    setMemorySize,
    setOptionalMemorySize,
    unsetMemorySize,
    setModel,
    setModelId,
    setName,
    setOnline,
    setOptionalOnline,
    unsetOnline,
    setOrientation,
    setProcessorCount,
    setOptionalProcessorCount,
    unsetProcessorCount,
    setProcessorFrequency,
    setOptionalProcessorFrequency,
    unsetProcessorFrequency,
    setScreenDensity,
    setOptionalScreenDensity,
    unsetScreenDensity,
    setScreenDpi,
    setOptionalScreenDpi,
    unsetScreenDpi,
    setScreenResolution,
    setSimulator,
    setOptionalSimulator,
    unsetSimulator,
    setStorageSize,
    setOptionalStorageSize,
    unsetStorageSize,
    setSupportsAccelerometer,
    setOptionalSupportsAccelerometer,
    unsetSupportsAccelerometer,
    setSupportsAudio,
    setOptionalSupportsAudio,
    unsetSupportsAudio,
    setSupportsGyroscope,
    setOptionalSupportsGyroscope,
    unsetSupportsGyroscope,
    setSupportsLocationService,
    setOptionalSupportsLocationService,
    unsetSupportsLocationService,
    setSupportsVibration,
    setOptionalSupportsVibration,
    unsetSupportsVibration,
    setTimezone,
    setUsableMemory,
    setOptionalUsableMemory,
    unsetUsableMemory,
  )
where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Patrol.Type.DeviceContext
import Sentry.Update (Update (..))
import Sentry.Update qualified
import Witch qualified

-- | A pending record change.
type DeviceContextUpdate :: Type
type DeviceContextUpdate = Update DeviceContext

-- | Observe preceding updates and apply the resulting update to the same record.
with :: (Witch.From a DeviceContextUpdate) => (DeviceContext -> a) -> DeviceContextUpdate
with = Sentry.Update.with

-- | Assign arch. Text fields can be cleared with empty text.
setArch :: Text -> DeviceContextUpdate
setArch !assigned = Update \r -> r{Patrol.Type.DeviceContext.arch = assigned}

-- | Assign batteryLevel.
setBatteryLevel :: Double -> DeviceContextUpdate
setBatteryLevel !assigned = Update \r -> r{Patrol.Type.DeviceContext.batteryLevel = Just assigned}

-- | Clear batteryLevel.
unsetBatteryLevel :: DeviceContextUpdate
unsetBatteryLevel = Update \r -> r{Patrol.Type.DeviceContext.batteryLevel = Nothing}

-- | Assign batteryStatus. Text fields can be cleared with empty text.
setBatteryStatus :: Text -> DeviceContextUpdate
setBatteryStatus !assigned = Update \r -> r{Patrol.Type.DeviceContext.batteryStatus = assigned}

-- | Assign bootTime.
setBootTime :: UTCTime -> DeviceContextUpdate
setBootTime !assigned = Update \r -> r{Patrol.Type.DeviceContext.bootTime = Just assigned}

-- | Clear bootTime.
unsetBootTime :: DeviceContextUpdate
unsetBootTime = Update \r -> r{Patrol.Type.DeviceContext.bootTime = Nothing}

-- | Assign brand. Text fields can be cleared with empty text.
setBrand :: Text -> DeviceContextUpdate
setBrand !assigned = Update \r -> r{Patrol.Type.DeviceContext.brand = assigned}

-- | Assign charging.
setCharging :: Bool -> DeviceContextUpdate
setCharging !assigned = Update \r -> r{Patrol.Type.DeviceContext.charging = Just assigned}

-- | Clear charging.
unsetCharging :: DeviceContextUpdate
unsetCharging = Update \r -> r{Patrol.Type.DeviceContext.charging = Nothing}

-- | Assign cpuDescription. Text fields can be cleared with empty text.
setCpuDescription :: Text -> DeviceContextUpdate
setCpuDescription !assigned = Update \r -> r{Patrol.Type.DeviceContext.cpuDescription = assigned}

-- | Assign deviceType. Text fields can be cleared with empty text.
setDeviceType :: Text -> DeviceContextUpdate
setDeviceType !assigned = Update \r -> r{Patrol.Type.DeviceContext.deviceType = assigned}

-- | Assign deviceUniqueIdentifier. Text fields can be cleared with empty text.
setDeviceUniqueIdentifier :: Text -> DeviceContextUpdate
setDeviceUniqueIdentifier !assigned = Update \r -> r{Patrol.Type.DeviceContext.deviceUniqueIdentifier = assigned}

-- | Assign externalFreeStorage.
setExternalFreeStorage :: Int -> DeviceContextUpdate
setExternalFreeStorage !assigned = Update \r -> r{Patrol.Type.DeviceContext.externalFreeStorage = Just assigned}

-- | Clear externalFreeStorage.
unsetExternalFreeStorage :: DeviceContextUpdate
unsetExternalFreeStorage = Update \r -> r{Patrol.Type.DeviceContext.externalFreeStorage = Nothing}

-- | Assign externalStorageSize.
setExternalStorageSize :: Int -> DeviceContextUpdate
setExternalStorageSize !assigned = Update \r -> r{Patrol.Type.DeviceContext.externalStorageSize = Just assigned}

-- | Clear externalStorageSize.
unsetExternalStorageSize :: DeviceContextUpdate
unsetExternalStorageSize = Update \r -> r{Patrol.Type.DeviceContext.externalStorageSize = Nothing}

-- | Assign family. Text fields can be cleared with empty text.
setFamily :: Text -> DeviceContextUpdate
setFamily !assigned = Update \r -> r{Patrol.Type.DeviceContext.family = assigned}

-- | Assign freeMemory.
setFreeMemory :: Int -> DeviceContextUpdate
setFreeMemory !assigned = Update \r -> r{Patrol.Type.DeviceContext.freeMemory = Just assigned}

-- | Clear freeMemory.
unsetFreeMemory :: DeviceContextUpdate
unsetFreeMemory = Update \r -> r{Patrol.Type.DeviceContext.freeMemory = Nothing}

-- | Assign freeStorage.
setFreeStorage :: Int -> DeviceContextUpdate
setFreeStorage !assigned = Update \r -> r{Patrol.Type.DeviceContext.freeStorage = Just assigned}

-- | Clear freeStorage.
unsetFreeStorage :: DeviceContextUpdate
unsetFreeStorage = Update \r -> r{Patrol.Type.DeviceContext.freeStorage = Nothing}

-- | Assign lowMemory.
setLowMemory :: Bool -> DeviceContextUpdate
setLowMemory !assigned = Update \r -> r{Patrol.Type.DeviceContext.lowMemory = Just assigned}

-- | Clear lowMemory.
unsetLowMemory :: DeviceContextUpdate
unsetLowMemory = Update \r -> r{Patrol.Type.DeviceContext.lowMemory = Nothing}

-- | Assign manufacturer. Text fields can be cleared with empty text.
setManufacturer :: Text -> DeviceContextUpdate
setManufacturer !assigned = Update \r -> r{Patrol.Type.DeviceContext.manufacturer = assigned}

-- | Assign memorySize.
setMemorySize :: Int -> DeviceContextUpdate
setMemorySize !assigned = Update \r -> r{Patrol.Type.DeviceContext.memorySize = Just assigned}

-- | Clear memorySize.
unsetMemorySize :: DeviceContextUpdate
unsetMemorySize = Update \r -> r{Patrol.Type.DeviceContext.memorySize = Nothing}

-- | Assign model. Text fields can be cleared with empty text.
setModel :: Text -> DeviceContextUpdate
setModel !assigned = Update \r -> r{Patrol.Type.DeviceContext.model = assigned}

-- | Assign modelId. Text fields can be cleared with empty text.
setModelId :: Text -> DeviceContextUpdate
setModelId !assigned = Update \r -> r{Patrol.Type.DeviceContext.modelId = assigned}

-- | Assign name. Text fields can be cleared with empty text.
setName :: Text -> DeviceContextUpdate
setName !assigned = Update \r -> r{Patrol.Type.DeviceContext.name = assigned}

-- | Assign online.
setOnline :: Bool -> DeviceContextUpdate
setOnline !assigned = Update \r -> r{Patrol.Type.DeviceContext.online = Just assigned}

-- | Clear online.
unsetOnline :: DeviceContextUpdate
unsetOnline = Update \r -> r{Patrol.Type.DeviceContext.online = Nothing}

-- | Assign orientation. Text fields can be cleared with empty text.
setOrientation :: Text -> DeviceContextUpdate
setOrientation !assigned = Update \r -> r{Patrol.Type.DeviceContext.orientation = assigned}

-- | Assign processorCount.
setProcessorCount :: Int -> DeviceContextUpdate
setProcessorCount !assigned = Update \r -> r{Patrol.Type.DeviceContext.processorCount = Just assigned}

-- | Clear processorCount.
unsetProcessorCount :: DeviceContextUpdate
unsetProcessorCount = Update \r -> r{Patrol.Type.DeviceContext.processorCount = Nothing}

-- | Assign processorFrequency.
setProcessorFrequency :: Double -> DeviceContextUpdate
setProcessorFrequency !assigned = Update \r -> r{Patrol.Type.DeviceContext.processorFrequency = Just assigned}

-- | Clear processorFrequency.
unsetProcessorFrequency :: DeviceContextUpdate
unsetProcessorFrequency = Update \r -> r{Patrol.Type.DeviceContext.processorFrequency = Nothing}

-- | Assign screenDensity.
setScreenDensity :: Double -> DeviceContextUpdate
setScreenDensity !assigned = Update \r -> r{Patrol.Type.DeviceContext.screenDensity = Just assigned}

-- | Clear screenDensity.
unsetScreenDensity :: DeviceContextUpdate
unsetScreenDensity = Update \r -> r{Patrol.Type.DeviceContext.screenDensity = Nothing}

-- | Assign screenDpi.
setScreenDpi :: Double -> DeviceContextUpdate
setScreenDpi !assigned = Update \r -> r{Patrol.Type.DeviceContext.screenDpi = Just assigned}

-- | Clear screenDpi.
unsetScreenDpi :: DeviceContextUpdate
unsetScreenDpi = Update \r -> r{Patrol.Type.DeviceContext.screenDpi = Nothing}

-- | Assign screenResolution. Text fields can be cleared with empty text.
setScreenResolution :: Text -> DeviceContextUpdate
setScreenResolution !assigned = Update \r -> r{Patrol.Type.DeviceContext.screenResolution = assigned}

-- | Assign simulator.
setSimulator :: Bool -> DeviceContextUpdate
setSimulator !assigned = Update \r -> r{Patrol.Type.DeviceContext.simulator = Just assigned}

-- | Clear simulator.
unsetSimulator :: DeviceContextUpdate
unsetSimulator = Update \r -> r{Patrol.Type.DeviceContext.simulator = Nothing}

-- | Assign storageSize.
setStorageSize :: Int -> DeviceContextUpdate
setStorageSize !assigned = Update \r -> r{Patrol.Type.DeviceContext.storageSize = Just assigned}

-- | Clear storageSize.
unsetStorageSize :: DeviceContextUpdate
unsetStorageSize = Update \r -> r{Patrol.Type.DeviceContext.storageSize = Nothing}

-- | Assign supportsAccelerometer.
setSupportsAccelerometer :: Bool -> DeviceContextUpdate
setSupportsAccelerometer !assigned = Update \r -> r{Patrol.Type.DeviceContext.supportsAccelerometer = Just assigned}

-- | Clear supportsAccelerometer.
unsetSupportsAccelerometer :: DeviceContextUpdate
unsetSupportsAccelerometer = Update \r -> r{Patrol.Type.DeviceContext.supportsAccelerometer = Nothing}

-- | Assign supportsAudio.
setSupportsAudio :: Bool -> DeviceContextUpdate
setSupportsAudio !assigned = Update \r -> r{Patrol.Type.DeviceContext.supportsAudio = Just assigned}

-- | Clear supportsAudio.
unsetSupportsAudio :: DeviceContextUpdate
unsetSupportsAudio = Update \r -> r{Patrol.Type.DeviceContext.supportsAudio = Nothing}

-- | Assign supportsGyroscope.
setSupportsGyroscope :: Bool -> DeviceContextUpdate
setSupportsGyroscope !assigned = Update \r -> r{Patrol.Type.DeviceContext.supportsGyroscope = Just assigned}

-- | Clear supportsGyroscope.
unsetSupportsGyroscope :: DeviceContextUpdate
unsetSupportsGyroscope = Update \r -> r{Patrol.Type.DeviceContext.supportsGyroscope = Nothing}

-- | Assign supportsLocationService.
setSupportsLocationService :: Bool -> DeviceContextUpdate
setSupportsLocationService !assigned = Update \r -> r{Patrol.Type.DeviceContext.supportsLocationService = Just assigned}

-- | Clear supportsLocationService.
unsetSupportsLocationService :: DeviceContextUpdate
unsetSupportsLocationService = Update \r -> r{Patrol.Type.DeviceContext.supportsLocationService = Nothing}

-- | Assign supportsVibration.
setSupportsVibration :: Bool -> DeviceContextUpdate
setSupportsVibration !assigned = Update \r -> r{Patrol.Type.DeviceContext.supportsVibration = Just assigned}

-- | Clear supportsVibration.
unsetSupportsVibration :: DeviceContextUpdate
unsetSupportsVibration = Update \r -> r{Patrol.Type.DeviceContext.supportsVibration = Nothing}

-- | Assign timezone. Text fields can be cleared with empty text.
setTimezone :: Text -> DeviceContextUpdate
setTimezone !assigned = Update \r -> r{Patrol.Type.DeviceContext.timezone = assigned}

-- | Assign usableMemory.
setUsableMemory :: Int -> DeviceContextUpdate
setUsableMemory !assigned = Update \r -> r{Patrol.Type.DeviceContext.usableMemory = Just assigned}

-- | Clear usableMemory.
unsetUsableMemory :: DeviceContextUpdate
unsetUsableMemory = Update \r -> r{Patrol.Type.DeviceContext.usableMemory = Nothing}

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalBatteryLevel :: Maybe Double -> DeviceContextUpdate
setOptionalBatteryLevel = maybe unsetBatteryLevel setBatteryLevel

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalBootTime :: Maybe UTCTime -> DeviceContextUpdate
setOptionalBootTime = maybe unsetBootTime setBootTime

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalCharging :: Maybe Bool -> DeviceContextUpdate
setOptionalCharging = maybe unsetCharging setCharging

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalExternalFreeStorage :: Maybe Int -> DeviceContextUpdate
setOptionalExternalFreeStorage = maybe unsetExternalFreeStorage setExternalFreeStorage

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalExternalStorageSize :: Maybe Int -> DeviceContextUpdate
setOptionalExternalStorageSize = maybe unsetExternalStorageSize setExternalStorageSize

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalFreeMemory :: Maybe Int -> DeviceContextUpdate
setOptionalFreeMemory = maybe unsetFreeMemory setFreeMemory

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalFreeStorage :: Maybe Int -> DeviceContextUpdate
setOptionalFreeStorage = maybe unsetFreeStorage setFreeStorage

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalLowMemory :: Maybe Bool -> DeviceContextUpdate
setOptionalLowMemory = maybe unsetLowMemory setLowMemory

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalMemorySize :: Maybe Int -> DeviceContextUpdate
setOptionalMemorySize = maybe unsetMemorySize setMemorySize

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalOnline :: Maybe Bool -> DeviceContextUpdate
setOptionalOnline = maybe unsetOnline setOnline

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalProcessorCount :: Maybe Int -> DeviceContextUpdate
setOptionalProcessorCount = maybe unsetProcessorCount setProcessorCount

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalProcessorFrequency :: Maybe Double -> DeviceContextUpdate
setOptionalProcessorFrequency = maybe unsetProcessorFrequency setProcessorFrequency

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalScreenDensity :: Maybe Double -> DeviceContextUpdate
setOptionalScreenDensity = maybe unsetScreenDensity setScreenDensity

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalScreenDpi :: Maybe Double -> DeviceContextUpdate
setOptionalScreenDpi = maybe unsetScreenDpi setScreenDpi

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalSimulator :: Maybe Bool -> DeviceContextUpdate
setOptionalSimulator = maybe unsetSimulator setSimulator

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalStorageSize :: Maybe Int -> DeviceContextUpdate
setOptionalStorageSize = maybe unsetStorageSize setStorageSize

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalSupportsAccelerometer :: Maybe Bool -> DeviceContextUpdate
setOptionalSupportsAccelerometer = maybe unsetSupportsAccelerometer setSupportsAccelerometer

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalSupportsAudio :: Maybe Bool -> DeviceContextUpdate
setOptionalSupportsAudio = maybe unsetSupportsAudio setSupportsAudio

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalSupportsGyroscope :: Maybe Bool -> DeviceContextUpdate
setOptionalSupportsGyroscope = maybe unsetSupportsGyroscope setSupportsGyroscope

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalSupportsLocationService :: Maybe Bool -> DeviceContextUpdate
setOptionalSupportsLocationService = maybe unsetSupportsLocationService setSupportsLocationService

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalSupportsVibration :: Maybe Bool -> DeviceContextUpdate
setOptionalSupportsVibration = maybe unsetSupportsVibration setSupportsVibration

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalUsableMemory :: Maybe Int -> DeviceContextUpdate
setOptionalUsableMemory = maybe unsetUsableMemory setUsableMemory
