-- | SDK identity metadata for 'sentry.haskell'.
--
-- Constructs the 'Patrol.Type.ClientSdkInfo.ClientSdkInfo' that is attached to
-- every event emitted by this SDK, following Sentry's dotted naming convention
-- (@sentry.rust@, @sentry.python@, …).
module Sentry.Sdk
  ( sdkInfo,
  )
where

import Data.Foldable (toList)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Version (showVersion)
import Paths_sentry_core qualified
import Patrol.Type.ClientSdkInfo (ClientSdkInfo (..))
import Patrol.Type.ClientSdkInfo qualified as ClientSdkInfo
import Patrol.Type.ClientSdkPackage qualified as ClientSdkPackage
import Sentry.Integration (SomeIntegration)
import Sentry.Integration qualified as Integration

-- | The version string derived from the @sentry-core@ package metadata.
sdkVersion :: Text
sdkVersion = Text.pack $ showVersion Paths_sentry_core.version

-- | Construct the 'ClientSdkInfo' for this SDK.
--
-- * @name@ is @"sentry.haskell"@ (Sentry's dotted SDK naming convention).
-- * @version@ is derived from the @sentry-core@ package version at compile
--   time via 'Paths_sentry_core'.
-- * @packages@ contains a single entry referencing @sentry-core@ on Hackage.
-- * @integrations@ lists the 'Sentry.Integration.Integration.name' of every
--   installed integration, sorted alphabetically for deterministic payloads
--   (matching sentry-python's behavior).
sdkInfo :: Vector SomeIntegration -> ClientSdkInfo
sdkInfo integrations =
  ClientSdkInfo.empty
    { ClientSdkInfo.name = "sentry.haskell",
      ClientSdkInfo.version = sdkVersion,
      ClientSdkInfo.packages = [corePackage],
      ClientSdkInfo.integrations = sort $ map Integration.name (toList integrations)
    }
  where
    corePackage =
      ClientSdkPackage.empty
        { ClientSdkPackage.name = "hackage:sentry-core",
          ClientSdkPackage.version = sdkVersion
        }
