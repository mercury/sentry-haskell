module Sentry.Client.Options where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Data.Vector (Vector)
import Patrol qualified
import Sentry.Transport (SomeTransport)

-- | TODO: Documentation
type BeforeCallback :: Type -> Type
type BeforeCallback t = t -> Maybe t

-- | TODO: Documentation
type ClientOptions :: Type
data ClientOptions = ClientOptions
  { dsn :: Patrol.Dsn,
    debug :: Bool,
    release :: Maybe Text,
    environment :: Maybe Text,
    sampleRate :: Float,
    maxBreadcrumbs :: Word,
    serverName :: Maybe Text,
    inAppInclude :: Vector Text,
    inAppExclude :: Vector Text,
    integrations :: Vector (),
    defaultIntegrations :: Bool,
    beforeSend :: BeforeCallback Patrol.Event,
    beforeBreadcrumb :: BeforeCallback Patrol.Breadcrumb,
    transport :: SomeTransport,
    shutdownTimeout :: NominalDiffTime
  }
