-- | Package-private representation. Public accessors cannot update a client.
module Sentry.Client.Internal where

import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Records (HasField (..))
import Patrol qualified
import Sentry.Client.Options (ClientOptions)
import Sentry.Client.Options qualified
import Sentry.Client.Options.Defaults qualified as Defaults
import Sentry.Client.Options.Dsn qualified as Dsn
import Sentry.Integration (SomeIntegration)
import Sentry.Transport (SomeTransport)

type RuntimeOptions :: Type
data RuntimeOptions = RuntimeOptions
  { debug :: Bool,
    sampleRate :: Float,
    environment :: Text,
    dsn :: Maybe Patrol.Dsn
  }

-- | Construct only from normalized options at the initialization boundary.
runtimeOptionsFromOptions :: ClientOptions -> RuntimeOptions
runtimeOptionsFromOptions opts =
  RuntimeOptions
    { debug = fromMaybe Defaults.debug opts.debug,
      sampleRate = fromMaybe Defaults.sampleRate opts.sampleRate,
      environment = fromMaybe Defaults.environment opts.environment,
      dsn = case opts.dsn of
        Dsn.Explicit value -> Just value
        _ -> Nothing
    }

-- | Initialized client; use the owned lifecycle helpers for application code.
type Client :: Type
data Client = Client ClientOptions (Maybe SomeTransport) (Vector SomeIntegration) RuntimeOptions

runtimeOptions :: Client -> RuntimeOptions
runtimeOptions (Client _ _ _ r) = r

-- | Finalized options supplied to integrations and callbacks.
options :: Client -> ClientOptions
options (Client o _ _ _) = o

-- | Realized transport, absent when recording is disabled.
transport :: Client -> Maybe SomeTransport
transport (Client _ t _ _) = t

-- | Selected, deduplicated integration roster.
integrations :: Client -> Vector SomeIntegration
integrations (Client _ _ i _) = i

instance HasField "options" Client ClientOptions where
  getField = options
instance HasField "transport" Client (Maybe SomeTransport) where
  getField = transport
instance HasField "integrations" Client (Vector SomeIntegration) where
  getField = integrations
