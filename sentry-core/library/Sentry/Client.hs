{-# LANGUAGE RequiredTypeArguments #-}

module Sentry.Client
  ( -- * Client
    Client (..),
    pattern NON_RECORDING_CLIENT,
    getIntegration,
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (cast)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Sentry.Client.Options (ClientOptions (..), pattern DEFAULT_CLIENT_OPTIONS)
import Sentry.Integration (Integration (..), SomeIntegration (..))
import Sentry.Transport (SomeTransport)
import Type.Reflection (someTypeRep)
import Witch qualified

-- | The 'Client' is responsible for processing events and sending them to the
-- Sentry server via the 'Sentry.Transport.SomeTransport' it contains; it can
-- be created from 'Sentry.Client.Options.ClientOptions', which it retains a
-- copy of after construction.
type Client :: Type
data Client = Client
  { options :: ClientOptions,
    transport :: Maybe SomeTransport,
    integrations :: Vector SomeIntegration
  }

instance Witch.From ClientOptions Client where
  from options@ClientOptions{transport, integrations} =
    Client
      { options,
        transport,
        integrations
      }

-- | Attempt to find a 'Sentry.Integration.Integration' in the list of
-- 'Sentry.Integration.SomeIntegration' installed in the 'Client'.
getIntegration :: forall i -> (Integration i) => Client -> Maybe i
getIntegration iType client = do
  let iid = someTypeRep (Proxy @iType)
  (SomeIntegration _ i) <- Vector.find (\(SomeIntegration rep _) -> rep == iid) client.integrations
  cast i

-- | Any client which does not have a valid 'Transport' is non-recording.
pattern NON_RECORDING_CLIENT :: Client
pattern NON_RECORDING_CLIENT <- Client{transport = Nothing}
  where
    NON_RECORDING_CLIENT =
      Client
        { options = DEFAULT_CLIENT_OPTIONS,
          transport = Nothing,
          integrations = Vector.empty
        }
