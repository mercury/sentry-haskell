{-# LANGUAGE RequiredTypeArguments #-}

module Sentry.Client where

import Data.IORef (IORef)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Typeable (cast)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Sentry.Client.Options (ClientOptions)
import Sentry.Integration (Integration, SomeIntegration)
import Sentry.Transport (SomeTransport)
import Type.Reflection (SomeTypeRep, someTypeRep)

-- | The 'Client' is responsible for processing events and sending them to the
-- Sentry server via the 'Sentry.Transport.SomeTransport' it contains; it can
-- be created from 'Sentry.Client.Options.ClientOptions', which it retains a
-- copy of after construction.
type Client :: Type
data Client = Client
  { options :: ClientOptions,
    transport :: Maybe (IORef SomeTransport),
    integrations :: Vector (SomeTypeRep, SomeIntegration)
  }

-- | Attempt to find a 'Sentry.Integration.Integration' in the list of
-- 'Sentry.Integration.SomeIntegration' installed in the 'Client'.
getIntegration :: forall i -> (Integration i) => Client -> Maybe i
getIntegration iType client = do
  let iid = someTypeRep (Proxy @iType)
  (_, i) <- Vector.find (\(rep, _) -> rep == iid) client.integrations
  cast i
