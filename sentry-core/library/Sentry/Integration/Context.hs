-- | Context integration scaffold.
--
-- This integration runs once at client initialisation (via its 'setup' hook)
-- and fills in 'Sentry.Client.Options.ClientOptions.serverName' from the
-- current host's name when the option has not been set explicitly.
--
-- It is registered in 'Sentry.Client.builtinIntegrations' so it is installed
-- automatically when
-- 'Sentry.Client.Options.ClientOptions.defaultIntegrations' is @True@.
--
-- __NOTE__: the 'processEvent' hook is intentionally left blank for now.
--
-- OS, device, and runtime context payloads will be added in a future commit.
module Sentry.Integration.Context
  ( ContextIntegration (..),
  )
where

import Data.Kind (Type)
import Data.Text qualified as Text
import Network.BSD (getHostName)
import Sentry.Integration (Integration (..))
import Sentry.Internal (ClientOptions (..))

-- | Built-in integration that populates host-level context fields.
type ContextIntegration :: Type
data ContextIntegration = ContextIntegration
  deriving stock (Show)

instance Integration ContextIntegration where
  name _ = "ContextIntegration"

  -- \| Fill 'serverName' from the OS hostname when it has not been set
  -- explicitly.  Leaves all other fields untouched.
  setup _ opts
    | Just _ <- opts.serverName = pure opts
    | otherwise = do
        h <- getHostName
        pure opts{serverName = Just (Text.pack h)}
