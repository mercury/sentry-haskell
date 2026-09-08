-- | Explicit DSN selection. Import qualified as @Dsn@.
module Sentry.Client.Options.Dsn (DsnSource (..)) where

import Data.Kind (Type)
import Patrol qualified

-- | Inherit the environment, disable recording, or use a concrete DSN.
type DsnSource :: Type
data DsnSource
  = -- | Consult @SENTRY_DSN@ once during construction.
    Inherit
  | -- | Prevent recording, including after integration setup.
    Disabled
  | -- | Use this DSN regardless of the process environment.
    Explicit Patrol.Dsn
  deriving stock (Eq, Show)
