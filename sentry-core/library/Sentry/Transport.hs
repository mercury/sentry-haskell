module Sentry.Transport
  (
    -- * TODO: Documentation.
    Transport (..),
    SomeTransport (..),
  )
  where

import Data.Time.Clock (NominalDiffTime)
import Data.Kind (Constraint, Type)
import Patrol qualified

-- | TODO: Documentation.
type Transport :: Type -> Constraint
class Transport t where
  -- | TODO: Documentation.
  send :: t -> Patrol.Envelope -> IO ()
  -- | TODO: Documentation.
  flush :: t -> NominalDiffTime -> IO Bool
  -- | TODO: Documentation.
  shutdown :: t -> NominalDiffTime -> IO Bool

-- | TODO: Documentation.
type SomeTransport :: Type
data SomeTransport = forall t. Transport t => SomeTransport t

instance Transport SomeTransport where
  send (SomeTransport t) = send t
  flush (SomeTransport t) = flush t
  shutdown (SomeTransport t) = shutdown t

