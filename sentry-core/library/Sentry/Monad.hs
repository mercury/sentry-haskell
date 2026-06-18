-- | Constraints for an environment that can provide a Sentry 'Client', along
-- with a basic 'SentryT' transformer that can be used to derive instances with
-- @DerivingVia@.
module Sentry.Monad
  ( -- * Capability
    HasClient (..),

    -- * Helpers
    askClient,
    withClient,

    -- * Concrete carrier
    SentryT (..),
    runSentryT,
  )
where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), asks, local)
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Constraint, Type)
import Sentry.Client (Client)

-- | Typeclass for environments that embed a 'Client'.
--
-- Declare an instance for your application environment to enable all
-- Sentry operations in any @'MonadReader' env m@:
--
-- @
-- data Env = Env { client :: Client, ... }
--
-- instance HasClient Env where
--   clientL f env = (\\client -> env{ client c }) \<$\> f env.client
-- @
type HasClient :: Type -> Constraint
class HasClient env where
  -- | A van Laarhoven lens focusing on the 'Client' within @env@.
  clientL :: Lens' env Client

-- | The trivial instance: a 'Client' environment /is/ the 'Client'.
instance HasClient Client where
  clientL f = f

-- | Retrieve the 'Client' from the ambient reader environment.
askClient :: (MonadReader env m, HasClient env) => m Client
askClient = asks (view clientL)

-- | Run an action with a different ambient 'Client', restoring the previous
-- one on return.
--
-- @
-- withSentry opts{dsn = Just otherDsn} \\c ->
--   withClient c do
--     captureMessage "routed to otherDsn"
-- @
withClient :: (MonadReader env m, HasClient env) => Client -> m a -> m a
withClient client = local (set clientL client)

-- | A concrete 'ReaderT'-based monad transformer that carries a 'Client'.
type SentryT :: (Type -> Type) -> Type -> Type
newtype SentryT m a = SentryT (ReaderT Client m a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Client,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadUnliftIO
    )

type role SentryT representational nominal

-- | Run a 'SentryT' action with the given 'Client'.
runSentryT :: Client -> SentryT m a -> m a
runSentryT c (SentryT r) = runReaderT r c

-- | A minimal lens type, compatible with most optics constructions.
type Lens' :: Type -> Type -> Type
type Lens' s a = forall f. (Functor f) => (a -> f a) -> s -> f s

-- | Project the focus of a 'Lens''.
view :: Lens' s a -> s -> a
view l s = getConst (l Const s)

-- | Replace the focus of a 'Lens''.
set :: Lens' s a -> a -> s -> s
set l a s = runIdentity (l (const (Identity a)) s)
