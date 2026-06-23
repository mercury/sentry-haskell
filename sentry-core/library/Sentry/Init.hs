-- | Lifecycle helpers for initializing a Sentry 'Client' and ensuring the
-- transport is flushed and shut down on exit.
--
-- 'init' builds a 'Client' and binds it onto the process-wide
-- 'Sentry.Scope.global' scope, so that capture and breadcrumb calls anywhere in
-- the process resolve it (see 'Sentry.Scope.resolveClient'). 'close' flushes and
-- shuts the transport down, then unbinds the client.
--
-- These functions can be called directly but most callers should prefer
-- 'withSentry', which brackets resource acquisition/release to guarantee that
-- all transports are given time to gracefully shut down before exiting:
--
-- @
-- withSentry opts \\_ -> app
-- @
--
-- __NOTE__: 'init' sets the /process default/ client.
-- 
-- For per-execution-context or concurrent clients (tests, multi-tenant), bind a
-- client onto the isolation scope using 'Sentry.Scope.IO.withClient' instead.
module Sentry.Init
  ( init,
    close,
    withSentry,
  )
where

import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_)
import Sentry.Client (Client)
import Sentry.Client qualified as Client
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Scope (bindClient, global)
import Sentry.Transport (SomeTransport (..), Transport (..))
import Prelude hiding (init)

-- | Build a 'Client' from 'ClientOptions' (running all the lifecycle hooks
-- first) and bind it onto the process-wide 'Sentry.Scope.global' scope.
--
-- __NOTE__: This function should be called no more than once throughout the
-- application's lifecycle; for concurrent or scoped clients, use one of the
-- @withClient@ helpers instead.
init :: ClientOptions -> IO Client
init opts = do
  client <- Client.new opts
  bindClient (Just client) global
  pure client

-- | Flush and shut down the 'Client's transport (bounded by
-- 'Sentry.Client.Options.ClientOptions.shutdownTimeout'), then unbind the
-- client from the 'Sentry.Scope.global' scope.
close :: Client -> IO ()
close client = do
  for_ client.transport \(SomeTransport t) -> do
    _ <- flush t client.options.shutdownTimeout
    shutdown t client.options.shutdownTimeout
  bindClient Nothing global

-- | Bracket the application with 'init' and 'close', passing the initialized
-- 'Client' to the callback.
--
-- The 'Client' is bound onto the process-wide 'Sentry.Scope.global' scope for
-- the duration of the callback, so capture and breadcrumb calls resolve it
-- without an explicit scope.
--
-- The transport is flushed and shut down before returning to the caller.
withSentry :: (MonadMask m, MonadIO m) => ClientOptions -> (Client -> m a) -> m a
withSentry opts = bracket (liftIO (init opts)) (liftIO . close)
