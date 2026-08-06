-- | The primary, "batteries-included" entry point for the SDK.
--
-- Re-exports the entire "Sentry.Core" surface, and overrides 'init' \/
-- 'withSentry' so they fill in
-- 'Sentry.Client.Options.ClientOptions.transport' with a default HTTP\/1.1
-- asynchronous transport ("Sentry.Transport.HTTP.Async") when the caller has
-- left it 'Nothing'. Code configuration always wins: set 'transport'
-- explicitly to opt out (a different transport, a tuned queue size, etc.).
--
-- @
-- import Sentry qualified as Sentry
--
-- main = Sentry.withSentry def{dsn = Just dsn} \\_client -> app
-- @
--
-- Reach for "Sentry.Core" directly (from @sentry-core@) instead when you need
-- a transport-agnostic client — integration authors, custom-transport
-- authors, or tests that don't want the HTTP dependency footprint.
module Sentry
  ( -- * Lifecycle (default-transport wrappers)
    init,
    withSentry,

    -- * Everything else, unchanged, from "Sentry.Core"
    module Sentry.Core,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Data.Default (def)
import Sentry.Core hiding (init, withSentry)
import Sentry.Core qualified as Core
import Sentry.Transport.Executor.Async qualified as AsyncExecutor
import Sentry.Transport.HTTP.Async qualified as Http1
import Prelude hiding (init)

-- | Fill in 'ClientOptions.transport' with the default HTTP\/1.1 async
-- transport if the caller left it unset. An explicitly-configured transport
-- is left untouched.
defaultTransport :: ClientOptions -> ClientOptions
defaultTransport opts =
  opts{transport = opts.transport <|> Just (Http1.new def AsyncExecutor.defaultQueueSize)}

-- | Like 'Sentry.Core.init', but defaults 'ClientOptions.transport' to the
-- HTTP\/1.1 async transport when unset.
init :: ClientOptions -> IO Client
init opts = Core.init (defaultTransport opts)

-- | Like 'Sentry.Core.withSentry', but defaults 'ClientOptions.transport' to
-- the HTTP\/1.1 async transport when unset.
withSentry :: (MonadMask m, MonadIO m) => ClientOptions -> (Client -> m a) -> m a
withSentry opts = Core.withSentry (defaultTransport opts)
