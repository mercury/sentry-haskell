-- | Lifecycle helpers for initializing a Sentry 'Client' and ensuring the
-- transport is flushed and shut down on exit.
--
-- The primary entry points are 'withSentry' (which hands the constructed
-- 'Client' to a callback) and 'withSentryM' (which runs the callback inside
-- 'Sentry.Monad.SentryT' so the client is available via
-- 'Sentry.Monad.askClient').
--
-- Both guarantee that 'close' runs when the callback returns, whether normally
-- or via an exception — preventing the async transport from dropping queued
-- envelopes on a dirty exit.
module Sentry.Init
  ( -- * Lifecycle
    withSentry,
    withSentryM,

    -- * Shutdown
    close,
  )
where

import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_)
import Data.Time.Clock (NominalDiffTime)
import Sentry.Client (Client)
import Sentry.Client qualified as Client
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Monad (SentryT, runSentryT)
import Sentry.Transport (SomeTransport (..), Transport (..))

-- | Build a 'Client' from 'ClientOptions' (running the full initialization
-- lifecycle via 'Client.new'), run the callback with it, and guarantee that
-- 'close' is called on exit — whether the callback returns normally or raises
-- an exception.
--
-- This is the recommended way to initialize Sentry in an application. For
-- applications that use 'Sentry.Monad.SentryT', see 'withSentryM'.
withSentry :: (MonadIO m, MonadMask m) => ClientOptions -> (Client -> m a) -> m a
withSentry opts =
  bracket
    (liftIO $ Client.new opts)
    (\client -> liftIO $ close client.options.shutdownTimeout client)

-- | Like 'withSentry', but runs the body inside 'SentryT' so the 'Client' is
-- available via 'Sentry.Monad.askClient' throughout the callback.
withSentryM :: (MonadIO m, MonadMask m) => ClientOptions -> SentryT m a -> m a
withSentryM opts body = withSentry opts \client -> runSentryT client body

-- | Flush and shut down the 'Client's transport within @timeout@ seconds.
--
-- This mirrors the @close()@ operation from the Sentry SDK specification:
-- pending envelopes are delivered (flush), then the transport worker is
-- stopped (shutdown), both bounded by @timeout@.
--
-- A no-op when the client has no transport (e.g. 'Client.NON_RECORDING_CLIENT').
close :: NominalDiffTime -> Client -> IO ()
close timeout client =
  for_ client.transport \(SomeTransport t) -> do
    _ <- flush t timeout
    shutdown t timeout
