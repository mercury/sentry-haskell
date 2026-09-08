-- | Explicit ownership of client resources, separate from ambient binding.
-- Use 'withSentry' for a process default or 'withScopedClient' for a temporary
-- scoped client. Bracket 'acquireClient' with 'close' for manual ownership.
-- Handles have no finalizer: callers must release them explicitly or through a bracket.
module Sentry.Init
  ( ClientHandle,
    acquireClient,
    clientOf,
    init,
    close,
    withSentry,
    withScopedClient,
  ) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Exception (SomeAsyncException, SomeException, displayException, fromException, mask_, onException, throwIO, try)
import Control.Monad (void)
import Control.Monad.Catch (ExitCase (..), MonadMask, generalBracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Kind (Type)
import Data.Text qualified as Text
import Data.Unique (Unique, newUnique)
import Sentry.Client (Client)
import Sentry.Client qualified as Client
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Scope qualified as Scope
import Sentry.Scope.Internal qualified as Internal
import Sentry.Scope.Monad qualified as Scope.Monad
import Sentry.Transport (ShutdownResponse (..), SomeTransport (..), Transport (..))
import Prelude hiding (init)

type HandleState :: Type
data HandleState = Open | Closing | Closed ShutdownResponse

-- | An explicitly owned client. Copies of a handle share one close operation.
-- A handle owns only its optional initialization binding, not other scopes
-- which callers may have bound to 'clientOf'.
type ClientHandle :: Type
data ClientHandle = ClientHandle
  { handleClient :: Client,
    handleBinding :: Maybe (Scope.Scope, Unique),
    handleState :: IORef HandleState,
    handleResult :: MVar ShutdownResponse
  }

-- | Obtain the client for capture or scoped binding. This does not transfer
-- resource ownership; release the original handle when finished using it.
clientOf :: ClientHandle -> Client
clientOf = (.handleClient)

-- | Construct an owned client without changing any ambient binding.
-- Use with 'close' in a bracket. Transport factories remain responsible for
-- cleaning up their own partially acquired resources if construction throws.
acquireClient :: ClientOptions -> IO ClientHandle
acquireClient opts = mask_ do
  state <- newIORef Open
  result <- newEmptyMVar
  client <- Client.new opts
  pure ClientHandle{handleClient = client, handleBinding = Nothing, handleState = state, handleResult = result}

-- | Acquire resources and install a managed process-default binding on the
-- currently resolved global scope. Closing restores the preceding live binding.
-- Closing an older handle cannot remove a newer binding, and closed handles
-- are never restored.
-- For concurrent scoped applications, prefer 'withScopedClient'.
init :: ClientOptions -> IO ClientHandle
init opts = mask_ do
  scope <- Scope.getGlobal
  token <- newUnique
  h <- acquireClient opts
  let bound = h{handleBinding = Just (scope, token)}
  Internal.bindManaged scope token h.handleClient `onException` void (close bound)
  pure bound

-- | Remove this handle's binding and shut down its transport exactly once.
-- Concurrent callers wait interruptibly for the same cached result. A client
-- with no transport closes successfully. Shutdown gets one nonnegative budget
-- from 'ClientOptions.shutdownTimeout'; no preliminary flush is performed.
--
-- Synchronous transport exceptions become 'ShutdownFailed_Other'. Cancellation
-- is rethrown after caching @ShutdownFailed_Other "Client close interrupted"@;
-- later calls return that failure without retrying an interrupted shutdown.
-- A cancelled waiter does not interrupt the owning close operation.
--
-- Custom transports, blocking cancellation, and connection cleanup can exceed
-- the budget. This is not a hard wall-clock deadline or a delivery guarantee.
close :: ClientHandle -> IO ShutdownResponse
close h = mask_ do
  decision <- atomicModifyIORef' h.handleState \s -> case s of
    Open -> (Closing, Left True)
    Closing -> (s, Left False)
    Closed result -> (s, Right result)
  case decision of
    Right result -> pure result
    Left False -> readMVar h.handleResult
    Left True -> do
      -- Everything after claiming ownership is covered, so cancellation or a
      -- failing custom transport cannot strand other callers in Closing.
      caught <- try do
        for_ h.handleBinding \(scope, token) -> Internal.unbindManaged scope token
        -- Enter shutdown masked so transports can install cleanup before cancellation.
        shutdownClient h.handleClient
      let result = case caught of
            Right response -> response
            Left (exn :: SomeException)
              | Just (_ :: SomeAsyncException) <- fromException exn ->
                  ShutdownFailed_Other "Client close interrupted"
              | otherwise -> ShutdownFailed_Other (Text.pack (displayException exn))
      putMVar h.handleResult result
      atomicModifyIORef' h.handleState (\_ -> (Closed result, ()))
      case caught of
        Left exn | Just (_ :: SomeAsyncException) <- fromException exn -> throwIO exn
        _ -> pure result

shutdownClient :: Client -> IO ShutdownResponse
shutdownClient client = case client.transport of
  Nothing -> pure ShutdownSucceeded
  Just (SomeTransport t) -> shutdown t (max 0 client.options.shutdownTimeout)

-- | Bracket an owned global binding, passing the client to the application.
-- Returned shutdown failures are discarded; use explicit handles and 'close'
-- when the result is needed. An application exception or monadic abort takes
-- precedence over cleanup exceptions, including cancellation during cleanup.
-- Cleanup cancellation after a successful body is propagated normally.
withSentry :: (MonadMask m, MonadIO m) => ClientOptions -> (Client -> m a) -> m a
withSentry opts = withOwnedClient (init opts)

-- | Own a temporary client bound to fresh isolation and current scopes.
-- Inherited metadata is retained, but an inherited current client cannot shadow
-- the override. Parent scopes are restored before the transport is closed.
-- The global binding is unchanged. Shutdown results are discarded, and body
-- exceptions or monadic aborts take precedence over cleanup exceptions, as in
-- 'withSentry'. Use 'acquireClient' and 'close' when shutdown status is needed.
withScopedClient :: (MonadMask m, MonadIO m) => ClientOptions -> m a -> m a
withScopedClient opts action =
  withOwnedClient (acquireClient opts) \client -> Scope.Monad.withClient client action

withOwnedClient :: (MonadMask m, MonadIO m) => IO ClientHandle -> (Client -> m a) -> m a
withOwnedClient acquire action = fst <$> generalBracket (liftIO acquire) release (action . clientOf)
  where
    release h exit = liftIO $ case exit of
      ExitCaseSuccess _ -> void (close h)
      ExitCaseException _ -> void (try @SomeException (close h))
      ExitCaseAbort -> void (try @SomeException (close h))
