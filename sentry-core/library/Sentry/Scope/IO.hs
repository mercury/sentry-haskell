module Sentry.Scope.IO (withScope, withIsolationScope, withClient) where

import Control.Exception (SomeAsyncException, SomeException, bracket, fromException, throwIO)
import Control.Exception.Annotated (AnnotatedException (..), Annotation (..))
import Control.Exception.Safe qualified as Safe
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Sentry.Client (Client)
import Sentry.Scope.Internal.Bracket qualified as Bracket
import Sentry.Scope.Operations (Scope, ScopeData)
import Sentry.Scope.Operations qualified as Scope

-- | Fork the current scope and pass it to the given action to be performed.
--
-- If a current scope already exists on the thread-local context it is cloned;
-- otherwise a new one is created.
--
-- The previous scope is restored when the action completes.
--
-- If the action throws a synchronous exception, the merged scope (global \<>
-- isolation \<> current) is attached as an 'Annotation' so that Sentry can
-- capture contextual metadata alongside the error.
--
-- This is the @MonadUnliftIO@ variant; see "Sentry.Scope.Monad" for the
-- @MonadMask@\/@MonadIO@ version.
withScope :: forall m a. (MonadUnliftIO m) => (Scope -> m a) -> m a
withScope action = withRunInIO \run -> bracket Bracket.acquireCurrent Bracket.releaseCurrent \(_, scope) ->
  catchAndAnnotate (run (action scope)) Scope.readAmbientScope

-- | Like 'withScope', but operates on the isolation scope layer.
--
-- Use this at request or task boundaries (e.g. one isolation scope per
-- incoming HTTP request).
--
-- This is the @MonadUnliftIO@ variant; see "Sentry.Scope.Monad" for the
-- @MonadMask@\/@MonadIO@ version.
withIsolationScope :: forall m a. (MonadUnliftIO m) => (Scope -> m a) -> m a
withIsolationScope action = withRunInIO \run -> bracket (Bracket.acquireIsolation Nothing) Bracket.releaseIsolation \(_, _, scope) ->
  catchAndAnnotate (run (action scope)) Scope.readAmbientScope

-- | Run an action with the given 'Client' bound to its isolation scope.
--
-- Forks fresh isolation and current scopes and binds the given 'Client' onto
-- the newly forked isolation scope, so capture and breadcrumb calls within the
-- enclosed action resolve to this client and the original client is restored
-- once the enclosing scope is exited. Any inherited current-scope client
-- binding is cleared on the clone; other metadata is retained. Deliberate
-- bindings made inside the action still follow normal scope precedence.
--
-- This is the @MonadUnliftIO@ variant; see "Sentry.Scope.Monad" for the
-- @MonadMask@\/@MonadIO@ version.
withClient :: forall m a. (MonadUnliftIO m) => Client -> m a -> m a
withClient client action = withRunInIO \run -> bracket (Bracket.acquireClient client) Bracket.releaseIsolation \_ ->
  run action

-- | Catch synchronous exceptions and annotate them with merged scope metadata.
--
-- Async exceptions are re-thrown immediately.
--
-- If the exception already carries a 'ScopeData' annotation, assume that it
-- was already caught and annotated by another handler and rethrow it unchanged
-- to preserve the innermost context.
catchAndAnnotate :: IO a -> IO ScopeData -> IO a
catchAndAnnotate action mergeScopes =
  action `Safe.catch` \(exn :: SomeException) ->
    case fromException @SomeAsyncException exn of
      Just _ -> throwIO exn
      Nothing -> do
        let (anns, inner, alreadyAnnotated) = Bracket.annotationFor exn
        if
          | alreadyAnnotated ->
              Safe.throwIO $ AnnotatedException anns inner
          | otherwise -> do
              merged <- mergeScopes
              Safe.throwIO $ AnnotatedException (Annotation merged : anns) inner
