module Sentry.Scope.Monad (withScope, withIsolationScope, withClient) where

import Control.Exception (SomeAsyncException, SomeException, fromException)
import Control.Exception.Annotated (AnnotatedException (..), Annotation (..))
import Control.Monad.Catch (MonadMask)
import Control.Monad.Catch qualified as MonadMask
import Control.Monad.IO.Class (MonadIO, liftIO)
import Sentry.Client (Client)
import Sentry.Scope (Scope, ScopeData)
import Sentry.Scope qualified as Scope
import Sentry.Scope.Internal.Bracket qualified as Bracket

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
-- This is the @MonadMask@\/@MonadIO@ variant; see "Sentry.Scope.IO" for the
-- @MonadUnliftIO@ version.
withScope :: forall m a. (MonadMask m, MonadIO m) => (Scope -> m a) -> m a
withScope action = MonadMask.bracket (liftIO Bracket.acquireCurrent) (liftIO . Bracket.releaseCurrent) \(_, scope) ->
  catchAndAnnotate (action scope) Scope.readAmbientScope

-- | Like 'withScope', but operates on the isolation scope layer. Use this at
-- request or task boundaries (e.g. one isolation scope per incoming HTTP
-- request).
--
-- This is the @MonadMask@\/@MonadIO@ variant; see "Sentry.Scope.IO" for the
-- @MonadUnliftIO@ version.
withIsolationScope :: forall m a. (MonadMask m, MonadIO m) => (Scope -> m a) -> m a
withIsolationScope action = MonadMask.bracket (liftIO $ Bracket.acquireIsolation Nothing) (liftIO . Bracket.releaseIsolation) \(_, _, scope) ->
  catchAndAnnotate (action scope) Scope.readAmbientScope

-- | Run an action with the given 'Client' bound for its dynamic extent.
--
-- Forks fresh isolation and current scopes and binds the given 'Client' onto
-- the newly forked isolation scope, so capture and breadcrumb calls within the
-- enclosed action resolve to this client and the original client is restored
-- once the enclosing scope is exited. Any inherited current-scope client
-- binding is cleared on the clone; other metadata is retained. Deliberate
-- bindings made inside the action still follow normal scope precedence.
--
-- This is the @MonadMask@\/@MonadIO@ variant; see "Sentry.Scope.IO" for the
-- @MonadUnliftIO@ version.
withClient :: forall m a. (MonadMask m, MonadIO m) => Client -> m a -> m a
withClient client action = MonadMask.bracket (liftIO $ Bracket.acquireClient client) (liftIO . Bracket.releaseIsolation) \_ ->
  action

-- | Catch synchronous exceptions and annotate them with merged scope metadata.
-- Async exceptions are re-thrown immediately. If the exception already carries
-- an 'ScopeData' annotation (from a nested scope) it is left unchanged to
-- preserve the innermost context.
catchAndAnnotate :: (MonadMask m, MonadIO m) => m a -> m ScopeData -> m a
catchAndAnnotate action mergeScopes =
  action `MonadMask.catch` \(exn :: SomeException) ->
    case fromException @SomeAsyncException exn of
      Just _ -> MonadMask.throwM exn
      Nothing -> do
        let (anns, inner, alreadyAnnotated) = Bracket.annotationFor exn
        if
          | alreadyAnnotated ->
              MonadMask.throwM $ AnnotatedException anns inner
          | otherwise -> do
              merged <- mergeScopes
              MonadMask.throwM $ AnnotatedException (Annotation merged : anns) inner
