module Sentry.Scope.Monad (withScope, withIsolationScope) where

import Control.Exception (SomeAsyncException, SomeException, fromException)
import Control.Exception.Annotated (AnnotatedException (..), Annotation (..))
import Control.Monad.Catch (MonadMask)
import Control.Monad.Catch qualified as MonadMask
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (isJust)
import Data.Typeable (cast)
import OpenTelemetry.Context.ThreadLocal qualified as ThreadLocal
import Sentry.Scope (Scope, ScopeData)
import Sentry.Scope qualified as Scope

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
withScope action = MonadMask.bracket acquire release \(_, scope) ->
  catchAndAnnotate (action scope) Scope.readAmbientScope
  where
    acquire :: m (Maybe Scope, Scope)
    acquire = do
      context <- ThreadLocal.getContext
      scope <- case Scope.lookupCurrent context of
        Nothing -> Scope.create Scope.Current
        Just s -> Scope.clone s
      let parentScope = Scope.lookupCurrent context
      ThreadLocal.adjustContext (Scope.insertCurrent scope)
      pure (parentScope, scope)

    release :: (Maybe Scope, Scope) -> m ()
    release (parentScope, _) =
      ThreadLocal.adjustContext \ctx ->
        maybe (Scope.removeCurrent ctx) (`Scope.insertCurrent` ctx) parentScope

-- | Like 'withScope', but operates on the isolation scope layer. Use this at
-- request or task boundaries (e.g. one isolation scope per incoming HTTP
-- request).
--
-- This is the @MonadMask@\/@MonadIO@ variant; see "Sentry.Scope.IO" for the
-- @MonadUnliftIO@ version.
withIsolationScope :: forall m a. (MonadMask m, MonadIO m) => (Scope -> m a) -> m a
withIsolationScope action = MonadMask.bracket acquire release \(_, _, scope) ->
  catchAndAnnotate (action scope) Scope.readAmbientScope
  where
    -- Per the scopes spec, forking the isolation scope MUST also fork the
    -- current scope at the same time, so that current-scope mutations within
    -- the block are likewise isolated and users need not call both 'withScope'
    -- and 'withIsolationScope'. The action receives the isolation scope handle.
    acquire :: m (Maybe Scope, Maybe Scope, Scope)
    acquire = do
      context <- ThreadLocal.getContext
      isolationScope <- case Scope.lookupIsolation context of
        Nothing -> Scope.create Scope.Isolation
        Just s -> Scope.clone s
      currentScope <- case Scope.lookupCurrent context of
        Nothing -> Scope.create Scope.Current
        Just s -> Scope.clone s
      let parentIsolation = Scope.lookupIsolation context
          parentCurrent = Scope.lookupCurrent context
      ThreadLocal.adjustContext (Scope.insertCurrent currentScope . Scope.insertIsolation isolationScope)
      pure (parentIsolation, parentCurrent, isolationScope)

    release :: (Maybe Scope, Maybe Scope, Scope) -> m ()
    release (parentIsolation, parentCurrent, _) =
      ThreadLocal.adjustContext \ctx ->
        let withIso = maybe (Scope.removeIsolation ctx) (`Scope.insertIsolation` ctx) parentIsolation
         in maybe (Scope.removeCurrent withIso) (`Scope.insertCurrent` withIso) parentCurrent

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
        let AnnotatedException anns inner =
              case fromException @(AnnotatedException SomeException) exn of
                Just ae -> ae
                Nothing -> AnnotatedException [] exn
        if
          | any (\(Annotation a) -> isJust (cast @_ @ScopeData a)) anns ->
              MonadMask.throwM $ AnnotatedException anns inner
          | otherwise -> do
              merged <- mergeScopes
              MonadMask.throwM $ AnnotatedException (Annotation merged : anns) inner
