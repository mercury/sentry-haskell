module Sentry.Scope.Monad where

import Control.Exception (SomeAsyncException, SomeException, fromException)
import Control.Exception.Annotated (AnnotatedException (..), Annotation (..))
import Control.Monad.Catch (MonadMask)
import Control.Monad.Catch qualified as MonadMask
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (isJust)
import Data.Typeable (cast)
import OpenTelemetry.Context.ThreadLocal qualified as ThreadLocal
import Sentry.Scope (ImmutableScope, Scope, unsafeReadScopeRef)
import Sentry.Scope qualified as Scope

-- | TODO: Documentation
withScope :: forall m a. (MonadMask m, MonadIO m) => (Scope -> m a) -> m a
withScope action = MonadMask.bracket acquire release \(_, scope) ->
  catchAndAnnotate (action scope) do
    global <- unsafeReadScopeRef Scope.global
    current <- unsafeReadScopeRef scope
    context <- ThreadLocal.getContext
    case Scope.lookupIsolation context of
      Nothing -> pure $ global <> current
      Just s -> do
        isolation <- unsafeReadScopeRef s
        pure $ global <> isolation <> current
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

-- | TODO: Documentation
withIsolationScope :: forall m a. (MonadMask m, MonadIO m) => (Scope -> m a) -> m a
withIsolationScope action = MonadMask.bracket acquire release \(_, scope) ->
  catchAndAnnotate (action scope) do
    global <- unsafeReadScopeRef Scope.global
    isolation <- unsafeReadScopeRef scope
    context <- ThreadLocal.getContext
    case Scope.lookupCurrent context of
      Nothing -> pure $ global <> isolation
      Just s -> do
        current <- unsafeReadScopeRef s
        pure $ global <> isolation <> current
  where
    acquire :: m (Maybe Scope, Scope)
    acquire = do
      context <- ThreadLocal.getContext
      scope <- case Scope.lookupIsolation context of
        Nothing -> Scope.create Scope.Isolation
        Just s -> Scope.clone s
      let parentScope = Scope.lookupIsolation context
      ThreadLocal.adjustContext (Scope.insertIsolation scope)
      pure (parentScope, scope)

    release :: (Maybe Scope, Scope) -> m ()
    release (parentScope, _) =
      ThreadLocal.adjustContext \ctx ->
        maybe (Scope.removeIsolation ctx) (`Scope.insertIsolation` ctx) parentScope

-- | TODO: Documentation
catchAndAnnotate :: (MonadMask m, MonadIO m) => m a -> m ImmutableScope -> m a
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
          | any (\(Annotation a) -> isJust (cast @_ @ImmutableScope a)) anns ->
              MonadMask.throwM $ AnnotatedException anns inner
          | otherwise -> do
              merged <- mergeScopes
              MonadMask.throwM $ AnnotatedException (Annotation merged : anns) inner
