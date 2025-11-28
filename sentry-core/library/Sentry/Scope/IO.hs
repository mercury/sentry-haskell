module Sentry.Scope.IO where

import Control.Exception (SomeAsyncException, SomeException, bracket, fromException, throwIO)
import Control.Exception.Annotated (AnnotatedException (..), Annotation (..))
import Control.Exception.Safe qualified as Safe
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Data.Maybe (isJust)
import Data.Typeable (cast)
import OpenTelemetry.Context.ThreadLocal qualified as ThreadLocal
import Sentry.Scope (ImmutableScope, Scope, unsafeReadScopeRef)
import Sentry.Scope qualified as Scope

-- | TODO: Documentation
catchAndAnnotate :: IO a -> IO ImmutableScope -> IO a
catchAndAnnotate action mergeScopes =
  action `Safe.catch` \(exn :: SomeException) ->
    case fromException @SomeAsyncException exn of
      Just _ -> throwIO exn
      Nothing -> do
        let AnnotatedException anns inner =
              case fromException @(AnnotatedException SomeException) exn of
                Just ae -> ae
                Nothing -> AnnotatedException [] exn
        if
          | any (\(Annotation a) -> isJust (cast @_ @ImmutableScope a)) anns ->
              Safe.throwIO $ AnnotatedException anns inner
          | otherwise -> do
              merged <- mergeScopes
              Safe.throwIO $ AnnotatedException (Annotation merged : anns) inner

-- | TODO: Documentation
withScope :: forall m a. (MonadUnliftIO m) => (Scope -> m a) -> m a
withScope action = withRunInIO \run -> bracket acquire release \(_, scope) ->
  catchAndAnnotate (run (action scope)) do
    global <- unsafeReadScopeRef Scope.global
    current <- unsafeReadScopeRef scope
    context <- ThreadLocal.getContext
    case Scope.lookupIsolation context of
      Nothing -> pure $ global <> current
      Just s -> do
        isolation <- unsafeReadScopeRef s
        pure $ global <> isolation <> current
  where
    acquire :: IO (Maybe Scope, Scope)
    acquire = do
      context <- ThreadLocal.getContext
      scope <- case Scope.lookupCurrent context of
        Nothing -> Scope.create Scope.Current
        Just s -> Scope.clone s
      let parentScope = Scope.lookupCurrent context
      ThreadLocal.adjustContext (Scope.insertCurrent scope)
      pure (parentScope, scope)

    release :: (Maybe Scope, Scope) -> IO ()
    release (parentScope, _) =
      ThreadLocal.adjustContext \ctx ->
        maybe (Scope.removeCurrent ctx) (`Scope.insertCurrent` ctx) parentScope

-- | TODO: Documentation
withIsolationScope :: forall m a. (MonadUnliftIO m) => (Scope -> m a) -> m a
withIsolationScope action = withRunInIO \run -> bracket acquire release \(_, scope) ->
  catchAndAnnotate (run (action scope)) do
    global <- unsafeReadScopeRef Scope.global
    isolation <- unsafeReadScopeRef scope
    context <- ThreadLocal.getContext
    case Scope.lookupCurrent context of
      Nothing -> pure $ global <> isolation
      Just s -> do
        current <- unsafeReadScopeRef s
        pure $ global <> isolation <> current
  where
    acquire :: IO (Maybe Scope, Scope)
    acquire = do
      context <- ThreadLocal.getContext
      scope <- case Scope.lookupIsolation context of
        Nothing -> Scope.create Scope.Isolation
        Just s -> Scope.clone s
      let parentScope = Scope.lookupIsolation context
      ThreadLocal.adjustContext (Scope.insertIsolation scope)
      pure (parentScope, scope)

    release :: (Maybe Scope, Scope) -> IO ()
    release (parentScope, _) =
      ThreadLocal.adjustContext \ctx ->
        maybe (Scope.removeIsolation ctx) (`Scope.insertIsolation` ctx) parentScope
