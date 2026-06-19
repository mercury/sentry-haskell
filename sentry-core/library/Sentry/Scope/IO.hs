module Sentry.Scope.IO (withScope, withIsolationScope) where

import Control.Exception (SomeAsyncException, SomeException, bracket, fromException, throwIO)
import Control.Exception.Annotated (AnnotatedException (..), Annotation (..))
import Control.Exception.Safe qualified as Safe
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
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
-- This is the @MonadUnliftIO@ variant; see "Sentry.Scope.Monad" for the
-- @MonadMask@\/@MonadIO@ version.
withScope :: forall m a. (MonadUnliftIO m) => (Scope -> m a) -> m a
withScope action = withRunInIO \run -> bracket acquire release \(_, scope) ->
  catchAndAnnotate (run (action scope)) Scope.readAmbientScope
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

-- | Like 'withScope', but operates on the isolation scope layer.
--
-- Use this at request or task boundaries (e.g. one isolation scope per
-- incoming HTTP request).
--
-- This is the @MonadUnliftIO@ variant; see "Sentry.Scope.Monad" for the
-- @MonadMask@\/@MonadIO@ version.
withIsolationScope :: forall m a. (MonadUnliftIO m) => (Scope -> m a) -> m a
withIsolationScope action = withRunInIO \run -> bracket acquire release \(_, _, scope) ->
  catchAndAnnotate (run (action scope)) Scope.readAmbientScope
  where
    -- Per the scopes spec, forking the isolation scope MUST also fork the
    -- current scope at the same time, so that current-scope mutations within
    -- the block are likewise isolated and users need not call both 'withScope'
    -- and 'withIsolationScope'. The action receives the isolation scope handle.
    acquire :: IO (Maybe Scope, Maybe Scope, Scope)
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

    release :: (Maybe Scope, Maybe Scope, Scope) -> IO ()
    release (parentIsolation, parentCurrent, _) =
      ThreadLocal.adjustContext \ctx ->
        let withIso = maybe (Scope.removeIsolation ctx) (`Scope.insertIsolation` ctx) parentIsolation
         in maybe (Scope.removeCurrent withIso) (`Scope.insertCurrent` withIso) parentCurrent

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
        let AnnotatedException anns inner =
              case fromException @(AnnotatedException SomeException) exn of
                Just ae -> ae
                Nothing -> AnnotatedException [] exn
        if
          | any (\(Annotation a) -> isJust (cast @_ @ScopeData a)) anns ->
              Safe.throwIO $ AnnotatedException anns inner
          | otherwise -> do
              merged <- mergeScopes
              Safe.throwIO $ AnnotatedException (Annotation merged : anns) inner
