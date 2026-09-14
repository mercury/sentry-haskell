-- | Internal scope acquisition and exception-annotation plumbing.
-- Call acquisition and release under the masking supplied by a bracket.
module Sentry.Scope.Internal.Bracket
  ( acquireCurrent,
    acquireIsolation,
    acquireClient,
    releaseCurrent,
    releaseIsolation,
    annotationFor,
  ) where

import Control.Exception (SomeException, fromException)
import Control.Exception.Annotated (AnnotatedException (..), Annotation (..))
import Data.Maybe (isJust)
import Data.Typeable (cast)
import OpenTelemetry.Context.ThreadLocal qualified as ThreadLocal
import Sentry.Client (Client)
import Sentry.Scope.Operations (Scope, ScopeData)
import Sentry.Scope.Operations qualified as Scope

-- | Clone the current layer and return its parent together with the child.
acquireCurrent :: IO (Maybe Scope, Scope)
acquireCurrent = do
  context <- ThreadLocal.getContext
  scope <- case Scope.lookupCurrent context of
    Nothing -> Scope.create Scope.Current
    Just s -> Scope.clone s
  let parentScope = Scope.lookupCurrent context
  ThreadLocal.adjustContext (Scope.insertCurrent scope)
  pure (parentScope, scope)

-- | Restore only the current key in the latest context.
releaseCurrent :: (Maybe Scope, Scope) -> IO ()
releaseCurrent (parentScope, _) =
  ThreadLocal.adjustContext \ctx ->
    maybe (Scope.removeCurrent ctx) (`Scope.insertCurrent` ctx) parentScope

-- | Clone both layers, optionally prepare a client binding, then install once.
acquireIsolation :: Maybe Client -> IO (Maybe Scope, Maybe Scope, Scope)
acquireIsolation binding = do
  context <- ThreadLocal.getContext
  isolationScope <- case Scope.lookupIsolation context of
    Nothing -> Scope.create Scope.Isolation
    Just s -> Scope.clone s
  currentScope <- case Scope.lookupCurrent context of
    Nothing -> Scope.create Scope.Current
    Just s -> Scope.clone s
  -- Prepare both scopes before installing either key.
  case binding of
    Nothing -> pure ()
    Just client -> do
      Scope.bindClient (Just client) isolationScope
      Scope.bindClient Nothing currentScope
  let parentIsolation = Scope.lookupIsolation context
      parentCurrent = Scope.lookupCurrent context
  ThreadLocal.adjustContext (Scope.insertCurrent currentScope . Scope.insertIsolation isolationScope)
  pure (parentIsolation, parentCurrent, isolationScope)

-- | Restore only the isolation and current keys in the latest context.
releaseIsolation :: (Maybe Scope, Maybe Scope, Scope) -> IO ()
releaseIsolation (parentIsolation, parentCurrent, _) =
  ThreadLocal.adjustContext \ctx ->
    let withIso = maybe (Scope.removeIsolation ctx) (`Scope.insertIsolation` ctx) parentIsolation
     in maybe (Scope.removeCurrent withIso) (`Scope.insertCurrent` withIso) parentCurrent

-- | Acquire a client binding without letting an inherited current client shadow it.
acquireClient :: Client -> IO (Maybe Scope, Maybe Scope, Scope)
acquireClient = acquireIsolation . Just

-- | Select existing annotations without reading scope data unless necessary.
annotationFor :: SomeException -> ([Annotation], SomeException, Bool)
annotationFor exn =
  let AnnotatedException anns inner =
        case fromException @(AnnotatedException SomeException) exn of
          Just ae -> ae
          Nothing -> AnnotatedException [] exn
   in (anns, inner, any (\(Annotation a) -> isJust (cast @_ @ScopeData a)) anns)
