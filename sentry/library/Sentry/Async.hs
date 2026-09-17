-- | Scope-propagating async helper functions in 'MonadUnliftIO'.
module Sentry.Async
  ( forkScopedIO,
    asyncScoped,
    withAsyncScoped,
    concurrentlyScoped,
    mapConcurrentlyScoped,
    forConcurrentlyScoped,
  )
where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.Async (Async, async, concurrently, mapConcurrently, withAsync)
import Sentry.Scope.Operations (propagateScope)
import UnliftIO (MonadUnliftIO, withRunInIO)

-- | Fork a thread with that inherits the caller's isolation scope and starts
-- with a fresh clone of the current scope.
forkScopedIO :: (MonadUnliftIO m) => m () -> m ThreadId
forkScopedIO action = withRunInIO \runInIO -> do
  wrapped <- propagateScope (runInIO action)
  forkIO wrapped

-- | Like 'async', but the forked action inherits the caller's isolation scope
-- and starts with a fresh clone of the current scope.
asyncScoped :: (MonadUnliftIO m) => m a -> m (Async a)
asyncScoped action = withRunInIO \runInIO -> do
  wrapped <- propagateScope (runInIO action)
  async wrapped

-- | Like 'withAsync', but the forked action inherits the caller's isolation
-- scope and starts with a fresh clone of the current scope.
withAsyncScoped :: (MonadUnliftIO m) => m a -> (Async a -> m b) -> m b
withAsyncScoped action k = withRunInIO \runInIO -> do
  wrapped <- propagateScope (runInIO action)
  withAsync wrapped (runInIO . k)

-- | Like 'concurrently', but each forked action inherits the caller's
-- isolation scope and starts with fresh clones of the current scope.
concurrentlyScoped :: (MonadUnliftIO m) => m a -> m b -> m (a, b)
concurrentlyScoped left right = withRunInIO \runInIO -> do
  left' <- propagateScope (runInIO left)
  right' <- propagateScope (runInIO right)
  concurrently left' right'

-- | Like 'mapConcurrently', but each forked action inherits the caller's
-- isolation scope and starts with fresh clones of the current scope.
--
-- __NOTE__: Every element's context is captured sequentially in the caller,
-- via 'traverse', before any thread is spawned, so there is no race between
-- capturing a context and forking the thread that will attach it.
mapConcurrentlyScoped :: (MonadUnliftIO m, Traversable t) => (a -> m b) -> t a -> m (t b)
mapConcurrentlyScoped f xs = withRunInIO \runInIO -> do
  wrapped <- traverse (propagateScope . runInIO . f) xs
  mapConcurrently id wrapped

-- | 'mapConcurrentlyScoped' with its arguments flipped.
forConcurrentlyScoped :: (MonadUnliftIO m, Traversable t) => t a -> (a -> m b) -> m (t b)
forConcurrentlyScoped = flip mapConcurrentlyScoped
