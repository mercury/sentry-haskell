module LifecycleTest where

import Control.Concurrent (ThreadId, forkFinally, killThread, yield)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Exception (AsyncException (..), Exception, MaskingState (..), SomeException, bracket, fromException, getMaskingState, throwIO, try)
import Control.Monad (replicateM_, void)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Foldable (traverse_)
import Data.IORef (IORef, newIORef, readIORef)
import Data.IORef qualified as IORef
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (NominalDiffTime)
import Data.Unique (newUnique)
import GHC.Conc (BlockReason (BlockedOnMVar), ThreadStatus (ThreadBlocked), threadStatus)
import Patrol.Type.Level qualified as Patrol.Level
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Client.Options.Dsn qualified as Dsn
import Sentry.Core qualified as Sentry
import Sentry.Scope.Internal qualified as Internal
import Sentry.Scope.Operations qualified as Scope
import Sentry.Test (withGlobalScope)
import Sentry.Test qualified as Test
import Sentry.Transport (FlushResponse (..), SendResponse (..), ShutdownResponse (..), SomeTransport (..), Transport (..))
import System.Timeout (timeout)
import Test.Hspec
import Witch qualified

spec_lifecycle :: Spec
spec_lifecycle = do
  describe "Sentry.init / Sentry.close" do
    it "shuts down without a preliminary flush on normal exit" do
      lt <- newLifecycleTransport
      let opts = def{transport = Just (Witch.from (SomeTransport lt)), dsn = Dsn.Explicit Test.TEST_DSN}
      withGlobalScope $ bracket (Sentry.init opts) Sentry.close \_ -> pure ()
      flushCount <- readIORef lt.flushes
      shutdownCount <- readIORef lt.shutdowns
      flushCount `shouldBe` 0
      shutdownCount `shouldBe` 1

    it "shuts down without a preliminary flush on exception, and rethrows" do
      lt <- newLifecycleTransport
      let opts = def{transport = Just (Witch.from (SomeTransport lt)), dsn = Dsn.Explicit Test.TEST_DSN}
      result <-
        try @SomeException $
          withGlobalScope $
            bracket (Sentry.init opts) Sentry.close \_ -> throwIO TestError
      case result of
        Right _ -> expectationFailure "expected exception to propagate"
        Left _ -> pure ()
      flushCount <- readIORef lt.flushes
      shutdownCount <- readIORef lt.shutdowns
      flushCount `shouldBe` 0
      shutdownCount `shouldBe` 1

    it "is a no-op on exit when there is no transport" do
      result <-
        withGlobalScope $
          bracket (Sentry.init def) Sentry.close \_ -> pure (42 :: Int)
      result `shouldBe` 42

    it "returns a client carrying the configured transport" do
      lt <- newLifecycleTransport
      let opts = def{transport = Just (Witch.from (SomeTransport lt)), dsn = Dsn.Explicit Test.TEST_DSN}
      withGlobalScope $ bracket (Sentry.init opts) Sentry.close \handle ->
        isJust (Sentry.clientOf handle).transport `shouldBe` True

    it "binds the client to the global scope so capture works without scopes" do
      transport <- Test.new
      let opts = def{transport = Just (Witch.from (SomeTransport transport)), dsn = Dsn.Explicit Test.TEST_DSN}
      withGlobalScope $
        bracket (Sentry.init opts) Sentry.close \_ ->
          () <$ Sentry.captureMessage Patrol.Level.Info "scope-free capture"
      events <- Test.fetchAndClearEvents transport
      length events `shouldBe` 1

  describe "owned bindings" do
    it "selects the newest client and never resurrects a closed predecessor" $ withGlobalScope do
      scope <- Scope.getGlobal
      a <- Sentry.init (namedOptions "A")
      b <- Sentry.init (namedOptions "B")
      assertBound scope (Just "B")
      Sentry.close a `shouldReturn` ShutdownSucceeded
      assertBound scope (Just "B")
      Sentry.close b `shouldReturn` ShutdownSucceeded
      assertBound scope Nothing

    it "restores the live predecessor, preserving metadata" $ withGlobalScope do
      scope <- Scope.getGlobal
      a <- Sentry.init (namedOptions "A")
      b <- Sentry.init (namedOptions "B")
      Scope.setTag scope "during" "B"
      void (Sentry.close b)
      assertBound scope (Just "A")
      snapshot <- Scope.readScopeRef scope
      snapshot.tags `shouldBe` Map.singleton "during" "B"
      void (Sentry.close a)
      assertBound scope Nothing

    it "restores nested withSentry bindings on normal and exceptional exit" $ withGlobalScope do
      scope <- Scope.getGlobal
      Sentry.withSentry (namedOptions "A") \_ -> do
        Sentry.withSentry (namedOptions "B") \_ -> assertBound scope (Just "B")
        assertBound scope (Just "A")
        result <- try @TestError @() $ Sentry.withSentry (namedOptions "B") \_ -> throwIO TestError
        result `shouldBe` Left TestError
        assertBound scope (Just "A")
      assertBound scope Nothing

    it "does not overwrite a direct replacement or clear" $ withGlobalScope do
      scope <- Scope.getGlobal
      a <- Sentry.init (namedOptions "A")
      bracket (Sentry.acquireClient (namedOptions "direct")) Sentry.close \direct -> do
        Scope.bindClient (Just (Sentry.clientOf direct)) scope
        void (Sentry.close a)
        assertBound scope (Just "direct")
        b <- Sentry.init (namedOptions "B")
        Scope.bindClient Nothing scope
        void (Sentry.close b)
        assertBound scope Nothing

    it "closes the original scope when called from a different global override" $ withGlobalScope do
      original <- Scope.getGlobal
      a <- Sentry.init (namedOptions "A")
      withWorker
        ( withGlobalScope do
            other <- Scope.getGlobal
            Sentry.withSentry (namedOptions "other") \_ -> do
              void (Sentry.close a)
              assertBound other (Just "other")
        )
        \_ wait -> expectSuccess =<< wait
      assertBound original Nothing

    it "resource-only acquisition and closure do not change ambient bindings" $ withGlobalScope do
      scope <- Scope.getGlobal
      Sentry.withSentry (namedOptions "ambient") \_ -> do
        bracket (Sentry.acquireClient (namedOptions "scoped")) Sentry.close \h -> do
          assertBound scope (Just "ambient")
          Sentry.withClient (Sentry.clientOf h) do
            client <- Scope.resolveClient
            client.options.release `shouldBe` Just "scoped"
          assertBound scope (Just "ambient")
        assertBound scope (Just "ambient")

    it "clones metadata without copying ownership" $ withGlobalScope do
      scope <- Scope.getGlobal
      a <- Sentry.init (namedOptions "A")
      cloned <- Scope.clone scope
      Scope.bindClient Nothing cloned
      assertBound scope (Just "A")
      void (Sentry.close a)
      assertBound cloned Nothing

    it "atomically preserves direct rebinding against stale release" $ withGlobalScope do
      scope <- Scope.getGlobal
      bracket (Sentry.acquireClient (namedOptions "direct")) Sentry.close \direct ->
        replicateM_ 50 do
          a <- Sentry.init (namedOptions "A")
          start <- newEmptyMVar
          withWorker (readMVar start >> Sentry.close a) \_ waitClose ->
            withWorker (readMVar start >> Scope.bindClient (Just (Sentry.clientOf direct)) scope) \_ waitBind -> do
              putMVar start ()
              expectSuccess =<< waitClose
              expectSuccess =<< waitBind
          assertBound scope (Just "direct")

    it "atomically preserves a new managed binding against stale release" $ withGlobalScope do
      scope <- Scope.getGlobal
      bracket (Sentry.acquireClient (namedOptions "B")) Sentry.close \b ->
        replicateM_ 50 do
          a <- Sentry.init (namedOptions "A")
          token <- newUnique
          start <- newEmptyMVar
          withWorker (readMVar start >> Sentry.close a) \_ waitClose ->
            withWorker (readMVar start >> Internal.bindManaged scope token (Sentry.clientOf b)) \_ waitBind -> do
              putMVar start ()
              expectSuccess =<< waitClose
              expectSuccess =<< waitBind
          assertBound scope (Just "B")
          Internal.unbindManaged scope token
          assertBound scope Nothing

  describe "scoped ownership" do
    it "captures with the override without changing the global client" $ withGlobalScope do
      transport <- Test.new
      Sentry.withSentry (namedOptions "global") \_ -> do
        global <- Scope.getGlobal
        Sentry.withScopedClient (transportOptions transport) do
          assertBound global (Just "global")
          void $ Sentry.captureMessage Patrol.Level.Info "scoped capture"
        assertBound global (Just "global")
      events <- Test.fetchAndClearEvents transport
      length events `shouldBe` 1

    it "overrides inherited current clients, nests, and restores metadata" $
      withGlobalScope $
        Sentry.withSentry (namedOptions "global") \parent ->
          Sentry.withScope \current -> do
            Scope.bindClient (Just parent) current
            Scope.setTag current "parent" "kept"
            Sentry.withScopedClient (namedOptions "outer") do
              assertResolved "outer"
              Sentry.withScope \child -> do
                snapshot <- Scope.readScopeRef child
                snapshot.tags `shouldBe` Map.singleton "parent" "kept"
                Scope.setTag child "child" "temporary"
                Sentry.withScopedClient (namedOptions "inner") $ assertResolved "inner"
                assertResolved "outer"
            assertResolved "global"
            snapshot <- Scope.readScopeRef current
            snapshot.tags `shouldBe` Map.singleton "parent" "kept"

    it "keeps concurrent overrides independent" $
      withGlobalScope $
        Sentry.withSentry (namedOptions "global") \_ -> do
          global <- Scope.getGlobal
          ready <- newEmptyMVar
          finish <- newEmptyMVar
          withWorker
            ( Sentry.withScopedClient (namedOptions "worker") do
                putMVar ready ()
                takeMVar finish
                assertResolved "worker"
            )
            \_ wait -> do
              within (takeMVar ready)
              Sentry.withScopedClient (namedOptions "caller") do
                assertResolved "caller"
                assertBound global (Just "global")
                putMVar finish ()
                expectSuccess =<< wait
              assertResolved "global"

    it "restores scopes before closing once on success" $
      withGlobalScope $
        Sentry.withSentry (namedOptions "parent") \_ -> do
          calls <- newIORef (0 :: Int)
          let t = ShutdownAction \_ -> do
                assertResolved "parent"
                IORef.modifyIORef' calls (+ 1)
                pure ShutdownSucceeded
          Sentry.withScopedClient (transportOptions t) (pure (42 :: Int)) `shouldReturn` 42
          readIORef calls `shouldReturn` 1

    it "restores and closes once on body failure, cancellation, or monadic abort" $
      withGlobalScope $
        Sentry.withSentry (namedOptions "parent") \_ -> do
          calls <- newIORef (0 :: Int)
          let t = ShutdownAction \_ -> do
                assertResolved "parent"
                IORef.modifyIORef' calls (+ 1)
                throwIO ThreadKilled
              opts = transportOptions t
          result <- try @TestError @() $ Sentry.withScopedClient opts (throwIO TestError)
          result `shouldBe` Left TestError
          cancelled <- try @AsyncException @() $ Sentry.withScopedClient opts (throwIO ThreadKilled)
          cancelled `shouldBe` Left ThreadKilled
          aborted <-
            runExceptT $
              Sentry.withScopedClient opts $
                ExceptT (pure (Left TestError :: Either TestError ()))
          aborted `shouldBe` Left TestError
          assertResolved "parent"
          readIORef calls `shouldReturn` 3

    it "preserves body exceptions over synchronous cleanup failures" $ withGlobalScope do
      let t = ShutdownAction (const (throwIO CleanupError))
      result <- try @TestError @() $ Sentry.withScopedClient (transportOptions t) (throwIO TestError)
      result `shouldBe` Left TestError
      isJust <$> Scope.lookupClient `shouldReturn` False

    it "propagates cleanup cancellation after success" $ withGlobalScope do
      let t = ShutdownAction (const (throwIO ThreadKilled))
      result <- try @AsyncException @() $ Sentry.withScopedClient (transportOptions t) (pure ())
      result `shouldBe` Left ThreadKilled
      isJust <$> Scope.lookupClient `shouldReturn` False

  describe "close results and cancellation" do
    it "enters shutdown masked before transports install cancellation cleanup" do
      observed <- newIORef Unmasked
      let t = ShutdownAction \_ -> do
            getMaskingState >>= IORef.writeIORef observed
            pure ShutdownSucceeded
      h <- Sentry.acquireClient (transportOptions t)
      Sentry.close h `shouldReturn` ShutdownSucceeded
      readIORef observed `shouldReturn` MaskedInterruptible

    it "returns the same failure on repeated close without retrying shutdown" do
      calls <- newIORef (0 :: Int)
      let response = ShutdownFailed_TimedOut 2
          t = ShutdownAction \_ -> do
            IORef.atomicModifyIORef' calls (\n -> (n + 1, ()))
            pure response
      h <- Sentry.acquireClient (transportOptions t)
      Sentry.close h `shouldReturn` response
      Sentry.close h `shouldReturn` response
      readIORef calls `shouldReturn` 1

    it "gives concurrent callers the same shutdown result" do
      entered <- newEmptyMVar
      finish <- newEmptyMVar
      calls <- newIORef (0 :: Int)
      let t = ShutdownAction \_ -> do
            IORef.atomicModifyIORef' calls (\n -> (n + 1, ()))
            putMVar entered ()
            takeMVar finish
            pure ShutdownSucceeded
      h <- Sentry.acquireClient (transportOptions t)
      withWorker (Sentry.close h) \_ waitOwner -> do
        within (takeMVar entered)
        withWorker (Sentry.close h) \waiter waitResult -> do
          awaitBlocked waiter
          putMVar finish ()
          expectRight ShutdownSucceeded =<< waitOwner
          expectRight ShutdownSucceeded =<< waitResult
      readIORef calls `shouldReturn` 1

    it "caches interruption, releases binding and wakes concurrent closers" $ withGlobalScope do
      scope <- Scope.getGlobal
      entered <- newEmptyMVar
      never <- newEmptyMVar @()
      let t = ShutdownAction \_ -> putMVar entered () >> takeMVar never >> pure ShutdownSucceeded
      h <- Sentry.init (transportOptions t)
      withWorker (Sentry.close h) \owner waitOwner -> do
        within (takeMVar entered)
        assertBound scope Nothing
        withWorker (Sentry.close h) \waiter waitResult -> do
          awaitBlocked waiter
          killThread owner
          expectException ThreadKilled =<< waitOwner
          expectRight interrupted =<< waitResult
      Sentry.close h `shouldReturn` interrupted

    it "cancelling a waiting closer does not cancel the owning close" do
      entered <- newEmptyMVar
      finish <- newEmptyMVar
      let t = ShutdownAction \_ -> putMVar entered () >> takeMVar finish >> pure ShutdownSucceeded
      h <- Sentry.acquireClient (transportOptions t)
      withWorker (Sentry.close h) \_ waitOwner -> do
        within (takeMVar entered)
        withWorker (Sentry.close h) \waiter waitResult -> do
          awaitBlocked waiter
          killThread waiter
          expectException ThreadKilled =<< waitResult
          putMVar finish ()
          expectRight ShutdownSucceeded =<< waitOwner
      Sentry.close h `shouldReturn` ShutdownSucceeded

    it "reports a synchronous shutdown exception and restores the binding" $ withGlobalScope do
      scope <- Scope.getGlobal
      let t = ShutdownAction (const (throwIO CleanupError))
      Sentry.withSentry (namedOptions "A") \_ -> do
        h <- Sentry.init (transportOptions t)
        response <- Sentry.close h
        case response of
          ShutdownFailed_Other message -> message `shouldSatisfy` Text.isPrefixOf "CleanupError"
          _ -> expectationFailure ("expected cleanup exception, got " <> show response)
        assertBound scope (Just "A")
        Sentry.close h `shouldReturn` response

    it "preserves the body exception when synchronous cleanup also fails" $ withGlobalScope do
      scope <- Scope.getGlobal
      let t = ShutdownAction (const (throwIO CleanupError))
      result <- try @TestError @() $ Sentry.withSentry (transportOptions t) \_ -> throwIO TestError
      result `shouldBe` Left TestError
      assertBound scope Nothing

    it "preserves the body exception when cleanup is cancelled" $ withGlobalScope do
      scope <- Scope.getGlobal
      let t = ShutdownAction (const (throwIO ThreadKilled))
      result <- try @TestError @() $ Sentry.withSentry (transportOptions t) \_ -> throwIO TestError
      result `shouldBe` Left TestError
      assertBound scope Nothing

    it "preserves body cancellation when cleanup also throws" $ withGlobalScope do
      let t = ShutdownAction (const (throwIO CleanupError))
      result <- try @AsyncException @() $ Sentry.withSentry (transportOptions t) \_ -> throwIO ThreadKilled
      result `shouldBe` Left ThreadKilled

    it "propagates cleanup cancellation after a successful body" $ withGlobalScope do
      scope <- Scope.getGlobal
      let t = ShutdownAction (const (throwIO ThreadKilled))
      result <- try @AsyncException @() $ Sentry.withSentry (transportOptions t) \_ -> pure ()
      result `shouldBe` Left ThreadKilled
      assertBound scope Nothing

    it "preserves monadic abort and still releases the handle" $ withGlobalScope do
      scope <- Scope.getGlobal
      let t = ShutdownAction (const (throwIO ThreadKilled))
      result <- runExceptT $ Sentry.withSentry (transportOptions t) \_ -> do
        liftIO $ assertBound scope (Just "transport")
        ExceptT (pure (Left TestError :: Either TestError ()))
      result `shouldBe` Left TestError
      assertBound scope Nothing

    it "passes the configured budget once and clamps negative budgets to zero" do
      budgets <- newIORef []
      let t = ShutdownAction \budget -> do
            IORef.atomicModifyIORef' budgets (\xs -> (xs <> [budget], ()))
            pure ShutdownSucceeded
      traverse_ (\budget -> bracket (Sentry.acquireClient ((transportOptions t){shutdownTimeout = budget})) Sentry.close (const (pure ()))) [2.5, 0, -1]
      readIORef budgets `shouldReturn` [2.5, 0, 0]

  describe "default transport shutdown" do
    traverse_
      ( \(response, expected) ->
          it ("propagates " <> show response) $
            shutdown (FlushOnly response) 1 `shouldReturn` expected
      )
      [ (FlushSucceeded, ShutdownSucceeded),
        (FlushFailed_TimedOut 1, ShutdownFailed_TimedOut 1),
        (FlushFailed_Shutdown, ShutdownFailed_AlreadyShutdown),
        (FlushFailed_Other "custom", ShutdownFailed_Other "custom"),
        (FlushFailed_QueueFull, ShutdownFailed_Other "Flush queue full")
      ]
    it "propagates delegated flush failure through close" do
      h <- Sentry.acquireClient (transportOptions (FlushOnly (FlushFailed_Other "custom")))
      Sentry.close h `shouldReturn` ShutdownFailed_Other "custom"

-- Helpers

-- | A 'Transport' that records flush and shutdown calls.
type LifecycleTransport :: Type
data LifecycleTransport = LifecycleTransport
  { flushes :: IORef Int,
    shutdowns :: IORef Int
  }

newLifecycleTransport :: IO LifecycleTransport
newLifecycleTransport = LifecycleTransport <$> newIORef 0 <*> newIORef 0

instance Transport LifecycleTransport where
  send _ _ = pure SendProcessed
  flush t _ = FlushSucceeded <$ IORef.modifyIORef' t.flushes (+ 1)
  shutdown t _ = ShutdownSucceeded <$ IORef.modifyIORef' t.shutdowns (+ 1)

type TestError :: Type
data TestError = TestError
  deriving stock (Eq, Show)

instance Exception TestError

type CleanupError :: Type
data CleanupError = CleanupError
  deriving stock (Show)

instance Exception CleanupError

type ShutdownAction :: Type
newtype ShutdownAction = ShutdownAction (NominalDiffTime -> IO ShutdownResponse)

instance Transport ShutdownAction where
  send _ _ = pure SendProcessed
  flush _ _ = throwIO (userError "unexpected preliminary flush")
  shutdown (ShutdownAction action) = action

type FlushOnly :: Type
newtype FlushOnly = FlushOnly FlushResponse

instance Transport FlushOnly where
  send _ _ = pure SendProcessed
  flush (FlushOnly response) _ = pure response

namedOptions :: Text -> ClientOptions
namedOptions name = def{release = Just name, defaultIntegrations = False}

transportOptions :: (Transport t) => t -> ClientOptions
transportOptions t = (namedOptions "transport"){dsn = Dsn.Explicit Test.TEST_DSN, transport = Just (Witch.from (SomeTransport t))}

assertBound :: Scope.Scope -> Maybe Text -> Expectation
assertBound scope expected = do
  snapshot <- Scope.readScopeRef scope
  (snapshot.client >>= (\c -> c.options.release)) `shouldBe` expected

interrupted :: ShutdownResponse
interrupted = ShutdownFailed_Other "Client close interrupted"

-- All worker waits have a generous watchdog; ordering is established by MVars,
-- not sleeps or scheduler timing. Brackets cancel workers on test failure.
withWorker :: IO a -> (ThreadId -> IO (Either SomeException a) -> IO b) -> IO b
withWorker action use = bracket spawn (killThread . fst) \(tid, result) -> use tid (within (readMVar result))
  where
    spawn = do
      result <- newEmptyMVar
      tid <- forkFinally action (putMVar result)
      pure (tid, result)

within :: IO a -> IO a
within action = timeout 5_000_000 action >>= maybe (throwIO (userError "lifecycle test watchdog expired")) pure

awaitBlocked :: ThreadId -> IO ()
awaitBlocked tid = within loop
  where
    loop =
      threadStatus tid >>= \case
        ThreadBlocked BlockedOnMVar -> pure ()
        _ -> yield >> loop

expectSuccess :: Either SomeException a -> Expectation
expectSuccess = either (expectationFailure . show) (const (pure ()))

expectRight :: (Eq a, Show a) => a -> Either SomeException a -> Expectation
expectRight expected = either (expectationFailure . show) (`shouldBe` expected)

expectException :: (Exception e, Eq e) => e -> Either SomeException a -> Expectation
expectException expected = \case
  Left exn -> fromException exn `shouldBe` Just expected
  Right _ -> expectationFailure "expected exception"

assertResolved :: Text -> Expectation
assertResolved expected = do
  client <- Scope.resolveClient
  client.options.release `shouldBe` Just expected
