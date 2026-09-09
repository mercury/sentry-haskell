module ScopeTest where

import Control.Concurrent (forkIO, myThreadId, newEmptyMVar, putMVar, takeMVar, throwTo)
import Control.Exception (AsyncException (ThreadKilled), SomeException, fromException)
import Control.Exception qualified as Exception
import Control.Exception.Annotated (AnnotatedException (..), Annotation (..))
import Control.Exception.Safe qualified as Safe
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Default (def)
import Data.Foldable (toList, traverse_)
import Data.Map.Strict qualified as Map
import Data.Typeable (cast)
import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Context.ThreadLocal qualified as ThreadLocal
import Patrol.Type.Breadcrumb qualified as Breadcrumb
import Patrol.Type.Level qualified as Level
import Sentry.Capture qualified as Capture
import Sentry.Client (Client)
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Scope (ScopeData (..), ScopeType (..))
import Sentry.Scope qualified as Scope
import Sentry.Scope.IO qualified as Scope.IO
import Sentry.Scope.Monad qualified as Scope.Monad
import Sentry.Test qualified as Test
import Test.Hspec

spec_Scope_IO :: Spec
spec_Scope_IO = describe "Scope.IO" $ scopeSpec Scope.IO.withScope Scope.IO.withIsolationScope Scope.IO.withClient

spec_Scope_Monad :: Spec
spec_Scope_Monad =
  describe "Scope.Monad" $
    scopeSpec
      (\action -> runReaderT (Scope.Monad.withScope (liftIO . action)) ())
      (\action -> runReaderT (Scope.Monad.withIsolationScope (liftIO . action)) ())
      (\client action -> runReaderT (Scope.Monad.withClient client (liftIO action)) ())

scopeSpec :: (forall a. (Scope.Scope -> IO a) -> IO a) -> (forall a. (Scope.Scope -> IO a) -> IO a) -> (forall a. Client -> IO a -> IO a) -> Spec
scopeSpec withCurrent withIsolation withBoundClient = do
  bindingSpec withCurrent withIsolation withBoundClient
  describe "withScope" do
    it "captures the enclosed scope and annotates exceptions with it" do
      result <- Safe.try @_ @(AnnotatedException SomeException) $ withCurrent \_ -> do
        Safe.throwIO $ userError "test exception"
      case result of
        Right _ -> expectationFailure "expected an exception to be thrown"
        Left (AnnotatedException anns _) -> do
          let scopes = immutableScopes anns
          scopes `shouldSatisfy` (not . null)
          case scopes of
            [] -> expectationFailure "no ScopeData annotation found"
            (scope : _) -> do
              scope.type_ `shouldBe` Just Merged

    it "nested calls do not add more than one scope annotation" do
      result <- Safe.try @_ @(AnnotatedException SomeException) $
        withCurrent \outer -> do
          Scope.setTag outer "location" "outer"
          withCurrent \inner -> do
            Scope.setTag inner "location" "inner"
            Safe.throwIO $ userError "nested test exception"
      case result of
        Right _ -> expectationFailure "expected an exception to be thrown"
        Left (AnnotatedException anns _) -> do
          let scopes = immutableScopes anns
          length scopes `shouldBe` 1
          map (.tags) scopes `shouldBe` [Map.singleton "location" "inner"]

    it "returns the value when no exception is thrown" do
      result <- withCurrent \_ -> pure (42 :: Int)
      result `shouldBe` 42

  describe "withIsolationScope" do
    it "captures the enclosed scope and annotates exceptions with it" do
      result <- Safe.try @_ @(AnnotatedException SomeException) $ withIsolation \_ -> do
        Safe.throwIO $ userError "test exception"
      case result of
        Right _ -> expectationFailure "expected an exception to be thrown"
        Left (AnnotatedException anns _) -> do
          let scopes = immutableScopes anns
          scopes `shouldSatisfy` (not . null)
          case scopes of
            [] -> expectationFailure "no ScopeData annotation found"
            (scope : _) -> do
              scope.type_ `shouldBe` Just Merged

    it "nested calls do not add more than one scope annotation" do
      result <- Safe.try @_ @(AnnotatedException SomeException) $
        withIsolation \_ ->
          withIsolation \_ ->
            Safe.throwIO $ userError "nested test exception"
      case result of
        Right _ -> expectationFailure "expected an exception to be thrown"
        Left (AnnotatedException anns _) -> do
          let scopes = immutableScopes anns
          length scopes `shouldBe` 1

    it "returns the value when no exception is thrown" do
      result <- withIsolation \_ -> pure (42 :: Int)
      result `shouldBe` 42

  describe "mixed nesting" do
    it "withScope inside withIsolationScope produces only one scope annotation" do
      result <- Safe.try @_ @(AnnotatedException SomeException) $
        withIsolation \_ ->
          withCurrent \_ ->
            Safe.throwIO $ userError "mixed nesting test"
      case result of
        Right _ -> expectationFailure "expected an exception to be thrown"
        Left (AnnotatedException anns _) -> do
          let scopes = immutableScopes anns
          length scopes `shouldBe` 1

    it "withIsolationScope inside withScope produces only one scope annotation" do
      result <- Safe.try @_ @(AnnotatedException SomeException) $
        withCurrent \_ ->
          withIsolation \_ ->
            Safe.throwIO $ userError "mixed nesting test"
      case result of
        Right _ -> expectationFailure "expected an exception to be thrown"
        Left (AnnotatedException anns _) -> do
          let scopes = immutableScopes anns
          length scopes `shouldBe` 1

  describe "async exceptions" do
    traverse_
      ( \(label, runScope) -> it (label <> " restores scope and preserves cancellation identity") $ Test.withGlobalScope do
          withIsolation \parent -> do
            Scope.setTag parent "parent" "retained"
            result <- Exception.try @SomeException $ runScope do
              ctx <- ThreadLocal.getContext
              case Scope.lookupCurrent ctx of
                Just child -> Scope.setTag child "child" "discarded"
                Nothing -> expectationFailure "expected current scope"
              tid <- myThreadId
              ready <- newEmptyMVar
              blocked <- newEmptyMVar
              _ <- forkIO $ takeMVar ready >> throwTo tid ThreadKilled
              putMVar ready ()
              takeMVar blocked
            case result of
              Right () -> expectationFailure "expected cancellation"
              Left exn -> fromException @Exception.AsyncException exn `shouldBe` Just ThreadKilled
            merged <- Scope.readAmbientScope
            merged.tags `shouldBe` Map.singleton "parent" "retained"
      )
      [ ("current", \action -> withCurrent (const action)),
        ("isolation", \action -> withIsolation (const action)),
        ("client", \action -> Test.new >>= \transport -> withBoundClient (Test.mkClient transport) action)
      ]

-- Helpers

immutableScopes :: [Annotation] -> [ScopeData]
immutableScopes anns = [s | Annotation a <- anns, Just s <- [cast @_ @ScopeData a]]

-- Both adapters must satisfy the same ownership and binding contract.
bindingSpec :: (forall a. (Scope.Scope -> IO a) -> IO a) -> (forall a. (Scope.Scope -> IO a) -> IO a) -> (forall a. Client -> IO a -> IO a) -> Spec
bindingSpec withCurrent withIsolation withBoundClient = do
  describe "restoration and ownership" do
    traverse_
      ( \(label, bracketScope) -> describe label do
          traverse_
            ( \fails -> it ("restores parents and unrelated context; failure=" <> show fails) $ Test.withGlobalScope do
                key <- Context.newKey "scope-test-unrelated"
                withIsolation \parentIso -> withCurrent \parentCurrent -> do
                  Scope.setTag parentIso "isolation" "parent"
                  Scope.setTag parentCurrent "current" "parent"
                  result <- Exception.try @SomeException $ bracketScope \child -> do
                    Scope.setTag child "child" "only"
                    childContext <- ThreadLocal.getContext
                    case Scope.lookupCurrent childContext of
                      Just childCurrent -> Scope.setTag childCurrent "current" "child"
                      Nothing -> expectationFailure "expected current scope"
                    ThreadLocal.adjustContext (Context.insert key ("changed" :: String))
                    if fails then Exception.throwIO (userError "restore") else pure ()
                  either (const fails) (const (not fails)) result `shouldBe` True
                  merged <- Scope.readAmbientScope
                  merged.tags `shouldBe` Map.fromList [("isolation", "parent"), ("current", "parent")]
                  ctx <- ThreadLocal.getContext
                  Context.lookup key ctx `shouldBe` Just "changed"
                  ThreadLocal.adjustContext (Context.delete key)
            )
            [False, True]
      )
      [("current", withCurrent), ("isolation", withIsolation)]

    it "supplied client wins, nested bindings restore, and parents retain metadata" $ Test.withGlobalScope do
      outer <- Test.new
      inner <- Test.new
      let outerClient = Test.mkClient outer
          innerClient = Test.mkCustomClient inner def{maxBreadcrumbs = 1}
      withCurrent \parent -> do
        Scope.bindClient (Just outerClient) parent
        Scope.setTag parent "parent" "retained"
        result <- Exception.try @SomeException $ withBoundClient innerClient do
          Scope.addBreadcrumbs [Breadcrumb.empty{Breadcrumb.message = "first"}, Breadcrumb.empty{Breadcrumb.message = "second"}]
          merged <- Scope.readAmbientScope
          map (.message) (toList merged.breadcrumbs) `shouldBe` ["second"]
          merged.tags `shouldBe` Map.singleton "parent" "retained"
          withBoundClient outerClient $ Capture.captureMessage_ Level.Info "nested"
          Capture.captureMessage_ Level.Info "inner"
          Exception.throwIO (userError "binding restore")
        either (const True) (const False) result `shouldBe` True
        Capture.captureMessage_ Level.Info "outer"
      innerEvents <- Test.fetchAndClearEvents inner
      outerEvents <- Test.fetchAndClearEvents outer
      length innerEvents `shouldBe` 1
      length outerEvents `shouldBe` 2

spec_transformerExit :: Spec
spec_transformerExit = it "Monad scope restores on ExceptT early exit" $ Test.withGlobalScope do
  Scope.IO.withScope \parent -> do
    Scope.setTag parent "parent" "retained"
    result <- runExceptT $ Scope.Monad.withIsolationScope \child -> do
      Scope.setTag child "child" "discarded"
      throwError ("early" :: String)
    (result :: Either String ()) `shouldBe` Left "early"
    merged <- Scope.readAmbientScope
    merged.tags `shouldBe` Map.singleton "parent" "retained"
