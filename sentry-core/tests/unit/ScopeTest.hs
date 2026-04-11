module ScopeTest where

import Control.Concurrent (forkIO, myThreadId, threadDelay, throwTo)
import Control.Exception (AsyncException (ThreadKilled), SomeException, fromException)
import Control.Exception qualified as Exception
import Control.Exception.Annotated (AnnotatedException (..), Annotation (..))
import Control.Exception.Safe qualified as Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Typeable (cast)
import Sentry.Scope (ScopeData (..), ScopeType (..))
import Sentry.Scope.IO qualified as Scope.IO
import Sentry.Scope.Monad qualified as Scope.Monad
import Test.Hspec

spec_Scope_IO :: Spec
spec_Scope_IO = describe "Scope.IO" do
  describe "withScope" do
    it "captures the enclosed scope and annotates exceptions with it" do
      result <- Safe.try @_ @(AnnotatedException SomeException) $ Scope.IO.withScope \_ -> do
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
        Scope.IO.withScope \_ ->
          Scope.IO.withScope \_ ->
            Safe.throwIO $ userError "nested test exception"
      case result of
        Right _ -> expectationFailure "expected an exception to be thrown"
        Left (AnnotatedException anns _) -> do
          let scopes = immutableScopes anns
          length scopes `shouldBe` 1

    it "returns the value when no exception is thrown" do
      result <- Scope.IO.withScope \_ -> pure (42 :: Int)
      result `shouldBe` 42

  describe "withIsolationScope" do
    it "captures the enclosed scope and annotates exceptions with it" do
      result <- Safe.try @_ @(AnnotatedException SomeException) $ Scope.IO.withIsolationScope \_ -> do
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
        Scope.IO.withIsolationScope \_ ->
          Scope.IO.withIsolationScope \_ ->
            Safe.throwIO $ userError "nested test exception"
      case result of
        Right _ -> expectationFailure "expected an exception to be thrown"
        Left (AnnotatedException anns _) -> do
          let scopes = immutableScopes anns
          length scopes `shouldBe` 1

    it "returns the value when no exception is thrown" do
      result <- Scope.IO.withIsolationScope \_ -> pure (42 :: Int)
      result `shouldBe` 42

  describe "mixed nesting" do
    it "withScope inside withIsolationScope produces only one scope annotation" do
      result <- Safe.try @_ @(AnnotatedException SomeException) $
        Scope.IO.withIsolationScope \_ ->
          Scope.IO.withScope \_ ->
            Safe.throwIO $ userError "mixed nesting test"
      case result of
        Right _ -> expectationFailure "expected an exception to be thrown"
        Left (AnnotatedException anns _) -> do
          let scopes = immutableScopes anns
          length scopes `shouldBe` 1

    it "withIsolationScope inside withScope produces only one scope annotation" do
      result <- Safe.try @_ @(AnnotatedException SomeException) $
        Scope.IO.withScope \_ ->
          Scope.IO.withIsolationScope \_ ->
            Safe.throwIO $ userError "mixed nesting test"
      case result of
        Right _ -> expectationFailure "expected an exception to be thrown"
        Left (AnnotatedException anns _) -> do
          let scopes = immutableScopes anns
          length scopes `shouldBe` 1

  describe "async exceptions" do
    it "does not annotate async exceptions with ScopeData" do
      result <- Exception.try @SomeException $ Scope.IO.withScope \_ -> do
        tid <- myThreadId
        _ <- forkIO $ throwTo tid ThreadKilled
        threadDelay maxBound
      case result of
        Right _ -> expectationFailure "expected an exception to be thrown"
        Left exn ->
          case fromException @(AnnotatedException SomeException) exn of
            Just (AnnotatedException anns _) ->
              immutableScopes anns `shouldSatisfy` null
            Nothing ->
              pure ()

spec_Scope_Monad :: Spec
spec_Scope_Monad = describe "Scope.Monad" do
  describe "withScope" do
    it "captures the enclosed scope and annotates exceptions with it" do
      result <- Safe.try @_ @(AnnotatedException SomeException) $ runM $ Scope.Monad.withScope \_ -> do
        liftIO $ Safe.throwIO $ userError "test exception"
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
        runM $
          Scope.Monad.withScope \_ ->
            Scope.Monad.withScope \_ ->
              liftIO $ Safe.throwIO $ userError "nested test exception"
      case result of
        Right _ -> expectationFailure "expected an exception to be thrown"
        Left (AnnotatedException anns _) -> do
          let scopes = immutableScopes anns
          length scopes `shouldBe` 1

    it "returns the value when no exception is thrown" do
      result <- runM $ Scope.Monad.withScope \_ -> pure (42 :: Int)
      result `shouldBe` 42

  describe "withIsolationScope" do
    it "captures the enclosed scope and annotates exceptions with it" do
      result <- Safe.try @_ @(AnnotatedException SomeException) $ runM $ Scope.Monad.withIsolationScope \_ -> do
        liftIO $ Safe.throwIO $ userError "test exception"
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
        runM $
          Scope.Monad.withIsolationScope \_ ->
            Scope.Monad.withIsolationScope \_ ->
              liftIO $ Safe.throwIO $ userError "nested test exception"
      case result of
        Right _ -> expectationFailure "expected an exception to be thrown"
        Left (AnnotatedException anns _) -> do
          let scopes = immutableScopes anns
          length scopes `shouldBe` 1

    it "returns the value when no exception is thrown" do
      result <- runM $ Scope.Monad.withIsolationScope \_ -> pure (42 :: Int)
      result `shouldBe` 42

  describe "mixed nesting" do
    it "withScope inside withIsolationScope produces only one scope annotation" do
      result <- Safe.try @_ @(AnnotatedException SomeException) $
        runM $
          Scope.Monad.withIsolationScope \_ ->
            Scope.Monad.withScope \_ ->
              liftIO $ Safe.throwIO $ userError "mixed nesting test"
      case result of
        Right _ -> expectationFailure "expected an exception to be thrown"
        Left (AnnotatedException anns _) -> do
          let scopes = immutableScopes anns
          length scopes `shouldBe` 1

    it "withIsolationScope inside withScope produces only one scope annotation" do
      result <- Safe.try @_ @(AnnotatedException SomeException) $
        runM $
          Scope.Monad.withScope \_ ->
            Scope.Monad.withIsolationScope \_ ->
              liftIO $ Safe.throwIO $ userError "mixed nesting test"
      case result of
        Right _ -> expectationFailure "expected an exception to be thrown"
        Left (AnnotatedException anns _) -> do
          let scopes = immutableScopes anns
          length scopes `shouldBe` 1

  describe "async exceptions" do
    it "does not annotate async exceptions with ScopeData" do
      result <- Exception.try @SomeException $ runM $ Scope.Monad.withScope \_ -> liftIO do
        tid <- myThreadId
        _ <- forkIO $ throwTo tid ThreadKilled
        threadDelay maxBound
      case result of
        Right _ -> expectationFailure "expected an exception to be thrown"
        Left exn ->
          case fromException @(AnnotatedException SomeException) exn of
            Just (AnnotatedException anns _) ->
              immutableScopes anns `shouldSatisfy` null
            Nothing ->
              pure ()

-- Helpers

immutableScopes :: [Annotation] -> [ScopeData]
immutableScopes anns = [s | Annotation a <- anns, Just s <- [cast @_ @ScopeData a]]

runM :: (forall m. (Safe.MonadMask m, Safe.MonadThrow m, MonadIO m) => m a) -> IO a
runM action = runReaderT action ()
