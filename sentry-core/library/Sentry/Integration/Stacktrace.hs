-- | Built-in stack trace integrations.
--
-- __NOTE__: It is important that `ProcessStacktraceIntegration` be run /after/
-- any other stack trace processors which could change the characteristics of
-- captured stack frames in a way that might change @in_app@ include/exclude
-- markers.
module Sentry.Integration.Stacktrace
  ( AttachAnnotatedExceptionIntegration (..),
    AttachExceptionContextIntegration (..),
    AttachCallStackIntegration (..),
    ProcessStacktraceIntegration (..),
  )
where

import Control.Exception (SomeException, fromException)
import Control.Exception.Annotated (AnnotatedException (..), annotations)
import Data.Kind (Type)
import Sentry.Event (CapturedEvent (..))
import Sentry.Integration (Integration (..))
import Sentry.Internal (ClientOptions (..))
import Sentry.Stacktrace qualified as Stacktrace

-- | Attach stack frames extracted from @annotated-exception@
-- 'Control.Exception.Annotated.AnnotatedException' annotations.
type AttachAnnotatedExceptionIntegration :: Type
data AttachAnnotatedExceptionIntegration = AttachAnnotatedExceptionIntegration
  deriving stock (Show)

instance Integration AttachAnnotatedExceptionIntegration where
  name _ = "AttachAnnotatedExceptionIntegration"

  processEvent _ ce _ = pure . Just $
    case ce.originalException of
      Nothing -> ce.event
      Just orig ->
        let anns = case fromException @(AnnotatedException SomeException) orig of
              Just ae -> annotations ae
              Nothing -> []
         in case Stacktrace.callStackFromAnnotations anns of
              Nothing -> ce.event
              Just cs ->
                if isMessage ce
                  then Stacktrace.mergeCallStackIntoThread cs ce.event
                  else Stacktrace.mergeCallStackIntoException cs ce.event

-- | Attach stack frames from the GHC
-- 'Control.Exception.Context.ExceptionContext' backtrace.
type AttachExceptionContextIntegration :: Type
data AttachExceptionContextIntegration = AttachExceptionContextIntegration
  deriving stock (Show)

instance Integration AttachExceptionContextIntegration where
  name _ = "AttachExceptionContextIntegration"

  processEvent _ ce _ = pure . Just $
    case ce.originalException >>= Stacktrace.callStackFromExceptionContext of
      Nothing -> ce.event
      Just cs ->
        if isMessage ce
          then Stacktrace.mergeCallStackIntoThread cs ce.event
          else Stacktrace.mergeCallStackIntoException cs ce.event

-- | Attach the 'GHC.Stack.CallStack' carried along by 'CapturedEvent' from the
-- callsite that captured the event itself.
type AttachCallStackIntegration :: Type
data AttachCallStackIntegration = AttachCallStackIntegration
  deriving stock (Show)

instance Integration AttachCallStackIntegration where
  name _ = "AttachCallStackIntegration"

  processEvent _ ce _ = pure . Just $
    case ce.captureCallStack of
      Nothing -> ce.event
      Just cs ->
        if isMessage ce
          then Stacktrace.mergeCallStackIntoThread cs ce.event
          else Stacktrace.mergeCallStackIntoException cs ce.event

-- | Classify each stack frame as in-app or not-in-app.
--
-- __NOTE__: This integration must run last among the stacktrace integrations so that
-- all frame-attachment passes have completed before classification.
type ProcessStacktraceIntegration :: Type
data ProcessStacktraceIntegration = ProcessStacktraceIntegration
  deriving stock (Show)

instance Integration ProcessStacktraceIntegration where
  name _ = "ProcessStacktraceIntegration"

  processEvent _ ce opts =
    pure . Just $
      Stacktrace.classifyInApp opts.inAppInclude opts.inAppExclude ce.event

-- | Return 'True' when this 'CapturedEvent' represents a message rather than
-- an exception capture.
--
-- Message events use a 'Patrol.Type.Thread.Thread' as the frame container;
-- exception events attach frames to the first entry in @exception.values@.
isMessage :: CapturedEvent -> Bool
isMessage ce = case ce.exception of
  Nothing -> True
  Just _ -> False
