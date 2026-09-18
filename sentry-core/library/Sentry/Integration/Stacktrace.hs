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
import GHC.Stack (CallStack)
import Sentry.Event (Event)
import Sentry.Event.Captured (CapturedEvent (..))
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

  processEvent _ ce _ = pure . Just $ maybe ce.event (\cs -> mergeCallStack cs ce) stack
    where
      stack = do
        orig <- ce.capturedException
        let anns = foldMap annotations (fromException @(AnnotatedException SomeException) orig)
        Stacktrace.callStackFromAnnotations anns

-- | Attach stack frames from the GHC
-- 'Control.Exception.Context.ExceptionContext' backtrace.
type AttachExceptionContextIntegration :: Type
data AttachExceptionContextIntegration = AttachExceptionContextIntegration
  deriving stock (Show)

instance Integration AttachExceptionContextIntegration where
  name _ = "AttachExceptionContextIntegration"

  processEvent _ ce _ = pure . Just $ maybe ce.event (\cs -> mergeCallStack cs ce) stack
    where
      stack = ce.capturedException >>= Stacktrace.callStackFromExceptionContext

-- | Attach the 'GHC.Stack.CallStack' carried along by 'CapturedEvent' from the
-- callsite that captured the event itself.
type AttachCallStackIntegration :: Type
data AttachCallStackIntegration = AttachCallStackIntegration
  deriving stock (Show)

instance Integration AttachCallStackIntegration where
  name _ = "AttachCallStackIntegration"

  processEvent _ ce _ = pure . Just $ maybe ce.event (\cs -> mergeCallStack cs ce) ce.captureCallStack

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

-- | Merge a 'CallStack' into whichever frame container this event uses,
-- returning the resulting event record.
--
-- Message events use a 'Patrol.Type.Thread.Thread' as the frame container;
-- exception events attach frames to the last entry in @exception.values@.
mergeCallStack :: CallStack -> CapturedEvent -> Event
mergeCallStack cs ce =
  if isMessage ce
    then Stacktrace.mergeCallStackIntoThread cs ce.event
    else Stacktrace.mergeCallStackIntoException cs ce.event

-- | Return 'True' when this 'CapturedEvent' represents a message rather than
-- an exception capture.
isMessage :: CapturedEvent -> Bool
isMessage ce = case ce.unwrappedException of
  Nothing -> True
  Just _ -> False
