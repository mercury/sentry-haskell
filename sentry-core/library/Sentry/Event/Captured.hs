-- | This module defines the event wrapper passed to integrations and
-- before-send hooks.
--
-- Hooks can inspect the exception metadata and return an updated event record.
module Sentry.Event.Captured
  ( CapturedEvent (..),
    withException,
  )
where

import Control.Exception (SomeException)
import Data.Kind (Type)
import GHC.Stack (CallStack)
import Patrol qualified
import Witch qualified

-- | A 'CapturedEvent' carries an event and any exception metadata and call
-- stack available at capture time.
type CapturedEvent :: Type
data CapturedEvent = CapturedEvent
  { -- | The event being prepared for delivery.
    event :: Patrol.Event,
    -- | The exception used to construct the event, with any outer
    -- 'Control.Exception.Annotated.AnnotatedException' wrapper removed.
    -- 
    -- Hooks can downcast this value to inspect the application exception.
    unwrappedException :: Maybe SomeException,
    -- | The exception supplied to capture, retaining any annotation wrapper
    -- and exception context for stacktrace integrations.
    --
    -- Without an annotation wrapper, this and 'unwrappedException' contain
    -- the same value.
    --
    -- Both are 'Nothing'.
    capturedException :: Maybe SomeException,
    -- | The call stack at the exception or message capture site.
    captureCallStack :: Maybe CallStack
  }

-- | Wrap a 'Patrol.Type.Event.Event' with no extra context.
--
-- Use 'withException' when the event was constructed from a 'SomeException'.
instance Witch.From Patrol.Event CapturedEvent where
  from event =
    CapturedEvent
      { event,
        unwrappedException = Nothing,
        capturedException = Nothing,
        captureCallStack = Nothing
      }

-- | Attach exception metadata to an event. The first exception is the
-- unwrapped value used to construct the event; the second is the value
-- supplied to capture.
--
-- This function stores both values without unwrapping either and leaves
-- 'captureCallStack' unset.
withException :: Patrol.Event -> SomeException -> SomeException -> CapturedEvent
withException event unwrappedException capturedException =
  CapturedEvent
    { event,
      unwrappedException = Just unwrappedException,
      capturedException = Just capturedException,
      captureCallStack = Nothing
    }
