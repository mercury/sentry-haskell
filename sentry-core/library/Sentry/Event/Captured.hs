-- | The in-flight value that flows through the capture pipeline.
--
-- A 'CapturedEvent' bundles a 'Patrol.Type.Event.Event' with optional
-- the originating 'Control.Exception.SomeException', if the event was
-- constructed from one (typically via 'Sentry.Capture.captureException').
--
-- Integrations and 'Sentry.Client.Options.beforeSend' callbacks receive this
-- wrapper and can modify the event using exception metadata (e.g. by
-- downcasting the exception to a library-specific type).
--
-- They return the resulting event record; see "Sentry.Event" for fields and
-- builders.
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

-- | A 'Patrol.Type.Event.Event' plus any contextual metadata that integrations
-- or callbacks may want to inspect.
type CapturedEvent :: Type
data CapturedEvent = CapturedEvent
  { -- | The wire-format event under construction.
    event :: Patrol.Event,
    -- | The inner (unwrapped) exception from which the event was built, if the
    -- event was constructed via 'Sentry.Capture.captureException'.
    --
    -- When the originating exception was an
    -- 'Control.Exception.Annotated.AnnotatedException', this holds the
    -- /inner/ exception — the same value used to build the 'Patrol.Event'.
    exception :: Maybe SomeException,
    -- | The original, unmodified exception exactly as it was passed to
    -- 'Sentry.Capture.captureException' — before any
    -- 'Control.Exception.Annotated.AnnotatedException' unwrapping.
    --
    -- Integrations that extract 'GHC.Stack.CallStack' annotations (e.g.
    -- "Sentry.Integration.Stacktrace") read this field so they see the full
    -- annotation set.  'Nothing' for events built from messages.
    originalException :: Maybe SomeException,
    -- | The 'GHC.Stack.CallStack' at the
    -- 'Sentry.Capture.captureException' \/ 'Sentry.Capture.captureMessage'
    -- call site.
    --
    -- Populated only when those functions are called with a
    -- 'GHC.Stack.HasCallStack' constraint in scope, which is the case for all
    -- public entry points in "Sentry.Capture".
    --
    -- Acts as a universal backstop when no richer frame source is available.
    captureCallStack :: Maybe CallStack
  }

-- | Wrap a 'Patrol.Type.Event.Event' with no extra context.
--
-- Use 'withException' when the event was constructed from a 'SomeException'.
instance Witch.From Patrol.Event CapturedEvent where
  from event =
    CapturedEvent
      { event,
        exception = Nothing,
        originalException = Nothing,
        captureCallStack = Nothing
      }

-- | Wrap a 'Patrol.Type.Event.Event' that was constructed from the given
-- 'SomeException'.
--
-- * @exception@ receives the inner (post-unwrap) exception used to build the
--   event body.
-- * @originalException@ receives @orig@ — the exception before any
--   'Control.Exception.Annotated.AnnotatedException' unwrapping.
withException :: Patrol.Event -> SomeException -> SomeException -> CapturedEvent
withException event exception originalException =
  CapturedEvent
    { event,
      exception = Just exception,
      originalException = Just originalException,
      captureCallStack = Nothing
    }
