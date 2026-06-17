-- | The in-flight value that flows through the capture pipeline, plus pure
-- event builders.
--
-- A 'CapturedEvent' bundles a 'Patrol.Type.Event.Event' with optional
-- contextual metadata:
--
-- * the originating 'Control.Exception.SomeException', if the event was
--   constructed from one (typically via 'Sentry.Capture.captureException')
--
-- Integrations and 'Sentry.Client.Options.beforeSend' callbacks receive this
-- wrapper and can use it to enrich the event (e.g. by downcasting the
-- exception to a library-specific type).
module Sentry.Event
  ( -- * In-flight wrapper
    CapturedEvent (..),
    withException,

    -- * Pure event builders
    fromException,
    fromMessage,
  )
where

import Control.Exception (SomeException)
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Stack (CallStack)
import Patrol qualified
import Patrol.Type.Event qualified as Patrol.Event
import Patrol.Type.EventType qualified as Patrol.EventType
import Patrol.Type.Exceptions qualified as Patrol.Exceptions
import Patrol.Type.Level qualified as Patrol.Level
import Patrol.Type.LogEntry qualified as Patrol.LogEntry
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
    -- public entry points in "Sentry.Capture".  Acts as a universal backstop
    -- when no richer frame source (e.g. 'annotated-exception' annotations or
    -- GHC 'Control.Exception.Context.ExceptionContext') is available.
    captureCallStack :: Maybe CallStack
  }

-- | Wrap a 'Patrol.Type.Event.Event' with no extra context.  Use
-- 'withException' when the event was constructed from a 'SomeException'.
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

-- | Build a minimal 'Patrol.Event' from a 'SomeException'.
--
-- Sets 'exception', 'level', and 'type_'; leaves 'eventId', 'timestamp',
-- 'sdk', 'platform', and all option-derived fields ('release', 'environment',
-- 'serverName', 'dist') to sentinel values that
-- 'Sentry.Capture.applyClientDefaults' can match on and fill at capture time.
--
-- __NOTE__: This replaces 'Patrol.Type.Event.fromSomeException' as the SDK's
-- own exception event builder, ensuring no pre-baked defaults from
-- 'Patrol.Type.Event.initial' can mask configuration values.
fromException :: SomeException -> Patrol.Event
fromException e =
  Patrol.Event.empty
    { Patrol.Event.exception = Just (Patrol.Exceptions.fromSomeException e),
      Patrol.Event.level = Just Patrol.Level.Error,
      Patrol.Event.type_ = Just Patrol.EventType.Default
    }

-- | Build a minimal 'Patrol.Event' for a plain text message at the given
-- severity level.
--
-- Sets 'logentry' and 'level'; leaves 'eventId', 'timestamp', 'sdk',
-- 'platform', and all option-derived fields to sentinel values that
-- 'Sentry.Capture.applyClientDefaults' can match on and fill at capture time.
fromMessage :: Patrol.Level -> Text -> Patrol.Event
fromMessage lvl msg =
  let logEntry =
        Patrol.LogEntry.empty
          { Patrol.LogEntry.formatted = msg,
            Patrol.LogEntry.message = msg
          }
   in Patrol.Event.empty
        { Patrol.Event.level = Just lvl,
          Patrol.Event.logentry = Just logEntry
        }
