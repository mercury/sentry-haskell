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
--
-- __Note__: This module replaces the old @Sentry.CapturedEvent@ module, which
-- has been removed.  Import from @Sentry.Event@ going forward.
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
    -- | The originating 'SomeException', if the event was constructed via
    -- 'Sentry.Capture.captureException'.
    exception :: Maybe SomeException
  }

-- | Wrap a 'Patrol.Type.Event.Event' with no extra context.  Use
-- 'withException' when the event was constructed from a 'SomeException'.
instance Witch.From Patrol.Event CapturedEvent where
  from event = CapturedEvent{event, exception = Nothing}

-- | Wrap a 'Patrol.Type.Event.Event' that was constructed from the given
-- 'SomeException'.
withException :: Patrol.Event -> SomeException -> CapturedEvent
withException event exception = CapturedEvent{event, exception = Just exception}

-- | Build a minimal 'Patrol.Event' from a 'SomeException'.
--
-- Sets 'exception', 'level', and 'type_'; leaves 'eventId', 'timestamp',
-- 'sdk', 'platform', and all option-derived fields ('release', 'environment',
-- 'serverName', 'dist') to sentinal values that
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
