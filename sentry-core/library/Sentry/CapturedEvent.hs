{-# LANGUAGE ViewPatterns #-}

-- | The in-flight value that flows through the capture pipeline.
--
-- A 'CapturedEvent' bundles a 'Patrol.Type.Event.Event' with optional
-- contextual metadata:
--
-- * the originating 'Control.Exception.SomeException', if the event was
--   constructed from one (typically via 'Sentry.Capture.captureException')
--
-- Integrations and 'Sentry.Client.Options.beforeSend' callbacks take this
-- wrapper to as an argument and can use to enrich the event structure (e.g.
-- by downcasting the exception to a library-specific type).
module Sentry.CapturedEvent
  ( CapturedEvent (..),
    withException,
  )
where

import Control.Exception (SomeException)
import Data.Kind (Type)
import Patrol qualified
import Witch qualified

-- | A 'Patrol.Type.Event.Event' plus any contextual metadata that integrations
-- or callbacks may want to inspect.
type CapturedEvent :: Type
data CapturedEvent = CapturedEvent
  { -- | The wire-format event under construction.
    event :: Patrol.Event,
    -- | The originating 'SomeException', typically from when the event was
    -- constructed via 'Sentry.Capture.captureException'.
    exception :: Maybe SomeException
  }

-- Wrap an 'Patrol.Type.Event.Event' with no extra context. Use
-- 'withException' when the event was constructed from a 'SomeException'.
instance Witch.From Patrol.Event CapturedEvent where
  from event = CapturedEvent{event, exception = Nothing}

-- | Wrap an 'Patrol.Type.Event.Event' that was constructed from the given
-- 'SomeException'.
withException :: Patrol.Event -> SomeException -> CapturedEvent
withException event (Just -> exception) = CapturedEvent{event, exception}
