-- | Event and envelope generators shared by the integration tests and the
-- @sentry-profile@ harness.
module Sentry.TestKit.Gen
  ( -- * Events
    sampleEvent,
    messageEvent,

    -- * Envelopes
    sampleEnvelope,
    messageEnvelope,
  )
where

import Control.Exception (SomeException (..))
import Data.Text qualified as Text
import Patrol qualified
import Patrol.Type.Envelope qualified as Patrol.Envelope
import Patrol.Type.Level qualified as Patrol.Level
import Sentry.Event qualified as Event

-- | A representative exception event, built the same way
-- 'Sentry.Capture.captureException' would build one.
sampleEvent :: Patrol.Event
sampleEvent = Event.fromException (SomeException (userError "boom"))

-- | A message event whose formatted body is @n@ bytes of filler, for stressing
-- serialization and allocation with larger payloads.
messageEvent :: Int -> Patrol.Event
messageEvent n = Event.fromMessage Patrol.Level.Error (Text.replicate n "x")

-- | A valid 'Patrol.Type.Envelope.Envelope' wrapping 'sampleEvent'.
sampleEnvelope :: Patrol.Dsn -> Patrol.Envelope
sampleEnvelope dsn = Patrol.Envelope.fromEvent dsn sampleEvent

-- | A valid 'Patrol.Type.Envelope.Envelope' wrapping a 'messageEvent' of the
-- given byte size.
messageEnvelope :: Patrol.Dsn -> Int -> Patrol.Envelope
messageEnvelope dsn n = Patrol.Envelope.fromEvent dsn (messageEvent n)
