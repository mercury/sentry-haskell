-- | Shared test fixtures: a valid envelope, event, and DSN, used wherever a
-- test needs one but does not care which.
--
-- Not a spec module itself, so tasty-discover contributes nothing from it;
-- import it directly instead.
module Fixtures (testEnvelope, testEvent, testDsn) where

import Patrol qualified
import Patrol.Type.Dsn qualified as Patrol.Dsn
import Patrol.Type.Envelope qualified as Patrol.Envelope
import Patrol.Type.Event qualified as Patrol.Event
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Exception (toException)

-- | A valid 'Patrol.Type.Envelope.Envelope', derived from 'testEvent' and
-- 'testDsn'.
testEnvelope :: Patrol.Envelope
testEnvelope = Patrol.Envelope.fromEvent testDsn testEvent

-- | A valid 'Patrol.Type.Event.Event' mock.
testEvent :: Patrol.Event
testEvent = unsafePerformIO . Patrol.Event.fromSomeException . toException $ userError "boom"
{-# NOINLINE testEvent #-}

-- | A valid 'Patrol.Type.Dsn.Dsn' mock.
testDsn :: Patrol.Dsn
testDsn =
  Patrol.Dsn.Dsn
    { Patrol.Dsn.protocol = "a",
      Patrol.Dsn.publicKey = "b",
      Patrol.Dsn.secretKey = "",
      Patrol.Dsn.host = "c",
      Patrol.Dsn.port = Nothing,
      Patrol.Dsn.path = "/",
      Patrol.Dsn.projectId = "d"
    }
