module Test.Event (lastMechanism) where

import Patrol.Type.Event qualified as Patrol.Event
import Patrol.Type.Exception qualified as Patrol.Exception
import Patrol.Type.Exceptions qualified as Patrol.Exceptions
import Patrol.Type.Mechanism qualified as Patrol.Mechanism

-- | Return the mechanism attached to the last exception in an event, if any.
lastMechanism :: Patrol.Event.Event -> Maybe Patrol.Mechanism.Mechanism
lastMechanism event = do
  exceptions <- event.exception
  case Patrol.Exceptions.values exceptions of
    [] -> Nothing
    excs -> Patrol.Exception.mechanism (last excs)
