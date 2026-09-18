-- | Instrumentation for envelope delivery, independent of any wire protocol.
module Sentry.Transport.Instrument (Attempt (..), observing) where

import Control.Exception (SomeException, evaluate, mask, throwIO, try)
import Data.Int (Int64)
import Data.Kind (Type)
import GHC.Clock (getMonotonicTimeNSec)
import Patrol qualified
import Sentry.Transport.Encoding (Compression, EncodedBody)
import Sentry.Transport.Encoding qualified as Encoding
import UnliftIO.Exception (catchAny)

-- | What one delivery attempt cost and how it resolved.
type Attempt :: Type -> Type
data Attempt a = Attempt
  { -- | The envelope as it was handed to the sender.
    envelope :: Patrol.Envelope,
    -- | Prepared body bytes, excluding headers, framing and TLS overhead.
    -- This does not confirm how many bytes reached the server.
    size :: Int64,
    -- | The encoding the body was prepared with.
    compression :: Compression,
    -- | Seconds spent inside the sender, including connection acquisition and
    -- response draining. The body is encoded and forced before the timer
    -- starts, so serialization and compression are not counted here.
    elapsed :: Double,
    -- | Whatever the sender returned.
    outcome :: a
  }
  deriving stock (Eq, Show)

type role Attempt representational

-- | Observe one attempted envelope delivery, including any attached client report.
-- Drops before sending produce no observation.
--
-- Returned outcomes are reported as 'Right', including HTTP network failures.
-- Escaped exceptions, including cancellation, are reported as 'Left' and then
-- rethrown.
--
-- Observers can decide whether cancellation counts in their metrics.
-- Synchronous observer failures are ignored.
--
-- The callback runs on the sending thread and must finish promptly, as it has
-- no timeout; blocking it also stops an async worker from draining its queue.
--
-- > httpOptions = def{wrapSender = Instrument.observing report}
observing ::
  (Attempt (Either SomeException a) -> IO ()) ->
  Compression ->
  (EncodedBody -> IO a) ->
  Patrol.Envelope ->
  IO a
observing observe compression send envelope = do
  -- Forced here, outside the timed region: see 'Encoding.EncodedBody.size'
  -- for why this is what finishes serialization and compression.
  body <- evaluate (Encoding.encode compression envelope)
  mask \restore -> do
    start <- getMonotonicTimeNSec
    outcome <- try (restore (send body))
    end <- getMonotonicTimeNSec
    let attempt =
          Attempt
            { envelope,
              size = body.size,
              compression = body.compression,
              elapsed = fromIntegral (end - start) / 1e9,
              outcome
            }
        notify = observe attempt `catchAny` \_ -> pure ()
    restore notify
    either throwIO pure outcome
