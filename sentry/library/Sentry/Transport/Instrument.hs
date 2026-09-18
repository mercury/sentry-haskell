-- | Instrumentation for envelope delivery, independent of any wire protocol.
module Sentry.Transport.Instrument (Attempt (..), observing) where

import Control.Exception (evaluate)
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

-- | Observe every delivery attempt made through the given sender.
--
-- The observer runs on the sending thread and is not given a timeout, so an
-- observer that blocks delays that thread.
--
-- Under the async executor that is the single worker, and a blocked worker
-- stops draining the queue, so this function __must not block__.
--
-- > sender = Instrument.observing report Encoding.Gzip backendSend >=> HTTPDelivery.interpretNow
observing ::
  (Attempt a -> IO ()) ->
  Compression ->
  (EncodedBody -> IO a) ->
  Patrol.Envelope ->
  IO a
observing observe compression send envelope = do
  -- Forced here, outside the timed region: see 'Encoding.EncodedBody.size'
  -- for why this is what finishes serialization and compression.
  body <- evaluate (Encoding.encode compression envelope)
  start <- getMonotonicTimeNSec
  outcome <- send body
  end <- getMonotonicTimeNSec
  let attempt =
        Attempt
          { envelope,
            size = body.size,
            compression = body.compression,
            elapsed = fromIntegral (end - start) / 1e9,
            outcome
          }
  observe attempt `catchAny` \_ -> pure ()
  pure outcome
