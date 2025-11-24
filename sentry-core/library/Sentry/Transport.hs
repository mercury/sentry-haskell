module Sentry.Transport
  ( -- * TODO: Documentation.
    Transport (..),
    SomeTransport (..),
    SendResponse (..),
    FlushResponse (..),
    ShutdownResponse (..),
  )
where

import Data.Kind (Constraint, Type)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Patrol qualified

-- | TODO: Documentation.
type Transport :: Type -> Constraint
class Transport t where
  -- | TODO: Documentation.
  send :: t -> Patrol.Envelope -> IO SendResponse

  -- | Signal the transport to flush all enqueued messages, blocking for up to
  -- the provided time limit before returning.
  flush :: t -> NominalDiffTime -> IO FlushResponse

  -- | Signal the transport to shut itself down within the provided time limit.
  shutdown :: t -> NominalDiffTime -> IO ShutdownResponse

-- | Potential responses from a call to 'send'.
type SendResponse :: Type
data SendResponse
  = -- | The transport failed to send because it has been shut down.
    SendFailed_Shutdown
  | -- | The transport failed to send because its queue is full.
    --
    -- This only really applies to transports that queue or buffer envelopes to
    -- be sent, but that's probably going to be the majority of them.
    SendFailed_QueueFull
  | -- | The request to send was successfully processed.
    --
    -- This does /not/ mean that the 'Patrol.Type.Envelope.Envelope' was
    -- /delivered/ successfully, whether or not that is true depends on the
    -- semantics of the transport itself.
    SendProcessed
  deriving stock (Eq, Show)

-- | Potential responses from a call to 'flush'.
type FlushResponse :: Type
data FlushResponse
  = -- | The transport failed to flush because its queue is full.
    --
    -- This only really applies to transports that queue or buffer envelopes to
    -- be sent, but that's probably going to be the majority of them.
    FlushFailed_QueueFull
  | -- | The transport failed to flush its queue before the given timeout.
    FlushFailed_TimedOut NominalDiffTime
  | -- | The transport failed to flush because it has been shut down.
    FlushFailed_Shutdown
  | -- | The flush failed for some other reason.
    --
    -- Since users provide their own 'Transport' implementations, we can't
    -- enumerate all possible failure modes here.
    FlushFailed_Other Text
  | -- | The transport was flushed successfully within the time limit.
    FlushSucceeded
  deriving stock (Eq, Show)

-- | Potential responses from a call to 'shutdown'.
type ShutdownResponse :: Type
data ShutdownResponse
  = -- | The transport was already shut down when the call to 'shutdown' was
    -- received.
    ShutdownFailed_AlreadyShutdown
  | -- | The transport failed to gracefully shut down before the given timeout.
    ShutdownFailed_TimedOut NominalDiffTime
  | -- | The transport failed to shutdown gracefully for some other reason.
    --
    -- Since users provide their own 'Transport' implementations, we can't
    -- enumerate all possible failure modes here.
    ShutdownFailed_Other Text
  | -- | The transport shutdown gracefully, within its time limit, after flushing
    -- all items enqueued to be sent.
    ShutdownSucceeded
  deriving stock (Eq, Show)

-- | An opaque wrapper around any type with a valid 'Transport' instance.
--
-- This allows a 'Sentry.Client.Client' to store any potential type which is
-- capable of sending 'Patrol.Event.Event's to a Sentry server, and lets users
-- provide whatever implementation they desire to the client.
type SomeTransport :: Type
data SomeTransport = forall t. (Transport t) => SomeTransport t

instance Transport SomeTransport where
  send (SomeTransport t) = send t
  flush (SomeTransport t) = flush t
  shutdown (SomeTransport t) = shutdown t
