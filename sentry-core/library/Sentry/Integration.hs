module Sentry.Integration where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Text qualified as Text
import Patrol qualified
import Sentry.Client.Options (ClientOptions (..))
import Type.Reflection (SomeTypeRep, Typeable, someTypeRep, typeOf)
import Witch qualified

-- | An 'Integration' has two primary purposes:
--
--     * it can act as a source for 'Patrol.Type.Event.Event's, which can
--       capture new 'Patrol.Type.Event.Event's
--     * it can act as a processor for 'Patrol.Type.Event.Event's, which can
--       modify every 'Patrol.Type.Event.Event' flowing through the pipeline
type Integration :: Type -> Constraint
class (Typeable t) => Integration t where
  -- | Human-readable name of this integration.
  --
  -- This will be included in SDK metadata sent to the Sentry server.
  --
  -- The default implementation renders the type representation of the
  -- integration as text.
  name :: t -> Text
  name = Text.show . typeOf @t

  -- | A setup hook called whenever the integration is attached to a
  -- 'Sentry.Client.Client'.
  --
  -- The default implementation is a no-op.
  setup :: t -> ClientOptions -> IO ()
  setup _ _ = pure ()

  -- | The event processor hook for the integration that implements this
  -- interface.
  --
  -- The main purpose behind the 'Integration' abstraction is to process
  -- 'Patrol.Type.Event.Event's flowing through it.
  --
  -- Examples include:
  --     * dropping 'Patrol.Type.Event.Event's entirely
  --     * adding or processing 'GHC.Stack.CallStack's
  --     * obfuscating personally identifiable information
  --     * adding information from, or produced by, the 'Integration' itself
  --
  -- The default implementation is a no-op.
  processEvent :: t -> Patrol.Event -> ClientOptions -> IO (Maybe Patrol.Event)
  processEvent _ event _ = pure . Just $ event

-- | An opaque wrapper around any type with a valid 'Integration' instance.
--
-- This allows a heterogeneous list of types which implement 'Integration' to
-- be stored in a 'Sentry.Client.Client' and applied successively as part of
-- an 'Patrol.Type.Event.Event' processing pipeline.
type SomeIntegration :: Type
data SomeIntegration = forall t. (Integration t) => SomeIntegration SomeTypeRep t

instance Integration SomeIntegration where
  name (SomeIntegration _ i) = name i
  setup (SomeIntegration _ i) = setup i
  processEvent (SomeIntegration _ i) = processEvent i

instance Integration i => Witch.From i SomeIntegration where
  from = fromIntegration

-- | Package up any type with an 'Integration' instance into 'SomeIntegration',
-- storing its type representation so that it may be used as an index when
-- looking up integrations installed in a 'Sentry.Client.Client'.
fromIntegration :: forall i. Integration i => i -> SomeIntegration
fromIntegration i = SomeIntegration (someTypeRep (Proxy @i)) i
