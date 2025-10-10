module Sentry.Integration where

import Data.Kind (Type, Constraint)
import Type.Reflection (Typeable, typeOf)
import Data.Text qualified as Text
import Data.Text (Text)
import Sentry.Client.Options (ClientOptions (..))
import Patrol qualified

-- | An 'Integration' has two primary purposes:
-- 
--     * it can act as a source for 'Patrol.Type.Event.Event's, which can
--       capture new 'Patrol.Type.Event.Event's
--     * it can act as a processor for 'Patrol.Type.Event.Event's, which can
--       modify every 'Patrol.Type.Event.Event' flowing through the pipeline
type Integration :: Type -> Constraint
class Typeable t => Integration t where
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
data SomeIntegration = forall t. Integration t => SomeIntegration t

instance Integration SomeIntegration where
  name (SomeIntegration i) = name i
  setup (SomeIntegration i) = setup i
  processEvent (SomeIntegration i) = processEvent i

