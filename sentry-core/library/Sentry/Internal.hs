{-# LANGUAGE ViewPatterns #-}

module Sentry.Internal
  ( -- * ClientOptions
    ClientOptions (..),
    pattern DEFAULT_CLIENT_OPTIONS,

    -- * Integration
    Integration (..),
    SomeIntegration (..),
    fromIntegration,
  ) where

import Data.Default (Default (def))
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (NominalDiffTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Network.URI (URI)
import Patrol qualified
import Patrol.Type.Dsn qualified as Patrol.Dsn
import Sentry.CapturedEvent (CapturedEvent (..))
import Sentry.Transport (SomeTransport)
import Type.Reflection (SomeTypeRep, Typeable, someTypeRep, typeOf)
import Witch qualified

-- | Configuration settings for 'Sentry.Client.Client'.
type ClientOptions :: Type
data ClientOptions = ClientOptions
  { -- | A Sentry Data Source Name (DSN) from which the client will derive a
    -- URL and send telemetry to.
    --
    -- Defaults to @Nothing@, which disables the client.
    --
    -- <https://docs.sentry.io/concepts/key-terms/dsn-explainer/>
    dsn :: Maybe Patrol.Dsn,
    -- | Whether the SDK should enable debug logging.
    --
    -- Defaults to @False@.
    debug :: Bool,
    -- | The instrumented application's release version, which must be unique
    -- across all projects in your organization.
    --
    -- This value can be the git SHA for the given project, or a product
    -- identifier with a semantic version.
    --
    -- Defaults to @Nothing@.
    release :: Maybe Text,
    -- | The environment name, such as @production@ or @staging@; the default
    -- value should be @production@.
    environment :: Maybe Text,
    -- | Error event sample rate; defaults to @1.0@.
    --
    -- Values outside of @[0,1]@ will be clamped.
    sampleRate :: Float,
    -- | Maximum number of breadcrumbs; defaults to 100.
    maxBreadcrumbs :: Word,
    -- | Whether capturing personally identifying information (PII) is
    -- permissible.
    --
    -- When enabled, some information that could be considered PII—such as
    -- potentially sensitive HTTP headers, user IP addresses in server
    -- integrations, etc.— may be captured by the SDK.
    --
    -- Defaults to @False@.
    sendDefaultPII :: Bool,
    -- | The server name to be reported.
    --
    -- Defaults to @Nothing@.
    serverName :: Maybe Text,
    -- | Module prefixes that are always considered @in_app@.
    --
    -- Defaults to an empty 'Data.HashSet.HashSet'.
    --
    -- __NOTE__: May be impacted by <https://github.com/getsentry/sentry/issues/110965>.
    inAppInclude :: HashSet Text,
    -- | Module prefixes that are never considered @in_app@.
    --
    -- Defaults to an empty 'Data.HashSet.HashSet'.
    --
    -- __NOTE__: May be impacted by <https://github.com/getsentry/sentry/issues/110965>.
    inAppExclude :: HashSet Text,
    -- | A list of 'Integration's that are enabled for the 'Sentry.Client.Client'
    -- constructed from these options.
    --
    -- Defaults to an empty 'Data.Vector.Vector'.
    integrations :: Vector SomeIntegration,
    -- | Whether to install default integrations.
    --
    -- Defaults to @True@.
    defaultIntegrations :: Bool,
    -- | Callback that is executed before a 'Patrol.Event' is sent.
    --
    -- Receives a 'CapturedEvent' so the callback can inspect contextual
    -- data such as the originating exception alongside the event itself.
    --
    -- Defaults to @Nothing@.
    --
    -- <https://develop.sentry.dev/sdk/foundations/client/hooks/>
    beforeSend :: Maybe (CapturedEvent -> Maybe Patrol.Event),
    -- | Callback that is executed when a 'Patrol.Breadcrumb' is constructed;
    -- this is somewhat deliberately ambiguous, as "constructed" can refer to
    -- "added to an event" or "added to the active scope".
    --
    -- Defaults to @Nothing@.
    --
    -- <https://develop.sentry.dev/sdk/foundations/client/hooks/>
    beforeBreadcrumb :: Maybe (Patrol.Breadcrumb -> Maybe Patrol.Breadcrumb),
    -- | The 'Sentry.Transport.Transport' that the 'Sentry.Client.Client'
    -- constructed from these options will use.
    --
    -- Defaults to @Nothing@.
    transport :: Maybe SomeTransport,
    -- | The timeout given to the client to drain events on shutdown.
    shutdownTimeout :: NominalDiffTime
  }

pattern DEFAULT_CLIENT_OPTIONS :: ClientOptions
pattern DEFAULT_CLIENT_OPTIONS <-
  ClientOptions
    { dsn = Nothing,
      debug = False,
      release = Nothing,
      environment = Nothing,
      sampleRate = 1.0,
      maxBreadcrumbs = 100,
      sendDefaultPII = False,
      serverName = Nothing,
      inAppInclude = (HashSet.null -> True),
      inAppExclude = (HashSet.null -> True),
      integrations = (Vector.null -> True),
      defaultIntegrations = True,
      beforeSend = Nothing,
      beforeBreadcrumb = Nothing,
      transport = Nothing,
      shutdownTimeout = 2 -- seconds
    }
  where
    DEFAULT_CLIENT_OPTIONS =
      ClientOptions
        { dsn = Nothing,
          debug = False,
          release = Nothing,
          environment = Nothing,
          sampleRate = 1.0,
          maxBreadcrumbs = 100,
          sendDefaultPII = False,
          serverName = Nothing,
          inAppInclude = HashSet.empty,
          inAppExclude = HashSet.empty,
          integrations = Vector.empty,
          defaultIntegrations = True,
          beforeSend = Nothing,
          beforeBreadcrumb = Nothing,
          transport = Nothing,
          shutdownTimeout = 2 -- seconds
        }

instance Default ClientOptions where
  def = DEFAULT_CLIENT_OPTIONS

instance Witch.From Patrol.Dsn ClientOptions where
  from (Just -> dsn) = def{dsn}

instance Witch.TryFrom URI ClientOptions where
  tryFrom uri = case Patrol.Dsn.fromUri uri of
    Left exc -> Left $ Witch.TryFromException uri (Just exc)
    Right dsn -> Right $ Witch.from dsn

instance Witch.TryFrom ClientOptions Patrol.Dsn where
  tryFrom options = case options.dsn of
    Nothing -> Left $ Witch.TryFromException options Nothing
    Just dsn -> Right dsn

instance Witch.TryFrom ClientOptions URI where
  tryFrom options = case options.dsn of
    Nothing -> Left $ Witch.TryFromException options Nothing
    Just dsn -> Right $ Patrol.Dsn.intoUri dsn

-- \| An 'Integration' has two primary purposes:
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
  -- The accompanying 'CapturedEvent' carries metadata about the event \-\-
  -- most notably the originating 'Control.Exception.SomeException' when one
  -- is available \-\- so integrations can downcast it to a library-specific
  -- type (e.g. @HttpException@, @SqlException@) and enrich the event
  -- accordingly.
  --
  -- Examples include:
  --     * dropping 'Patrol.Type.Event.Event's entirely
  --     * adding or processing 'GHC.Stack.CallStack's
  --     * obfuscating personally identifiable information
  --     * adding information from, or produced by, the 'Integration' itself
  --
  -- The default implementation is a no-op.
  processEvent :: t -> CapturedEvent -> ClientOptions -> IO (Maybe Patrol.Event)
  processEvent _ ce _ = pure . Just $ ce.event

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

instance (Integration i) => Witch.From i SomeIntegration where
  from = fromIntegration

instance Witch.From SomeIntegration SomeTypeRep where
  from (SomeIntegration t _) = t

-- | Package up any type with an 'Integration' instance into 'SomeIntegration',
-- storing its type representation so that it may be used as an index when
-- looking up integrations installed in a 'Sentry.Client.Client'.
fromIntegration :: forall i. (Integration i) => i -> SomeIntegration
fromIntegration i = SomeIntegration (someTypeRep (Proxy @i)) i
