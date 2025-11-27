{-# LANGUAGE ViewPatterns #-}

module Sentry.Client.Options where

import Data.Default (Default (def))
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Kind (Type)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Network.URI (URI)
import Patrol qualified
import Patrol.Type.Dsn qualified as Patrol.Dsn
import {-# SOURCE #-} Sentry.Integration (SomeIntegration)
import Sentry.Transport (SomeTransport)
import Witch qualified

-- | Alias for a callback that will be run when a client processes something
-- (e.g. a 'Patrol.Type.Breadcrumb.Breadcrumb' or 'Patrol.Type.Event.Event'),
-- and which can potentially filter that item by returning 'Nothing'.
type BeforeCallback :: Type -> Type
type BeforeCallback t = t -> IO (Maybe t)

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
    -- | Callback that is executed for each 'Patrol.Event' added.
    --
    -- Defaults to @Nothing@.
    --
    -- <https://develop.sentry.dev/sdk/foundations/client/hooks/>
    beforeSend :: Maybe (BeforeCallback Patrol.Event),
    -- | Callback that is executed for each 'Patrol.Breadcrumb' added.
    --
    -- Defaults to @Nothing@.
    --
    -- <https://develop.sentry.dev/sdk/foundations/client/hooks/>
    beforeBreadcrumb :: Maybe (BeforeCallback Patrol.Breadcrumb),
    -- | The 'Sentry.Transport.Transport' that the 'Sentry.Client.Client'
    -- constructed from these options will use.
    --
    -- Defaults to @Nothing@.
    transport :: Maybe SomeTransport,
    -- | The timeout given to the client to drain events on shutdown.
    shutdownTimeout :: NominalDiffTime
  }

instance Default ClientOptions where
  def =
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
