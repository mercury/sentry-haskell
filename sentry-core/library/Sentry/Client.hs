{-# LANGUAGE RequiredTypeArguments #-}

module Sentry.Client
  ( -- * Client
    Client (..),
    pattern NON_RECORDING_CLIENT,
    getIntegration,

    -- * Construction
    new,
    builtinIntegrations,

    -- * Helpers
    disableIntegration,
    realizePrebuilt,
    resolveOptionDefaults,
  )
where

import Control.Exception.Backtrace (BacktraceMechanism (..), setBacktraceMechanismState)
import Data.Function ((&))
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Typeable (cast)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Sentry.Client.Options (ClientOptions (..), pattern DEFAULT_CLIENT_OPTIONS)
import Sentry.Integration (Integration (..), SomeIntegration (..), fromIntegration)
import Sentry.Integration.Context (ContextIntegration (..))
import Sentry.Integration.Stacktrace
  ( AttachAnnotatedExceptionIntegration (..),
    AttachCallStackIntegration (..),
    AttachExceptionContextIntegration (..),
    ProcessStacktraceIntegration (..),
  )
import Sentry.Internal (TransportProvider (..))
import Sentry.Internal qualified as Internal
import Sentry.Transport (SomeTransport)
import System.Environment (lookupEnv)
import Type.Reflection (SomeTypeRep, someTypeRep)
import Witch qualified

-- | The 'Client' is responsible for processing events and sending them to the
-- Sentry server via the 'Sentry.Transport.SomeTransport' it contains; it can
-- be created from 'Sentry.Client.Options.ClientOptions', which it retains a
-- copy of after construction.
type Client :: Type
data Client = Client
  { options :: ClientOptions,
    transport :: Maybe SomeTransport,
    integrations :: Vector SomeIntegration
  }

-- | Realize a prebuilt transport from options, DSN-gated.
--
-- A 'DeferredTransport' cannot run without 'IO', so it yields 'Nothing'
-- (a non-recording client). Use 'new' to realize deferred providers.
realizePrebuilt :: ClientOptions -> Maybe SomeTransport
realizePrebuilt opts = case (opts.dsn, opts.transport) of
  (Just _, Just (PrebuiltTransport t)) -> Just t
  _ -> Nothing

-- | Low-level conversion that copies fields directly, running neither default
-- integration prepending nor 'Sentry.Integration.Integration.setup'. Useful
-- in tests or when constructing a 'Client' without the full initialization
-- lifecycle.
--
-- 'PrebuiltTransport' providers are realized (DSN-gated).
-- 'DeferredTransport' providers cannot run without 'IO' and yield a
-- non-recording client; use 'new' for those.
--
-- For normal application use, prefer 'new' (or 'Sentry.Init.withSentry').
instance Witch.From ClientOptions Client where
  from options@ClientOptions{integrations} =
    Client
      { options,
        transport = realizePrebuilt options,
        integrations
      }

-- | Attempt to find a 'Sentry.Integration.Integration' in the list of
-- 'Sentry.Integration.SomeIntegration' installed in the 'Client'.
getIntegration :: forall i -> (Integration i) => Client -> Maybe i
getIntegration iType client = do
  let iid = someTypeRep (Proxy @iType)
  (SomeIntegration _ i) <- Vector.find (\(SomeIntegration rep _) -> rep == iid) client.integrations
  cast i

-- | Mark a built-in 'Sentry.Integration.Integration' type as disabled,
-- preventing it from being included in 'builtinIntegrations' when 'new' is
-- called.
--
-- This lets you opt out of a single default integration without turning off
-- all defaults via @defaultIntegrations = False@.
--
-- Example:
--
-- @
-- withSentry
--   (def & disableIntegration (type ProcessStacktraceIntegration))
--   \\client -> ...
-- @
disableIntegration :: forall i -> (Integration i) => ClientOptions -> ClientOptions
disableIntegration iType opts =
  opts
    { Internal.disabledIntegrations =
        Set.insert
          (someTypeRep (Proxy @iType))
          opts.disabledIntegrations
    }

-- | Integrations installed automatically when
-- 'Sentry.Client.Options.ClientOptions.defaultIntegrations' is @True@.
--
-- __Ordering is significant for the stacktrace integrations:__
-- 'AttachExceptionContextIntegration', 'AttachAnnotatedExceptionIntegration',
-- and 'AttachCallStackIntegration' must all run /before/
-- 'ProcessStacktraceIntegration' (which classifies in-app after all frame
-- sources have contributed).
builtinIntegrations :: Vector SomeIntegration
builtinIntegrations =
  Vector.fromList
    [ fromIntegration ContextIntegration,
      -- Stacktrace: frame-attachment sources (run first, in priority order)
      fromIntegration AttachExceptionContextIntegration,
      fromIntegration AttachAnnotatedExceptionIntegration,
      fromIntegration AttachCallStackIntegration,
      -- Stacktrace: in-app classification (must be last)
      fromIntegration ProcessStacktraceIntegration
    ]

-- | Construct a 'Client' from 'Sentry.Client.Options.ClientOptions', running
-- the full initialization lifecycle:
--
-- 1. If 'Sentry.Client.Options.ClientOptions.defaultIntegrations' is @True@,
--    prepend 'builtinIntegrations' (minus any whose type is already present in
--    the user-provided list, so the user-provided integration wins).
-- 2. Deduplicate integrations based on 'Type.Reflection.SomeTypeRep'.
-- 3. Run 'Sentry.Integration.Integration.setup' for each integration in order,
--    threading the returned 'Sentry.Client.Options.ClientOptions' through to the
--    next (so later integrations see earlier integrations' changes).
-- 4. Build the 'Client' from the final options, setting the integrations to
--    the deduplicated list from step 2.
new :: ClientOptions -> IO Client
new initialOpts = do
  -- Ensure the HasCallStack backtrace mechanism is on so every thrown
  -- exception carries a CallStack in its ExceptionContext.  We leave
  -- CostCentre, Execution (DWARF), and IPE untouched — those require
  -- specific build flags and are opt-in by the user.
  setBacktraceMechanismState HasCallStackBacktrace True
  resolvedOpts <- resolveOptionDefaults initialOpts
  let typeReps = Set.fromList [r | SomeIntegration r _ <- Vector.toList resolvedOpts.integrations]
      kept
        | resolvedOpts.defaultIntegrations =
            builtinIntegrations & Vector.filter \(SomeIntegration rep _) ->
              rep `Set.notMember` typeReps
                && rep `Set.notMember` resolvedOpts.disabledIntegrations
        | otherwise = Vector.empty
      installed = dedupByTypeRep (kept <> resolvedOpts.integrations)
      opts' = resolvedOpts{Internal.integrations = installed}
  finalOpts <- Vector.foldM (\o i -> setup i o) opts' installed
  realized <- case (finalOpts.dsn, finalOpts.transport) of
    (Just _, Just (PrebuiltTransport t)) -> pure (Just t)
    (Just dsn, Just (DeferredTransport mk)) -> Just <$> mk dsn finalOpts
    _ -> pure Nothing
  pure
    Client
      { options = finalOpts{Internal.integrations = installed},
        transport = realized,
        integrations = installed
      }
  where
    -- Keep only the first occurrence of each 'SomeTypeRep' in the vector.
    dedupByTypeRep :: Vector SomeIntegration -> Vector SomeIntegration
    dedupByTypeRep = snd . Vector.foldl' step (Set.empty, Vector.empty)
      where
        step :: (Set SomeTypeRep, Vector SomeIntegration) -> SomeIntegration -> (Set SomeTypeRep, Vector SomeIntegration)
        step (seen, acc) si@(SomeIntegration rep _)
          | rep `Set.member` seen = (seen, acc)
          | otherwise = (Set.insert rep seen, Vector.snoc acc si)

-- | Any client which does not have a valid 'Transport' is non-recording.
pattern NON_RECORDING_CLIENT :: Client
pattern NON_RECORDING_CLIENT <- Client{transport = Nothing}
  where
    NON_RECORDING_CLIENT =
      Client
        { options = DEFAULT_CLIENT_OPTIONS,
          transport = Nothing,
          integrations = Vector.empty
        }

-- | Resolve environment-variable and other computed defaults into
-- 'ClientOptions' once, at client construction time.
--
-- Fields that are already set to a @'Just'@ value by the caller are left
-- untouched — explicit configuration always wins.
--
-- * 'release': falls back to @SENTRY_RELEASE@ if unset.
-- * 'environment': falls back to @SENTRY_ENVIRONMENT@, then @"production"@.
resolveOptionDefaults :: ClientOptions -> IO ClientOptions
resolveOptionDefaults opts = do
  release <- case opts.release of
    Just r -> pure (Just r)
    Nothing -> do
      mval <- lookupEnv "SENTRY_RELEASE"
      pure $ Text.pack <$> mval
  environment <- case opts.environment of
    Just e -> pure (Just e)
    Nothing -> do
      mval <- lookupEnv "SENTRY_ENVIRONMENT"
      pure . Just $ maybe "production" Text.pack mval
  pure opts{release, environment}
