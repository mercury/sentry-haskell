{-# LANGUAGE RequiredTypeArguments #-}

module Sentry.Client
  ( -- * Client
    Client,
    options,
    transport,
    integrations,
    pattern NON_RECORDING_CLIENT,
    getIntegration,

    -- * Construction
    new,
    builtinIntegrations,

    -- * Helpers
    disableIntegration,
  )
where

import Control.Exception.Backtrace (BacktraceMechanism (..), setBacktraceMechanismState)
import Control.Monad (when)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.List (nub)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Typeable (cast)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Sentry.Client.Internal (Client (..), integrations, options, transport)
import Sentry.Client.Internal qualified as ClientInternal
import Sentry.Client.Options (ClientOptions, pattern DEFAULT_CLIENT_OPTIONS)
import Sentry.Client.Options.Dsn qualified as Dsn
import Sentry.Client.Options.Env qualified as Env
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
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import Type.Reflection (SomeTypeRep, someTypeRep)

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
-- Sentry.init (def & disableIntegration (type ProcessStacktraceIntegration))
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
-- Reads environment configuration once and fills initial defaults before setup.
-- Caller-supplied @Dsn.Disabled@ remains authoritative after every hook.
--
-- 1. If 'Sentry.Client.Options.ClientOptions.defaultIntegrations' is @True@,
--    prepend 'builtinIntegrations' (minus any whose type is already present in
--    the user-provided list, so the user-provided integration wins).
-- 2. Deduplicate integrations based on 'Type.Reflection.SomeTypeRep'.
-- 3. Run 'Sentry.Integration.Integration.setup' for each integration in order,
--    threading the returned 'Sentry.Client.Options.ClientOptions' through to the
--    next (so later integrations see earlier integrations' changes).
-- 4. Normalize setup output and fill terminal defaults, retaining the installed
--    roster. Only DSN inheritance consults the original environment again.
-- 5. Emit configuration diagnostics using the final debug setting, then realize
--    the transport once if a concrete DSN exists.
--
-- Application code should prefer the owned lifecycle helpers in "Sentry.Init".
new :: ClientOptions -> IO Client
new initialOpts = do
  -- Ensure the HasCallStack backtrace mechanism is on so every thrown
  -- exception carries a CallStack in its ExceptionContext.  We leave
  -- CostCentre, Execution (DWARF), and IPE untouched — those require
  -- specific build flags and are opt-in by the user.
  setBacktraceMechanismState HasCallStackBacktrace True
  snapshot <- Env.snapshotWith lookupEnv
  let (resolvedOpts, envWarnings) = Env.resolveSnapshot snapshot initialOpts
  let enforceDisabled opts
        | initialOpts.dsn == Dsn.Disabled = opts{Internal.dsn = Dsn.Disabled}
        | otherwise = opts
  let typeReps = Set.fromList [r | SomeIntegration r _ <- Vector.toList resolvedOpts.integrations]
      kept
        | resolvedOpts.defaultIntegrations =
            builtinIntegrations & Vector.filter \(SomeIntegration rep _) ->
              rep `Set.notMember` typeReps
                && rep `Set.notMember` resolvedOpts.disabledIntegrations
        | otherwise = Vector.empty
      installed = dedupByTypeRep (kept <> resolvedOpts.integrations)
      opts' = resolvedOpts{Internal.integrations = installed}
  setupOpts <- Vector.foldM (\o i -> enforceDisabled <$> setup i o) opts' installed
  let (normalized, finalWarnings) = Env.finalize snapshot (enforceDisabled setupOpts)
      finalOpts = normalized{Internal.integrations = installed}
      runtime = ClientInternal.runtimeOptionsFromOptions finalOpts
  when runtime.debug $
    for_ (map Env.renderWarning (nub (envWarnings <> finalWarnings))) \w ->
      hPutStrLn stderr $ "[sentry] " <> Text.unpack w
  realized <- case (finalOpts.dsn, finalOpts.transport) of
    (Dsn.Explicit _, Just (PrebuiltTransport t)) -> pure (Just t)
    (Dsn.Explicit dsn, Just (DeferredTransport mk)) -> Just <$> mk dsn finalOpts
    _ -> pure Nothing
  pure (Client finalOpts realized installed runtime)
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
pattern NON_RECORDING_CLIENT <- Client _ Nothing _ _
  where
    NON_RECORDING_CLIENT = Client opts Nothing Vector.empty (ClientInternal.runtimeOptionsFromOptions opts)
      where
        opts =
          fst $
            Env.finalize (Env.EnvSnapshot []) $
              (DEFAULT_CLIENT_OPTIONS)
                { Internal.dsn = Dsn.Disabled,
                  Internal.defaultIntegrations = False
                }
