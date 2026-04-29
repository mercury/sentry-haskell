-- |  This module re-exports the API that consumers will typically use
-- when instrumenting: the 'Client' and 'ClientOptions' types, the
-- 'captureEvent' verb, and the 'withScope' \/ 'withIsolationScope' helpers for
-- enriching events with contextual metadata.
--
-- The re-exported 'withScope' \/ 'withIsolationScope' come from
-- "Sentry.Scope.IO" and require 'Control.Monad.IO.Unlift.MonadUnliftIO'. If
-- your application stack only provides @MonadMask@ \/ @MonadIO@, import the
-- equivalent helpers from "Sentry.Scope.Monad" directly.
--
-- Plumbing modules ("Sentry.Transport", "Sentry.Integration", and the
-- "Sentry.Scope" operations) are intentionally not re-exported here, and are
-- intended to be imported with qualification.
module Sentry
  ( -- * Client
    Client,
    pattern NON_RECORDING_CLIENT,

    -- * Client Options
    ClientOptions (..),
    pattern DEFAULT_CLIENT_OPTIONS,
    BeforeCallback,

    -- * Capturing Events
    captureEvent,
    captureEvent_,
    captureException,
    captureException_,

    -- * Scope
    Scope,
    ScopeData (..),
    ScopeType (..),
    withScope,
    withIsolationScope,
  )
where

import Sentry.Capture (captureEvent, captureEvent_, captureException, captureException_)
import Sentry.Client (Client, pattern NON_RECORDING_CLIENT)
import Sentry.Client.Options (BeforeCallback, ClientOptions (..), pattern DEFAULT_CLIENT_OPTIONS)
import Sentry.Scope (Scope, ScopeData (..), ScopeType (..))
import Sentry.Scope.IO (withIsolationScope, withScope)
