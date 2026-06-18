-- |  This module re-exports the API that consumers will typically use
-- when instrumenting: the 'Client' and 'ClientOptions' types, the
-- 'HasClient' capability and 'SentryT' carrier, the capture verbs, the
-- 'withScope' \/ 'withIsolationScope' helpers for enriching events with
-- contextual metadata, and the 'withSentry' \/ 'withSentryM' lifecycle
-- brackets for initializing the SDK.
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
  ( -- * Lifecycle
    withSentry,
    withSentryM,

    -- * Client
    Client,
    pattern NON_RECORDING_CLIENT,

    -- * Client Options
    ClientOptions (..),
    pattern DEFAULT_CLIENT_OPTIONS,
    TransportProvider (..),
    disableIntegration,

    -- * Stacktrace integrations
    AttachAnnotatedExceptionIntegration (..),
    AttachCallStackIntegration (..),
    AttachExceptionContextIntegration (..),
    ProcessStacktraceIntegration (..),

    -- * Ambient client capability
    HasClient (..),
    askClient,

    -- * Concrete monad carrier
    SentryT,
    runSentryT,

    -- * Capturing Events
    captureEvent,
    captureEvent_,
    captureException,
    captureException_,
    captureMessage,
    captureMessage_,

    -- * Captured Event
    CapturedEvent (..),

    -- * Scope
    Scope,
    ScopeData (..),
    ScopeType (..),
    withScope,
    withIsolationScope,

    -- * Breadcrumbs
    addBreadcrumb,
    addBreadcrumbs,
    clearBreadcrumbs,
  )
where

import Sentry.Capture (captureEvent, captureEvent_, captureException, captureException_, captureMessage, captureMessage_)
import Sentry.Client (Client, disableIntegration, pattern NON_RECORDING_CLIENT)
import Sentry.Client.Options (ClientOptions (..), TransportProvider (..), pattern DEFAULT_CLIENT_OPTIONS)
import Sentry.Event (CapturedEvent (..))
import Sentry.Init (withSentry, withSentryM)
import Sentry.Integration.Stacktrace
  ( AttachAnnotatedExceptionIntegration (..),
    AttachCallStackIntegration (..),
    AttachExceptionContextIntegration (..),
    ProcessStacktraceIntegration (..),
  )
import Sentry.Monad (HasClient (..), SentryT, askClient, runSentryT)
import Sentry.Scope (Scope, ScopeData (..), ScopeType (..), addBreadcrumb, addBreadcrumbs, clearBreadcrumbs)
import Sentry.Scope.IO (withIsolationScope, withScope)
