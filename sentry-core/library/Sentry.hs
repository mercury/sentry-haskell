-- |  This module re-exports the API that consumers will typically use
-- when instrumenting: the 'Client' and 'ClientOptions' types, the capture
-- verbs, the 'withScope' \/ 'withIsolationScope' \/ 'withClient' helpers for
-- enriching events and binding a client, and the 'init' \/ 'close' lifecycle
-- functions for initializing the SDK.
--
-- Plumbing modules ("Sentry.Transport", "Sentry.Integration", and the
-- "Sentry.Scope" operations) are intentionally not re-exported here, and are
-- intended to be imported with qualification.
module Sentry
  ( -- * Lifecycle
    init,
    close,
    withSentry,

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
    withClient,
    resolveClient,

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
import Sentry.Init (close, init, withSentry)
import Sentry.Integration.Stacktrace
  ( AttachAnnotatedExceptionIntegration (..),
    AttachCallStackIntegration (..),
    AttachExceptionContextIntegration (..),
    ProcessStacktraceIntegration (..),
  )
import Sentry.Scope (Scope, ScopeData (..), ScopeType (..), addBreadcrumb, addBreadcrumbs, clearBreadcrumbs, resolveClient)
import Sentry.Scope.IO (withClient, withIsolationScope, withScope)
import Prelude hiding (init)
