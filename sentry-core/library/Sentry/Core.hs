-- |  This module re-exports the API that consumers will typically use
-- when instrumenting: the 'Client' and 'ClientOptions' types, the capture
-- verbs, the 'withScope' \/ 'withIsolationScope' \/ 'withClient' helpers for
-- enriching events and binding a client, and the 'init' \/ 'close' lifecycle
-- functions for initializing the SDK.
--
-- This is the transport-agnostic surface: 'init' \/ 'withSentry' leave
-- 'Sentry.Client.Options.ClientOptions.transport' as 'Nothing' unless the
-- caller sets it explicitly, so a 'Sentry.Client.Client' built here is
-- non-recording by default. It's intended for integration authors,
-- custom-transport authors, and tests that don't want an HTTP dependency.
--
-- Application authors should generally prefer the @sentry@ package's
-- "Sentry" module instead, which re-exports this entire surface and adds a
-- default HTTP transport.
--
-- Plumbing modules ("Sentry.Transport", "Sentry.Integration", and the
-- "Sentry.Scope" operations) are intentionally not re-exported here, and are
-- intended to be imported with qualification.
module Sentry.Core
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
    captureExceptionWith,
    captureExceptionWith_,
    captureMessage,
    captureMessage_,
    captureUnhandledException,
    captureUnhandledException_,

    -- * Capture Overrides
    CaptureOverrides (..),

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

import Sentry.Capture
  ( CaptureOverrides (..),
    captureEvent,
    captureEvent_,
    captureException,
    captureExceptionWith,
    captureExceptionWith_,
    captureException_,
    captureMessage,
    captureMessage_,
    captureUnhandledException,
    captureUnhandledException_,
  )
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
