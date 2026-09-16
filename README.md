> [!CAUTION]
> This is a `0.0.0` release and the surface area is still moving. Module layout,
> option names, and transport APIs may change between commits without ceremony.

> [!NOTE]
> This is not an official Mercury Technologies product, nor is it affiliated
> with or endorsed by Sentry. It is an unofficial, community SDK.

# Sentry for Haskell

An (unofficial) [Sentry] SDK for Haskell. It captures exceptions, messages, and
structured events, attaches metadata from the active scopes, and sends them to a
Sentry backend.

[Sentry]: https://sentry.io

## Contents

- [Installation](#installation)
- [Quick Start](#quick-start)
- [Usage](#usage)
  - [Import Conventions](#import-conventions)
  - [Environment Variables](#environment-variables)
  - [Initializing the SDK](#initializing-the-sdk)
  - [Capturing Messages and Exceptions](#capturing-messages-and-exceptions)
  - [Working with Metadata](#working-with-metadata)
  - [Scopes](#scopes)
  - [Breadcrumbs](#breadcrumbs)
  - [Choosing a Transport](#choosing-a-transport)
  - [Testing](#testing)
- [How It Works](#how-it-works)
- [Reference](#reference)
- [Development](#development)
- [Frequently Asked Questions](#frequently-asked-questions)
- [Acknowledgements](#acknowledgements)

## Installation

This project is not yet published to Hackage, and it depends on `patrol`, which
is also unpublished. To use it, add the following `source-repository-package`
stanzas to your `cabal.project` and then list `sentry` (and/or `sentry-core`) as
a dependency of your package.

<details> <summary>cabal.project fragment</summary>

```
source-repository-package
  type: git
  location: https://github.com/MercuryTechnologies/sentry-haskell
  tag: main
  subdir: sentry

source-repository-package
  type: git
  location: https://github.com/MercuryTechnologies/sentry-haskell
  tag: main
  subdir: sentry-core

source-repository-package
  type: git
  location: https://github.com/tfausak/patrol
  tag: main
```

</details>

The SDK targets GHC 9.10 and 9.12 and is written against the GHC2024 language
edition.

## Quick Start

`withSentry` reads configuration variables from the environment, uses the
default asynchronous HTTP/1.1 transport, and attempts to drain enqueued
messages on application shutdown.

> [!TIP]
> Set the `SENTRY_DSN` environment variable to your project's DSN; without a
> DSN, the SDK will drop all outgoing events.

This example attaches metadata to a request and captures a message:

```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Default (def)
import Sentry qualified
import Sentry.Breadcrumb qualified
import Sentry.Scope qualified
import Sentry.User qualified

main :: IO ()
main =
  Sentry.withSentry def \_ ->
    Sentry.withIsolationScope \scope -> do
      Sentry.updateScope scope
        [ Sentry.Scope.setUser
            [ Sentry.User.setId "42",
              Sentry.User.setName "Alice"
            ],
          Sentry.Scope.setTag "feature" "checkout"
        ]

      Sentry.addBreadcrumb
        [ Sentry.Breadcrumb.setCategory "checkout",
          Sentry.Breadcrumb.setMessage "Payment submitted"
        ]

      Sentry.captureMessage_ Sentry.Info "Checkout completed"
```

The captured `"Checkout completed"` message includes Alice's ID and name, the
`"feature"` tag, and the `"Payment submitted"` breadcrumb.

See [Working with scopes](#scopes) and [Initializing the SDK](#initializing-the-sdk)
for more examples and configuration.

## Usage

### Import Conventions

Import `Sentry` for lifecycle, capture, and scope operations, `Sentry.Scope` for
pure scope builders, and record modules such as `Sentry.User` for their fields
and builders:

```haskell
import Sentry       qualified
import Sentry.Scope qualified
import Sentry.Level qualified
```

Import any builder modules you use in the same way:

```haskell
import Sentry.Breadcrumb     qualified
import Sentry.Event          qualified
import Sentry.Request        qualified
import Sentry.OsContext      qualified
import Sentry.AppContext     qualified
import Sentry.RuntimeContext qualified
import Sentry.Geo            qualified
import Sentry.User           qualified
```

> [!TIP]
> **Advanced protocol types**
> 
> Common metadata can be constructed and modified entirely through `Sentry.*`
> modules.
> 
> Explicit DSN parsing currently uses `Patrol.Type.Dsn.fromText`; constructing
> lower-level payloads such as thread data or debug metadata also
> requires Patrol imports.

### Environment Variables

`Sentry.Client.new` uses the standard Sentry environment variables to fill unset
`ClientOptions` fields, options supplied directly in code take precedence over
values supplied by the environment.

| Variable                       | `ClientOptions` field  | Default when unset in code and environment |
| ------------------------------ | ---------------------- | -------------------------------------------|
| `SENTRY_DSN`                   | `dsn`                  | `Sentry.Client.Options.Dsn.Disabled`       |
| `SENTRY_RELEASE`               | `release`              | `Nothing`                                  |
| `SENTRY_ENVIRONMENT`           | `environment`          | `"production"`                             |
| `SENTRY_DEBUG`                 | `debug`                | `False`                                    |
| `SENTRY_SAMPLE_RATE`           | `sampleRate`           | `1.0`                                      |

Booleans accept `1`/`true`/`yes`/`on` and `0`/`false`/`no`/`off`,
case-insensitively; anything else is treated as unset.

Sample rates parse as floats and are clamped to `[0, 1]`.

Invalid environment values are ignored and logged only when debug logging is
enabled.

### Initializing the SDK

Wrap your application in `withSentry` to set the process-wide client and attempt
to drain enqueued messages when the application exits:

```haskell
import Data.Default (def)
import Sentry qualified

main :: IO ()
main =
  Sentry.withSentry def \_client ->
    runApplication
```

This reads `SENTRY_DSN` (and other `SENTRY_*` environment variables) and uses
the HTTP/1.1 transport by default.

Use `dsn = Sentry.Client.Options.Dsn.Disabled` to prevent recording even when `SENTRY_DSN` is set;
use `dsn = Sentry.Client.Options.Dsn.Explicit value` to override the environment.

Disabled clients still run integration setup but do not create a default
transport worker.

Pass a `ClientOptions` record to override environment settings. This example
supplies a DSN and environment while keeping the default transport:

```haskell
import Data.Default (def)
import Patrol.Type.Dsn qualified
import Sentry.Client.Options.Dsn qualified
import Sentry qualified

main :: IO ()
main = do
  dsn <- maybe (fail "Invalid Sentry DSN") pure $
    Patrol.Type.Dsn.fromText "https://public@o0.ingest.sentry.io/0"
  let clientOptions =
        def
          { Sentry.dsn = Sentry.Client.Options.Dsn.Explicit dsn,
            Sentry.environment = Just "production"
          }
  Sentry.withSentry clientOptions \_client ->
    runApplication
```

To use a different client for one request or test, wrap the action in
`withScopedClient`:

```haskell
import Sentry.Level qualified

Sentry.withScopedClient opts do
  Sentry.captureMessage_ Sentry.Level.Info "Processing request"
  handleRequest request
```

The `"Processing request"` message and events captured by `handleRequest` use
the new client with the surrounding scope metadata.

On exit, the previous scopes are restored and the new client is closed; other
threads keep their existing clients.

`close` returns a `ShutdownResponse` and is safe to call repeatedly.

See [Sentry.Init](sentry-core/library/Sentry/Init.hs) for detailed shutdown and
exception behavior.

### Capturing Messages and Exceptions

The following example demonstrates how both string messages and structured
Haskell exceptions can be captured and reported to Sentry:

```haskell
import Control.Exception.Safe (SomeException, try)
import Sentry.Level qualified
import Sentry qualified

reportTrouble :: IO ()
reportTrouble = do
  Sentry.captureMessage_ Sentry.Level.Warning "payment processor latency is high"

  try attemptCharge >>= \case
    Left err -> Sentry.captureException_ (err :: SomeException)
    Right () -> pure ()
```

### Working with Metadata

With a recording client, `Sentry.setUser` and `Sentry.setTag` select the isolation
scope automatically, attaching request-wide metadata without a scope handle:

```haskell
Sentry.setUser [Sentry.User.setId "42"]
Sentry.setTag "request_id" "req-123"
```

`Sentry.setTransaction` selects the current scope to name the current operation.

To update a specific scope, pass its handle to `Sentry.updateScope`; this
function applies pure builder updates atomically and in list order

```haskell
import Sentry qualified
import Sentry.Scope qualified
import Sentry.User qualified

setCheckoutMetadata :: Sentry.Scope -> IO ()
setCheckoutMetadata scope =
  Sentry.updateScope scope
    [ Sentry.Scope.setUser
        [ Sentry.User.setId "42",
          Sentry.User.setName "Alice"
        ],
      Sentry.Scope.setTag "feature" "checkout"
    ]
```

This replaces the selected scope's user with Alice's ID and name and sets its
`"feature"` tag to `"checkout"`.

Breadcrumbs, events, requests, and typed contexts have corresponding builder
modules that follow the same composition rules.

Updates can be factored out into named fragments and composed with `<>`:

```haskell
checkoutMetadata :: Sentry.ScopeUpdate
checkoutMetadata =
  Sentry.Scope.setTag "feature" "checkout"
    <> Sentry.Scope.setTag "team" "payments"

setCheckoutTeam :: Sentry.Scope -> IO ()
setCheckoutTeam scope =
  Sentry.updateScope scope
    [ checkoutMetadata,
      Sentry.Scope.setTag "team" "checkout-platform"
    ]
```

In `setCheckoutTeam`, the second update replaces the `"team"` tag's value of
`"payments"` with `"checkout-platform"`.

Use `Sentry.User.with` to compute an update from the user's existing fields:

```haskell
fillMissingUserName :: Sentry.Scope -> IO ()
fillMissingUserName scope =
  Sentry.updateScope scope $
    Sentry.Scope.modifyExistingUser $
      Sentry.User.with \user ->
        Sentry.User.setName
          (if user.name == "" then user.username else user.name)
```

This uses the username when the name is empty, leaving an existing name
unchanged; if the scope has no user, `modifyExistingUser` does nothing.

`with` reads the record at that point in the update sequence, including earlier
updates; `Sentry.Request.with`, `Sentry.Event.with`, and the other record helpers
follow the same pattern.

See [Setting and modifying users](docs/scopes.md#setting-and-modifying-users)
for more examples.

### Scopes

The global scope holds process-wide metadata, the isolation scope holds metadata
for a request or task, and the current scope holds metadata for arbitrary
operations where a caller may want to temporarily supply additional metadata.

Use `withIsolationScope` to group metadata for a request or asynchronous job
processing task; it clones the isolation and current scopes, creating either
if absent, and restores the previous scope bindings when the action finishes.

Use `withScope` for all other operations that require their own scope; anything
that doesn't represent some context with a meaningful lifetime within your
application.

In this example, `appUser` supplies the request user's account ID and email,
and `runHandler` is the application code that handles the request:

```haskell
import Sentry qualified
import Sentry.Scope qualified
import Sentry.User qualified

handleRequest :: AppUser -> IO ()
handleRequest appUser =
  Sentry.withIsolationScope \scope -> do
    Sentry.updateScope scope
      [ Sentry.Scope.setUser
          [ Sentry.User.setId appUser.accountId,
            Sentry.User.setEmail appUser.email
          ],
        Sentry.Scope.setTag "feature" "checkout"
      ]
    runHandler
```

The user and `"feature"` tag set here provide metadata for messages, events, and
exceptions captured within `runHandler`.

See [Scopes and metadata](docs/scopes.md) for merge rules, nested scope examples,
and updates to users and contexts.

### Breadcrumbs

Breadcrumbs are a trail of events leading up to a problem; `addBreadcrumb`
appends to the isolation scope directly, since they are almost always meant
to be stored there:

```haskell
import Sentry qualified
import Sentry.Breadcrumb qualified

trackPayment :: IO ()
trackPayment =
  Sentry.addBreadcrumb
    [ Sentry.Breadcrumb.setCategory "ui",
      Sentry.Breadcrumb.setMessage "user clicked 'pay'"
    ]
```

Subsequent events captured with this isolation scope include the
`"user clicked 'pay'"` breadcrumb. `addBreadcrumb` keeps the trail within
`ClientOptions.maxBreadcrumbs`.

### Choosing a Transport

The `sentry` package provides asynchronous HTTP transports backed by a dedicated
worker thread and a bounded queue; when the queue is full, events are dropped
rather than blocking the caller.

> [!NOTE]
> All transports honor rate-limits provided by the `X-Sentry-Rate-Limits` and
> `Retry-After` headers, as well as HTTP `429` responses.
>
> This behavior is not configurable.

`Sentry.withSentry`/`Sentry.init` already default to the HTTP/1.1 async
transport with a queue size of `Sentry.Transport.Executor.Async.defaultQueueSize`
when left unset (see [Initializing the SDK](#initializing-the-sdk)).

Set `transport` explicitly to override it, e.g. to switch to HTTP/2 or tune the
queue size:

```haskell
import Sentry.Transport.HTTP2.Async qualified
import Sentry.Transport.HTTP.Async qualified

-- HTTP/1.1 (recommended)
http1Transport = Just (Sentry.Transport.HTTP.Async.new def 1000)

-- HTTP/2 (experimental): multiplexes envelopes over a single connection
http2Transport = Just (Sentry.Transport.HTTP2.Async.new def 1000)
```

> [!CAUTION]
> The HTTP/2 options record exposes a `validateCert` boolean, which disables
> TLS certificate validation; this is intended **for testing against a local
> mock server only**!

### Testing

`Sentry.Test` provides an in-memory transport for inspecting recorded events in
unit tests. `Sentry.Test.withClient` binds a client with a test DSN to the isolation
scope for the action and returns the transport for inspection:

```haskell
import Sentry qualified
import Sentry.Level qualified
import Sentry.Test qualified
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = it "captures a message" do
  (_, transport) <- Sentry.Test.withClient \_ ->
    Sentry.captureMessage_ Sentry.Level.Info "hello"
  events <- Sentry.Test.fetchAndClearEvents transport
  length events `shouldBe` 1
```

Use `Sentry.Test.withCustomClient opts` when testing custom configuration, such as
sampling or event processors.

Use `Sentry.Test.mkClient transport` or `Sentry.Test.mkCustomClient transport opts`
when a test needs to manage client bindings itself.

> [!TIP]
> Use `fetchAndClearEnvelopes` to inspect the raw envelopes and
> `fetchAndClearDrops` to inspect recorded discards.

## How It Works

The SDK uses four main abstractions:

- A `Scope` stores metadata associated with captured events.
- A `Client` holds configuration options, a transport, and integrations.
- An `Integration` can process or discard events before delivery.
- A `Transport` delivers serialized envelopes to Sentry.

When `captureEvent`, `captureException`, or `captureMessage` records an event,
the SDK:

1. Resolves the client by checking the current, isolation, and global scopes in
   that order.
2. Merges metadata in global, isolation, and current order, applies it to the
   event, and runs the scope's event processor.
3. Runs the integration processors, fills in default values, and invokes
   `beforeSend`.
4. Applies sampling, wraps a retained event in an envelope, and passes it to
   the transport.

The SDK counts events excluded by sampling, blocked by rate limits, or discarded
for other reasons (e.g. a full transport queue); transports include these
counts in client reports sent to Sentry.

## Reference

Commonly set `ClientOptions` fields:

| Field              | Type                                           | Purpose                                                                  |
| ------------------ | ---------------------------------------------- | ------------------------------------------------------------------------ |
| `dsn`              | `DsnSource`                                    | Inherit from the environment, disable, or use an explicitly provided DSN |
| `transport`        | `Maybe TransportProvider`                      | How events are sent (see [Choosing a Transport](#choosing-a-transport))  |
| `environment`      | `Maybe Text`                                   | Environment tag (e.g. `"production"`)                                    |
| `release`          | `Maybe Text`                                   | Release identifier attached to events                                    |
| `sampleRate`       | `Maybe Float`                                  | Fraction of events to send, in `[0,1]`                                   |
| `sendDefaultPII`   | `Bool`                                         | Permission for integration-defined automatic PII collection              |
| `maxBreadcrumbs`   | `Word`                                         | Per-scope breadcrumb cap                                                 |
| `beforeSend`       | `Maybe (CapturedEvent -> Maybe Event)`         | Final hook to rewrite or drop each event                                 |
| `beforeBreadcrumb` | `Maybe (Breadcrumb -> Maybe Breadcrumb)`       | Hook to rewrite or drop each breadcrumb                                  |
| `integrations`     | `Vector SomeIntegration`                       | Extra integrations to run                                                |
| `shutdownTimeout`  | `NominalDiffTime`                              | Time budget for draining the transport on close                          |
| `debug`            | `Maybe Bool`                                   | Log dropped events to `stderr`                                           |

### Capturing

| Function                       | Captures                                              |
| ------------------------------ | ----------------------------------------------------- |
| `captureMessage lvl msg`       | A plain message at a given `Level`                    |
| `captureException e`           | Any `Exception`, building an event (with stack trace) |
| `captureEvent ev`              | A fully formed `Sentry.Event`                         |

Each has a `_`-suffixed variant that discards the returned `Maybe EventId`.

### Built-in Integrations

| Integration                            | Effect                                                       |
| -------------------------------------- | ------------------------------------------------------------ |
| `AttachCallStackIntegration`           | Attaches the call-site stack to captured events              |
| `AttachAnnotatedExceptionIntegration`  | Lifts `annotated-exception` annotations onto the event       |
| `AttachExceptionContextIntegration`    | Attaches exception context as event context                  |
| `ProcessStacktraceIntegration`         | Post-processes frames, applying in-app include/exclude rules |

### Transport Responses

Transport operations return explicit sum types rather than throwing:

| Type               | Constructors                                                                                                   |
| ------------------ | -------------------------------------------------------------------------------------------------------------- |
| `SendResponse`     | `SendProcessed`, `SendFailed_QueueFull`, `SendFailed_Shutdown`, `SendFailed_Other`                             |
| `FlushResponse`    | `FlushSucceeded`, `FlushFailed_TimedOut`, `FlushFailed_QueueFull`, `FlushFailed_Shutdown`, `FlushFailed_Other` |
| `ShutdownResponse` | `ShutdownSucceeded`, `ShutdownFailed_TimedOut`, `ShutdownFailed_AlreadyShutdown`, `ShutdownFailed_Other`       |

## Development

Clone the repository and enter the development shell with `nix develop` (or
`direnv allow` if you use direnv).

The shell provides GHC 9.10, `cabal-install`, `cabal-gild`, `fourmolu`,
`ghciwatch`, the `kent-server` mock backend, and the profiling toolchain.

Common tasks are wrapped in the `just` command runner:

```shell
$ just build                 # build the 'sentry' package
$ just build-core            # build just 'sentry-core'
$ just test                  # run the 'sentry' test suite
$ just test sentry-core      # run a specific package's tests
$ just bench                 # run benchmarks
$ just ghciwatch             # live-reloading REPL
$ just ghciwatch-unit        # live-reloading REPL that re-runs the unit tests
```

For performance work, the `profile-run`, `profile-space`, and `profile-time`
recipes drive the transport harness against a local TLS sink; the latter two use
`cabal.project.profiling`, which turns on late cost-centre profiling so the
profile reflects optimized code.

> [!IMPORTANT]
> `.cabal` files are hand-written and are the source of truth. `cabal-gild`
> keeps module lists (`exposed-modules`, `other-modules`, `extra-source-files`)
> current via `-- cabal-gild: discover` pragmas, and keeps shared settings
> (warnings, extensions, `tested-with`) in sync via `-- cabal-gild: fragment`
> pragmas pointing at `cabal/`. Run `just format` after adding a source
> file or editing a fragment, before building.

Formatting is handled by `nix fmt` (`just format` / `just check-format`).

## Frequently Asked Questions

### What's missing?

The core capture path, scopes, integrations, and HTTP transports are usable
today, but some things are still outstanding:

- publication to Hackage
- a broader catalogue of integrations and context providers
- transaction / performance monitoring
- a stable, frozen public API

## Acknowledgements

[Mercury Technologies](https://mercury.com/), for providing the time and space
to build this project during the course of my work (if that sounds fun,
[we're hiring!](https://mercury.com/jobs)).

[`patrol`](https://github.com/tfausak/patrol), for the Sentry protocol types
that this SDK serializes to the wire.

[`kent`](https://github.com/mozilla-services/kent), which made development and
integration testing significantly more pleasant than having to constantly make
calls to the Sentry API itself.

[`sentry-rust`](https://github.com/getsentry/sentry-rust), which initially
inspired much of this project's architecture.

[`hs-opentelemetry`](https://github.com/iand675/hs-opentelemetry), which provides
the thread-local context machinery the scope system is built on.

[`sentry-rust`]: https://github.com/getsentry/sentry-rust
[`patrol`]: https://github.com/tfausak/patrol
