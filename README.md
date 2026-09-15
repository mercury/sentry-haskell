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

Initialize the SDK, attach metadata for a request or task, and capture an event:

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

`withSentry` reads the environment, uses the default asynchronous HTTP/1.1
transport, and drains it when the application exits. Remember  to set `SENTRY_DSN`
to your project's DSN before running the application; without a DSN the client
won't record any events.

The captured message includes the scope's user and tag, along with the
breadcrumb.

See [Working with scopes](#scopes) and [Initializing the SDK](#initializing-the-sdk)
for more examples and configuration.

## Usage

### Import Conventions

Import `Sentry` for lifecycle, capture, and scope operations; import `Sentry.Scope`
for pure scope builders, and the corresponding record modules like
as `Sentry.User` and `Sentry.Event` for record fields and builders.

Use qualified, unaliased imports for these scope & type fields:

```haskell
import Sentry                qualified
import Sentry.Scope          qualified
import Sentry.Level          qualified
```

...as well as for the record modifiers:

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
> lower-level payloads such like the thread interface or debug metadata also
> requires Patrol imports.

### Environment Variables

`Sentry.Client.new` resolves the standard Sentry environment variables to
field values that are set on `ClientOptions`; any options set directly in
code override values sourced from the environment.

| Variable                       | `ClientOptions` field  | Terminal default if unset by both code and env     |
| ------------------------------ | ---------------------- | -------------------------------------------------- |
| `SENTRY_DSN`                   | `dsn`                  | `Sentry.Client.Options.Dsn.Disabled` (non-recording client)              |
| `SENTRY_RELEASE`               | `release`              | `Nothing`                                          |
| `SENTRY_ENVIRONMENT`           | `environment`          | `"production"`                                     |
| `SENTRY_DEBUG`                 | `debug`                | `False`                                            |
| `SENTRY_SAMPLE_RATE`           | `sampleRate`           | `1.0`                                              |

Booleans (`SENTRY_DEBUG`) accept `1`/`true`/`yes`/`on` and `0`/`false`/`no`/`off`,
case-insensitively; anything else is treated as unset.

Sample rates parse as floats and are clamped to `[0, 1]`.

A variable that's set but fails to parse is ignored and will only log a
message if the final debug setting is enabled.

Explicit code settings suppress diagnostics for the environment values they
override.

### Initializing the SDK

Wrap your application in `withSentry` to set the process-wide client and drain
its transport when the application exits:

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

The `ClientOptions` record can be modified, which will override any values
pulled from the environment, as follows:

```haskell
import Data.Default (def)
import Patrol.Type.Dsn qualified
import Sentry.Client.Options.Dsn qualified
import Sentry qualified
import Sentry.Transport.HTTP2.Async qualified

main :: IO ()
main = do
  dsn <- maybe (fail "Invalid Sentry DSN") pure $
    Patrol.Type.Dsn.fromText "https://public@o0.ingest.sentry.io/0"
  let clientOptions =
        def
          { Sentry.dsn = Sentry.Client.Options.Dsn.Explicit dsn,
            Sentry.environment = Just "production",
            Sentry.transport = Just (Sentry.Transport.HTTP2.Async.new def 1000)
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

The action keeps the surrounding scope metadata and uses the new client for
captures. On exit, the previous scopes are restored and the new client is
closed. Other threads keep their existing clients.

`close` returns a `ShutdownResponse`, and is safe to call repeatedly.

See [Sentry.Init](sentry-core/library/Sentry/Init.hs) for detailed shutdown and
exception behavior.

### Capturing Messages and Exceptions

Once a client is bound to the global scope, the capture functions can be called
from anywhere:

```haskell
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

These automatic calls are called ambient operations. `Sentry.setTransaction`
selects the current scope to name the current operation. Without a recording
client, ambient operations leave metadata unchanged.

Explicit scope edits remain available before initialization. Metadata builders
describe pure changes; apply them with `Sentry.updateScope` to change several
fields on a scope atomically:

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

`Sentry.User` builds changes to a user object, while `Sentry.Scope` builds
changes to scope metadata.

The same pattern applies to breadcrumbs, events, requests, and typed contexts
through their respective `Sentry.*` modules.

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

Use `Sentry.User.with` to compute an update from the user's existing fields:

```haskell
fillMissingUserName :: Sentry.Scope -> IO ()
fillMissingUserName scope =
  Sentry.updateScope scope . Sentry.Scope.modifyUser $
    Sentry.User.with \user ->
      Sentry.User.setName
        (if user.name == "" then user.username else user.name)
```

`with` reads the record at that point in the update sequence, including earlier
changes; `Sentry.Request.with`, `Sentry.Event.with`, and the other record helpers
follow the same pattern.

> [!NOTE]
> `Sentry.setUser` builds a replacement user, while `Sentry.modifyUser` updates
> user metadata on either an existing or empty user value depending on what is
> present in the scope, while `Sentry.modifyExistingUser` skips modifications
> if no user exists on the scope being edited.
>
> All builders should follow this rough pattern of `set*`, `modify*`,
> `modifyExisting*`.
>
> See [Scopes](#scopes) for how metadata from different scope layers is combined.

### Scopes

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
    -- captured exceptions in runHandler include this request's metadata
    runHandler
```

### Breadcrumbs

Breadcrumbs are a trail of events leading up to a problem; `addBreadcrumb`
appends to the ambient isolation scope, so it does not need a `Scope` handle:

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

The number of breadcrumbs retained per scope is capped by
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

`Sentry.Test` provides an in-memory transport so you can assert on what *would*
have been sent to Sentry within your unit testing framework.

Use `Sentry.Test.withClient` to create a test transport and initialize a client bound
to the isolation scope for the action. It supplies the test DSN automatically
and returns the transport for inspection:

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

Use `Sentry.Test.mkClient transport` and `Sentry.Test.mkCustomClient transport opts` for tests
that need to manage client binding themselves,

> [!TIP]
> Use `fetchAndClearEnvelopes` to inspect the raw envelopes and
> `fetchAndClearDrops` to inspect recorded discards.

## How It Works

The SDK is organized around the following abstractions:

- A `Scope`, which defines an interface for associating contextual metadata
  with an enclosed scope of execution and is separated into three tiers of
  responsibility that are merged before an event is handed off to the `Client`:
  - a process-wide `Global` scope
  - a per-task `Isolation` scope
  - a narrowly-bound `Current` scope
- A `Client`, which records configuration options, a `Transport`, and a list of
  `Integration`s
- `Integration`s, which observe, rewrites, and potentially discard events as
  they pass through the `Client` on their way to a `Transport`
- A `Transport`, which delivers a serialized envelope to Sentry

When an artifact is captured using `captureEvent`, `captureException`, or `captureMessage`, the SDK follows this pipeline: resolve the `Client` bound to the nearest `Scope`, merge the three scope layers and apply them to the `Event`, run each integration's `processEvent` hook in order, fill in default values, invoke the user-provided `beforeSend` hook, apply sampling based on the configured rate, wrap the result in an `Envelope`, and deliver it to the transport.

If an event is discarded at any point in this pipeline, the SDK increments an internal counter for the discard stage. A client report with counters for each stage is sent to Sentry at regular intervals.

`withSentry` sets the default client for the process. Use `withScopedClient` to
override it within an action, for example in a test or a request handler. See
[Initializing the SDK](#initializing-the-sdk) for examples.

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

