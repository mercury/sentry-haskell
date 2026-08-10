> [!CAUTION]
> This is a `0.0.0` release and the surface area is still moving. Module layout,
> option names, and transport APIs may change between commits without ceremony.

> [!NOTE]
> This is not an official Mercury Technologies product, nor is it affiliated
> with or endorsed by Sentry. It is an unofficial, community SDK.

# Sentry for Haskell

An (unofficial) [Sentry] SDK for Haskell. It captures exceptions, messages, and
structured events from a running program and ships them to a Sentry backend,
enriched with whatever contextual metadata the surrounding scope has accumulated.

[Sentry]: https://sentry.io

## Contents

- [How It Works](#how-it-works)
- [Installation](#installation)
- [Usage](#usage)
  - [Import Conventions](#import-conventions)
  - [Environment Variables](#environment-variables)
  - [Initializing the SDK](#initializing-the-sdk)
  - [Capturing Messages and Exceptions](#capturing-messages-and-exceptions)
  - [Scopes](#scopes)
  - [Breadcrumbs](#breadcrumbs)
  - [Optics (optional)](#optics-optional)
  - [Choosing a Transport](#choosing-a-transport)
  - [Testing](#testing)
- [Reference](#reference)
- [Development](#development)
- [Frequently Asked Questions](#frequently-asked-questions)
- [Acknowledgements](#acknowledgements)

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

When an artifact is captured using `captureException` or `captureMessage`, the SDK follows this pipeline: resolve the `Client` bound to the nearest `Scope`, merge the three scope layers and apply them to the `Event`, run each integration's `processEvent` hook in order, fill in default values, invoke the user-provided `beforeSend` hook, apply sampling based on the configured rate, wrap the result in an `Envelope`, and deliver it to the transport.

If an event is discarded at any point in this pipeline, the SDK increments an internal counter for the discard stage. A client report with counters for each stage is sent to Sentry at regular intervals.

> [!IMPORTANT]
> `init` (and therefore `withSentry`) binds the client onto the _global_ scope,
> so `captureException` / `captureMessage` work anywhere in the process.
> 
> For situations where true process-wide global state is unnacceptable (e.g.
> tests, multi-tenant servers), `withClient` can be used to bind a `Client` to
> a special-purpose thread-local variable that intercepts global scope resolution.

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

The optics-based API (see [Optics](#optics-optional)) is optional and depends
on orphan instances for `patrol` types defined in the `patrol-optics` package.

Neither `sentry` nor `sentry-core` carries an `optics` dependency on its own:

- `sentry-optics` acts as an opinionated drop-in replacement for`sentry`
- `sentry-core-optics` provides a transport-agnostic base, intended as a
  drop-in replacement for `sentry-core`

`sentry-optics` depends on `sentry-core-optics`:

<details> <summary>cabal.project fragment (sentry-optics)</summary>

```
source-repository-package
  type: git
  location: https://github.com/MercuryTechnologies/sentry-haskell
  tag: main
  subdir: sentry-optics

source-repository-package
  type: git
  location: https://github.com/MercuryTechnologies/sentry-haskell
  tag: main
  subdir: sentry-core-optics

source-repository-package
  type: git
  location: https://github.com/MercuryTechnologies/sentry-haskell
  tag: main
  subdir: patrol-optics
```

</details>

<details> <summary>cabal.project fragment (sentry-core-optics only)</summary>

```
source-repository-package
  type: git
  location: https://github.com/MercuryTechnologies/sentry-haskell
  tag: main
  subdir: sentry-core-optics

source-repository-package
  type: git
  location: https://github.com/MercuryTechnologies/sentry-haskell
  tag: main
  subdir: patrol-optics
```

</details>

The SDK targets GHC 9.10 and 9.12 and is written against the GHC2024 language
edition.

## Usage

### Import Conventions

The API is meant to be used through qualified imports. The examples below — and
recommended usage generally — alias the modules like so:

```haskell
import Sentry                qualified as Sentry          -- lifecycle, capture, the core surface
import Sentry.Scope          qualified as Scope           -- scope mutators (setTag, setLevel, …)
import Sentry.Level          qualified as Level           -- severity values (Level.Warning, …)
import Sentry.BreadcrumbType qualified as BreadcrumbType  -- breadcrumb kinds (BreadcrumbType.Navigation, …)
```

`Sentry.Level` and `Sentry.BreadcrumbType` re-export the corresponding `patrol`
sum types under the `Sentry` namespace, so their constructors can be named without
depending on `patrol` directly. The optional optics API swaps this set of
imports for its own — see [Optics](#optics-optional).

### Environment Variables

`Sentry.Client.new` (which both `Sentry.Core.init`/`withSentry` and the
`sentry` package's `Sentry.init`/`withSentry` build on) resolves a standard
set of environment variables to fill in whatever `ClientOptions` fields the
caller left unset. **Code configuration always wins**: if you set a field
explicitly, its environment variable is ignored entirely.

| Variable                       | `ClientOptions` field  | Terminal default if unset by both code and env     |
| ------------------------------ | ---------------------- | -------------------------------------------------- |
| `SENTRY_DSN`                   | `dsn`                  | `Nothing` (non-recording client)                   |
| `SENTRY_RELEASE`               | `release`              | `Nothing`                                          |
| `SENTRY_ENVIRONMENT`           | `environment`          | `"production"`                                     |
| `SENTRY_DEBUG`                 | `debug`                | `False`                                            |
| `SENTRY_SAMPLE_RATE`           | `sampleRate`           | `1.0`                                              |
| `SENTRY_TRACES_SAMPLE_RATE`    | `tracesSampleRate`     | `Nothing`                                          |
| `SENTRY_PROFILES_SAMPLE_RATE`  | `profilesSampleRate`   | `Nothing`                                          |

Booleans (`SENTRY_DEBUG`) accept `1`/`true`/`yes`/`on` and `0`/`false`/`no`/`off`,
case-insensitively; anything else is treated as unset.

Sample rates parse as floats and are clamped to `[0, 1]`.

A variable that's set but fails to parse is **ignored** and will only log a
message if the SDK initialized in debug mode.

### Initializing the SDK

`withSentry` brackets the SDK's lifecycle:

* it builds a client from your options
* binds it to the global scope
* runs your application
* then flushes and shuts the transport down on exit

```haskell
import Data.Default (def)
import Sentry qualified as Sentry

main :: IO ()
main =
  Sentry.withSentry def \_client ->
    runApplication
```

This reads `SENTRY_DSN` (and other `SENTRY_*` environment variables), and uses
the HTTP/1.1 transport by default.

To configure from code instead, or to override the transport:

```haskell
import Data.Default (def)
import Patrol.Type.Dsn qualified as Dsn
import Sentry (ClientOptions (..))
import Sentry qualified as Sentry
import Sentry.Transport.HTTP2.Async qualified as Http2

main :: IO ()
main = do
  dsn <- Dsn.fromText "https://public@o0.ingest.sentry.io/0"
  let clientOptions =
        def
          { dsn = Just dsn,
            environment = Just "production",
            transport = Just (Http2.new def 1000) -- overrides the default HTTP/1.1 transport
          }
  Sentry.withSentry clientOptions \_client ->
    runApplication
```

### Capturing Messages and Exceptions

Once a client is bound to the global scope, the capture functions can be called
from anywhere:

```haskell
import Sentry.Level qualified as Level
import Sentry qualified

reportTrouble :: IO ()
reportTrouble = do
  Sentry.captureMessage_ Level.Warning "payment processor latency is high"

  try attemptCharge >>= \case
    Left err -> Sentry.captureException_ (err :: SomeException)
    Right () -> pure ()
```

### Scopes

Use `withScope` (or `withIsolationScope` for per-request/per-task boundaries) to
attach metadata that should apply to everything captured inside the block. The
scope is forked on entry and restored on exit, and any synchronous exception
that escapes carries the merged scope with it.

```haskell
import Sentry.Level qualified as Level
import Patrol.Type.User qualified as User
import Sentry qualified
import Sentry.Scope qualified as Scope

handleRequest :: User.User -> IO ()
handleRequest user =
  Sentry.withIsolationScope \scope -> do
    Scope.setUser scope user
    Scope.setTag scope "feature" "checkout"
    Scope.setLevel scope Level.Warning
    -- anything captured in here inherits the user, tag, and level
    runHandler
```

The scope mutators (`setTag`, `setUser`, `setLevel`, `setExtra`, `setContext`,
`setFingerprint`, `setTransaction`, and their `unset`/`remove`/`clear`
counterparts) live in `Sentry.Scope` and operate on the `Scope` handle the
bracket hands you.

> [!NOTE]
> `captureException` follows an "innermost wins" rule for scope selection: if an
> exception escaped a `withScope`/`withIsolationScope` block, the scope captured
> at the throw site is attached to it and treated as authoritative; otherwise
> the scope ambient at the call site is used.

> [!TIP]
> Asynchronous exceptions are **not** annotated with scope data when they pass
> through `withScope`, since wrapping them would change their identity and could
> break cancellation semantics.
>
> Capture them explicitly if you want them > reported.

### Breadcrumbs

Breadcrumbs are a trail of events leading up to a problem. `addBreadcrumb`
appends to the ambient scope, so it does not need a `Scope` handle:

```haskell
import Patrol.Type.Breadcrumb qualified as Breadcrumb
import Sentry qualified

trackClick :: IO ()
trackClick =
  Sentry.addBreadcrumb
    Breadcrumb.empty
      { Breadcrumb.category = "ui",
        Breadcrumb.message = "user clicked 'pay'"
      }
```

The number of breadcrumbs retained per scope is capped by
`ClientOptions.maxBreadcrumbs`.

### Optics (optional)

For composable scope and record manipulation, the optional `sentry-optics`
package provides an [`optics`](https://github.com/well-typed/optics)-based layer
on top of `patrol-optics`.

It is used through two imports, one qualified and one not, with the
`OverloadedLabels` extension enabled:

```haskell
{-# LANGUAGE OverloadedLabels #-}

import Sentry.Optics qualified as Sentry  -- the full Sentry surface + editScope/empty*
import Sentry.Optics.Prelude              -- import unqualified, *in place of* `import Optics`
```

> [!NOTE]
> Instrumentation/integration authors who want the optics API without the HTTP
> transport dependency should depend on `sentry-core-optics` and import
> `Sentry.Core.Optics`/`Sentry.Core.Optics.Prelude` instead.

`Sentry.Optics` is a drop-in for `Sentry` that additionally exports `editScope`,
`apply` / `runScopeUpdate` / `ScopeUpdate`, and `empty`-prefixed record values
(e.g. `emptyUser`, `emptyBreadcrumb`, `emptyRequest`, `emptyEvent`) that can
be used as builders for record construction.

`Sentry.Optics.Prelude` re-exports the `optics` vocabulary, the state operators
(`?=` / `.=` / `%=`), `&~` for applying those operators to a plain value, as
well as the field and value labels, so it replaces `import Optics` entirely.


#### Scope Updates

`editScope` applies a block of optic assignments to a scope as a single atomic
update — the optics counterpart to the effectful `Sentry.Scope` setters:

```haskell
Sentry.withIsolationScope \scope ->
  Sentry.editScope scope do
    #level ?= #warning
    #tags % at "feature" ?= "checkout"
    #user  ?= (Sentry.emptyUser & #email .~ "alice@example.com")
```

#### Value Labels

`#warning`, `#error`, `#navigation`, ... are labels that correspond to
`patrol`'s sum types, resolved by the type the surrounding optic expects.

That is to say, the same `#error` is a `Level` under `#level` and a
`BreadcrumbType` under `#type_`.

#### Field Labels

The `#field` lenses and `#_Constructor` prisms for `patrol`'s own types come
from the `patrol-optics` package (`Patrol.Optics`). Paired with the `empty*`
values, they let you build a record field by field:

```haskell
user = Sentry.emptyUser & #email .~ "alice@example.com" & #username .~ "alice"
```

For longer records, `&~` applies a block of optic assignments to a plain value
— the value-level counterpart to `editScope`, which applies one to a live scope:

```haskell
crumb =
  Sentry.emptyBreadcrumb &~ do
    #type_ ?= #navigation
    #category .= "ui"
    #message .= "user clicked 'pay'"
```

The same labels work on values you already have, so a `ClientOptions.beforeSend`
hook can rewrite a captured `Patrol.Event` the same way.

### Choosing a Transport

The `sentry` package provides asynchronous HTTP transports backed by a dedicated
worker thread and a bounded queue; when the queue is full, events are dropped
rather than blocking the caller.

> [!NOTE]
> All transports honor rate-limits provided by the `X-Sentry-Rate-Limits` and
> `Retry-After` headers, as well as HTTP `429` responses.
>
> This behavior is not configurable.

`Sentry.withSentry`/`Sentry.init` (from the `sentry` package) already default
`ClientOptions.transport` to the HTTP/1.1 async transport with a queue size of
`Sentry.Transport.Executor.Async.defaultQueueSize` when left unset (see
[Initializing the SDK](#initializing-the-sdk)).

Set `transport` explicitly to override it, e.g. to switch to HTTP/2 or tune the
queue size:

```haskell
import Sentry.Transport.HTTP2.Async qualified as Http2
import Sentry.Transport.HTTP.Async qualified as Http1

-- HTTP/1.1 (recommended)
http1Transport = Just (Http1.new def 1000)

-- HTTP/2 (experimental): multiplexes envelopes over a single connection
http2Transport = Just (Http2.new def 1000)
```

> [!CAUTION]
> The HTTP/2 options record exposes a `validateCert` boolean, which disables
> TLS certificate validation; this is intended **for testing against a local
> mock server only**!

### Testing

`Sentry.Test` provides an in-memory transport so you can assert on what *would*
have been sent to Sentry within your unit testing framework.

It exposes a `TEST_DSN`, a `TestTransport`, scope-isolation helpers, and accessors
that drain the collected data:

```haskell
import Data.Default (def)
import Sentry.Level qualified as Level
import Sentry (ClientOptions (..))
import Sentry qualified
import Sentry.Test qualified as Test
import Sentry.Transport (SomeTransport (..))
import Witch qualified

spec :: Spec
spec = it "captures a message" do
  transport <- Test.new
  let opts =
        def
          { transport = Just (Witch.from (SomeTransport transport)),
            dsn = Just Test.TEST_DSN
          }
  Test.withGlobalScope $
    Sentry.withSentry opts \_ ->
      Sentry.captureMessage_ Level.Info "hello"
  events <- Test.fetchAndClearEvents transport
  length events `shouldBe` 1
```

> [!TIP]
> Use `fetchAndClearEnvelopes` to inspect the raw envelopes and
> `fetchAndClearDrops` to inspect recorded discards.

## Reference

### Selected `ClientOptions`

`ClientOptions` is constructed with `def` (or the `DEFAULT_CLIENT_OPTIONS`
pattern) and updated record-style.

Commonly set `ClientOptions` fields:

| Field               | Type                                          | Purpose                                                                 |
| ------------------- | --------------------------------------------- | ------------------------------------------------------------------      |
| `dsn`               | `Maybe Patrol.Dsn`                            | Where events are sent; `Nothing` makes the client non-recording         |
| `transport`         | `Maybe TransportProvider`                     | How events are sent (see [Choosing a Transport](#choosing-a-transport)) |
| `environment`       | `Maybe Text`                                  | Environment tag (e.g. `"production"`)                                   |
| `release`           | `Maybe Text`                                  | Release identifier attached to events                                   |
| `sampleRate`        | `Float`                                       | Fraction of events to send, in `[0,1]`                                  |
| `sendDefaultPII`    | `Bool`                                        | Whether to include personally-identifiable info                         |
| `maxBreadcrumbs`    | `Word`                                        | Per-scope breadcrumb cap                                                |
| `beforeSend`        | `Maybe (CapturedEvent -> Maybe Patrol.Event)` | Final hook to rewrite or drop each event                                |
| `beforeBreadcrumb`  | `Maybe (Breadcrumb -> Maybe Breadcrumb)`      | Hook to rewrite or drop each breadcrumb                                 |
| `integrations`      | `Vector SomeIntegration`                      | Extra integrations to run                                               |
| `shutdownTimeout`   | `NominalDiffTime`                             | How long `close` waits for the transport to drain                       |
| `debug`             | `Bool`                                        | Log dropped events to `stderr`                                          |

### Capturing

| Function                       | Captures                                              |
| ------------------------------ | ----------------------------------------------------- |
| `captureMessage lvl msg`       | A plain message at a given `Level`                    |
| `captureException e`           | Any `Exception`, building an event (with stack trace) |
| `captureEvent ev`              | A fully-formed `Patrol.Event`                         |

Each has a `_`-suffixed variant that discards the returned `Maybe EventId`.

### Built-in Integrations

| Integration                            | Effect                                                       |
| -------------------------------------- | ------------------------------------------------------------ |
| `AttachCallStackIntegration`           | Attaches the call-site stack to captured events              |
| `AttachAnnotatedExceptionIntegration`  | Lifts `annotated-exception` annotations onto the event       |
| `AttachExceptionContextIntegration`    | Attaches exception context as event context                  |
| `ProcessStacktraceIntegration`         | Post-processes frames, applying in-app include/exclude rules |

### Optics modules

The optional optics layer (see [Optics](#optics-optional)) is split across:

| Module                        | Package                | Provides                                                                                             |
| ----------------------------- | ---------------------- | ---------------------------------------------------------------------------------------------------- |
| `Sentry.Optics`               | `sentry-optics`        | Drop-in for `Sentry` + `editScope` / `apply` / `runScopeUpdate` + `empty*`                           |
| `Sentry.Optics.Prelude`       | `sentry-optics`        | Unqualified batteries, same as below                                                                 |
| `Sentry.Core.Optics`          | `sentry-core-optics`   | Drop-in for `Sentry.Core` (transport-agnostic) + `editScope` / `apply` / `runScopeUpdate` + `empty*` |
| `Sentry.Core.Optics.Prelude`  | `sentry-core-optics`   | Unqualified batteries: the `optics` vocabulary, `?=` / `.=` / `%=`, field & value labels             |
| `Patrol.Optics`               | `patrol-optics`        | Orphan `#field` lenses and `#_Constructor` prisms for the `patrol` protocol types                    |

Without optics, `Sentry.Level` and `Sentry.BreadcrumbType` (in `sentry-core`)
re-export the `patrol` types for qualified import.

### Transport Responses

Transport operations return explicit sum types rather than throwing:

| Type               | Constructors                                                                                                   |
| ------------------ | -------------------------------------------------------------------------------------------------------------- |
| `SendResponse`     | `SendProcessed`, `SendFailed_QueueFull`, `SendFailed_Shutdown`                                                 |
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

[`sentry-rust`](https://github.com/getsentry/sentry-rust), which initially
inspired much of this project's architecture.

[`hs-opentelemetry`](https://github.com/iand675/hs-opentelemetry), which provides
the thread-local context machinery the scope system is built on.

[`optics`](https://github.com/well-typed/optics), which powers the optional
optics-based authoring API in `sentry-optics`, `sentry-core-optics`, and
`patrol-optics`.

[`sentry-rust`]: https://github.com/getsentry/sentry-rust
[`patrol`]: https://github.com/tfausak/patrol
