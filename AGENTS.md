# AGENTS.md

## Project Overview

This is an **unofficial Sentry SDK for Haskell**, currently in early development (v0.0.0). The project provides a type-safe, extensible SDK for integrating Haskell applications with Sentry error tracking.

**Heavily inspired by [sentry-rust](https://github.com/getsentry/sentry-rust)**: This project follows the architectural patterns and design philosophy of the official Rust SDK, adapting them to Haskell's type system and idioms.

**Key characteristics:**
- Multi-package Cabal workspace with Nix flake development environment
- Plugin architecture using typeclasses and existential types
- Modern Haskell (GHC2024) with strict type safety
- Tested on GHC 9.10.3 and 9.12.2

## Repository Structure

```
sentry-haskell/
├── sentry-core/          # Core SDK abstractions (for integration authors)
├── sentry/               # High-level SDK (WIP: async executor & rate limiter done)
├── nix/                  # Nix build infrastructure
│   ├── packages/         # Custom packages (kent mock server)
│   └── overlays/         # Nix overlays
├── cabal.project         # Multi-package workspace config
├── flake.nix            # Nix development environment
└── hpack-defaults.yaml  # Shared hpack settings
```

## Package Architecture

### `sentry-core`
Foundation package providing low-level abstractions:
- **`Sentry.Client`**: Main SDK client coordinating transport and integrations
- **`Sentry.Client.Options`**: Configuration options (DSN, sample rates, filters, etc.)
- **`Sentry.Transport`**: Typeclass for event delivery backends
- **`Sentry.Integration`**: Typeclass for pluggable event processors

**Target audience**: Integration authors, library developers

**Rust SDK parallel**: Similar to `sentry-core` and `sentry-types` in sentry-rust

### `sentry` (WIP)
High-level SDK with concrete transport implementations:
- **`Sentry.Transport.Executor.Async`**: Asynchronous executor with worker threads (✅ implemented)
- **`Sentry.Transport.Executor.RateLimiter`**: Server-compliant rate limiting (✅ implemented)
- ⏳ HTTP transport implementation pending
- ⏳ Integration tests pending

**Target audience**: End users integrating Sentry into applications

**Rust SDK parallel**: Similar to the main `sentry` crate in sentry-rust

## Development Environment

### Prerequisites
- **Nix with flakes** (recommended), OR
- **GHC 9.10+** and **cabal-install** manually

### Quick Start

```bash
# With Nix (automatic via direnv)
direnv allow

# Or manually enter Nix shell
nix develop

# Build the project
just build

# Build just the core package
just build-core

# Run tests
cabal test sentry-core:unit

# Watch mode for development
just ghciwatch

# Watch mode with auto-testing
just ghciwatch-unit
```

### What's in the Dev Shell

The Nix flake provides:
- **GHC 9.10** and **cabal-install**
- **just**: Command runner for common tasks (see `justfile`)
- **ghciwatch**: Live-reloading REPL watcher for fast feedback
- **hpack**: Generate .cabal files from package.yaml
- **fourmolu**: Haskell code formatter (config in fourmolu.yaml)
- **kent-server**: Mock Sentry backend for integration testing (v2.1.0)
- **zlib**: C dependencies

## Build System

### Cabal + hpack

This project uses **hpack** for package configuration (source of truth is `package.yaml` files):

```bash
# After modifying package.yaml, regenerate .cabal files
hpack

# Then build as normal
just build
```

**Common shared settings** (`hpack-defaults.yaml`):
- Language: GHC2024
- Strict warnings (`-Weverything` with pragmatic exclusions)
- Default extensions: `BlockArguments`, `ImportQualifiedPost`, `OverloadedRecordDot`, etc.
- Debug info included (`-g`)

### Testing Infrastructure

**Unit tests** (sentry-core/tests/unit/):
- Framework: **tasty** + **tasty-hspec**
- Discovery: **tasty-discover** (automatic test discovery)
- Run: `cabal test sentry-core:unit`

**Unit tests** (sentry/tests/unit/):
- Framework: **tasty** + **tasty-hspec**
- Test files:
  - `RateLimiterTest.hs` - Rate limiter behavior (7 test cases)
    - Header parsing (`X-Sentry-Rate-Limits`, `Retry-After`)
    - HTTP 429 handling
    - Category filtering
- Run: `cabal test sentry:unit`

**Benchmarks** (sentry-core/benchmarks/):
- **Time**: `tasty-bench` → `cabal bench sentry-core:time`
- **Space**: `weigh` library → `cabal bench sentry-core:space`

**Integration testing**:
- Use `kent-server` (available in dev shell) as mock Sentry backend

## Typical Development Workflow

```bash
# 1. Make changes to source code
vim sentry-core/library/Sentry/Client.hs

# 2. If you modified package.yaml, regenerate .cabal
hpack

# 3. Auto-reload development (recommended)
just ghciwatch              # Standard watch mode
just ghciwatch-unit         # Watch with auto-testing

# 4. Or build manually
just build-core

# 5. Run tests
cabal test sentry-core:unit

# 6. Check benchmarks if performance-sensitive
cabal bench sentry-core:time
```

## Architecture Highlights

### Plugin System (Inspired by sentry-rust)

The SDK uses Haskell typeclasses for extensibility, mirroring the trait-based approach in Rust:

1. **Transport abstraction**: Implement `Transport` typeclass to add new backends
   - Rust parallel: `Transport` trait
2. **Integration system**: Implement `Integration` typeclass for event processors/enrichers
   - Rust parallel: `Integration` trait

Both use existential types (`SomeTransport`, `SomeIntegration`) for heterogeneous storage.

### Key Design Patterns

- **Type-safe configuration**: `ClientOptions` record with strongly-typed fields
- **Existential wrappers**: Enable storing different transport/integration implementations together (similar to trait objects in Rust)
- **Plugin architecture**: Integrations act as both event sources AND processors
- **Concurrent-safe**: Uses `IORef`, `stm-containers`, `unagi-chan` for thread safety

#### Concurrency Patterns

The SDK uses several Haskell concurrency primitives for thread-safe operation:

- **Bounded channels** (`unagi-chan`): Lock-free producer-consumer queues for task distribution
- **TVar** (`stm`): Software transactional memory for shutdown coordination
- **MVar**: Point-to-point synchronization for flush operations
- **Async**: Lightweight thread management for dedicated workers
- **Pure threading**: Rate limiter uses immutable values threaded through loops (no shared mutable state)

This approach minimizes contention while maintaining type safety and preventing race conditions.

### Async Transport Executor

**File**: `sentry/library/Sentry/Transport/Executor/Async.hs`

Asynchronous envelope delivery using dedicated worker threads (mirrors sentry-rust's executor pattern):

**Architecture:**
- **Bounded task queue** (default 30 items, configurable) using `unagi-chan`
- **Dedicated worker thread** processes tasks asynchronously via `async`
- **Rate limiter integration** filters envelopes before sending
- **Non-blocking semantics**: Queue-full events are dropped (prevents blocking callers)

**Task types:**
- `SendEnvelope` - Deliver event envelope
- `Flush` - Synchronous flush point with MVar coordination
- `Shutdown` - Graceful termination with timeout

**Error handling:**
- Explicit response types: `SendResponse`, `FlushResponse`, `ShutdownResponse`
- No exceptions thrown to callers
- Timeout support via UnliftIO

### Transport Rate Limiter

**File**: `sentry/library/Sentry/Transport/Executor/RateLimiter.hs`

Server-side rate limit enforcement per Sentry protocol (compliant with official spec):

**Supported headers:**
- `X-Sentry-Rate-Limits` - Per-category rate limits
- `Retry-After` - Global rate limit (numeric seconds or HTTP-date)
- HTTP 429 - Defaults to 60-second rate limit

**Rate limit categories:**
- `Error`, `Session`, `Transaction`, `Attachment`, `LogItem`, `Any` (global)

**Key operations:**
- `updateFrom429` - Handle HTTP 429 responses
- `updateFromRetryAfter` - Parse `Retry-After` headers
- `updateFromSentryHeader` - Parse `X-Sentry-Rate-Limits`
- `isEnabled` - Check if category can send now
- `filterEnvelope` - Remove rate-limited items from payload

**Threading model:**
- Pure functional updates (immutable `RateLimiter` value)
- Threaded through worker loop (no shared mutable state)
- Per-category UTC timestamp tracking

## Important Files to Understand

| File | Purpose |
|------|---------|
| `sentry-core/library/Sentry/Client.hs` | Main client type, orchestrates SDK |
| `sentry-core/library/Sentry/Integration.hs` | Plugin interface for event processing |
| `sentry-core/library/Sentry/Transport.hs` | Abstraction for event delivery |
| `sentry-core/library/Sentry/Client/Options.hs` | Configuration options |
| `sentry/library/Sentry/Transport/Executor/Async.hs` | Async executor with worker threads |
| `sentry/library/Sentry/Transport/Executor/RateLimiter.hs` | Rate limiting implementation |
| `cabal.project` | Workspace package list |
| `flake.nix` | Development environment definition |
| `justfile` | Common build/test commands |
| `fourmolu.yaml` | Code formatter configuration |

## Code Quality Standards

- **Warnings**: Comprehensive `-Weverything` with pragmatic exclusions
- **Language**: GHC2024 with modern extensions
- **Documentation**: Inline Haddock comments (use `-- |` for exports)
- **Type safety**: Strict data by default, leveraging type system for correctness
- **Formatting**: Fourmolu with project-specific configuration
  - Style: Trailing commas, 2-space indentation, single-line Haddock
  - Integration: Treefmt-nix in dev shell (`nix fmt`)
  - Config: `fourmolu.yaml`

## Dependencies Overview

**Core runtime** (sentry-core):
- `patrol`: Sentry protocol types (external package)
- `witch`: Type conversions
- `stm-containers`, `unagi-chan`: Concurrent data structures
- `text`, `time`, `vector`: Standard utilities

**sentry runtime** (extends sentry-core):
- `async`, `stm`, `unagi-chan`, `unliftio`: Concurrency primitives
- `http-client`: HTTP transport client
- `aeson`, `bytestring`: JSON parsing and binary data
- `extra`: Utility functions

**Development only**:
- `tasty`, `tasty-hspec`, `tasty-discover`: Testing
- `tasty-bench`, `weigh`: Benchmarking
- `ghciwatch`: Fast feedback loop

## Current Project Status

**Stage**: Active development (v0.0.0)
- ✅ Core abstractions defined and documented
- ✅ Test/benchmark infrastructure configured
- ✅ Plugin system (Transport, Integration) in place
- ✅ Async transport executor with rate limiting
- ⏳ HTTP transport implementation
- ⏳ Integration tests for sentry package
- ⏳ Example integrations (breadcrumbs, contexts)

## Version Control

This project uses **Git** as primary VCS, with experimental **Jujutsu** (`.jj/`) for evaluation.

## Relationship to sentry-rust

This project adapts sentry-rust's proven architecture to Haskell:

| sentry-rust | sentry-haskell | Adaptation |
|-------------|----------------|------------|
| `Transport` trait | `Transport` typeclass | Existential types instead of trait objects |
| `Integration` trait | `Integration` typeclass | Type.Reflection for dynamic retrieval |
| `ClientOptions` struct | `ClientOptions` record | Haskell records with accessor dot syntax |
| `Hub` (thread-local) | `Client` (explicit) | More explicit, less implicit global state |
| Cargo workspace | Cabal multi-package | Similar multi-package structure |
| `sentry-core` + `sentry` crates | `sentry-core` + `sentry` packages | Same separation of concerns |

## Tips for AI Assistants

1. **Always run `hpack`** after modifying `package.yaml` files before building
2. **Use `just` commands**: Prefer `just build`, `just build-core`, and `just ghciwatch` over direct cabal commands
3. **Use `just ghciwatch`** for fast feedback during development
4. **Check `hpack-defaults.yaml`** for shared GHC options/extensions
5. **Kent server** is available for integration testing (Flask-based mock)
6. **Type-driven development**: Leverage strict types and GHC warnings
7. **Plugin pattern**: New transports/integrations should implement respective typeclasses
8. **Reference sentry-rust**: When in doubt about design decisions, check how sentry-rust solves similar problems
9. **NEVER** search `/nix/store`; reading store paths is okay, but the top-level `/nix/store` directory is very large and running `find` can take a long time

## Getting Help

- **README**: Basic project information
- **Haddock**: Build docs with `cabal haddock sentry-core`
- **Source code**: Well-documented modules in `sentry-core/library/Sentry/`
- **sentry-rust reference**: https://github.com/getsentry/sentry-rust for architectural inspiration
