# CLAUDE.md

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
├── sentry/               # High-level "batteries included" SDK (WIP)
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

### `sentry` (Planned)
High-level SDK with default implementations and convenience functions.

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
just ghcid
```

### What's in the Dev Shell

The Nix flake provides:
- **GHC 9.10** and **cabal-install**
- **just**: Command runner for common tasks (see `justfile`)
- **ghcid**: Fast feedback loop for development
- **hpack**: Generate .cabal files from package.yaml
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
just ghcid

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

## Important Files to Understand

| File | Purpose |
|------|---------|
| `sentry-core/library/Sentry/Client.hs` | Main client type, orchestrates SDK |
| `sentry-core/library/Sentry/Integration.hs` | Plugin interface for event processing |
| `sentry-core/library/Sentry/Transport.hs` | Abstraction for event delivery |
| `sentry-core/library/Sentry/Client/Options.hs` | Configuration options |
| `cabal.project` | Workspace package list |
| `flake.nix` | Development environment definition |
| `justfile` | Common build/test commands |

## Code Quality Standards

- **Warnings**: Comprehensive `-Weverything` with pragmatic exclusions
- **Language**: GHC2024 with modern extensions
- **Documentation**: Inline Haddock comments (use `-- |` for exports)
- **Type safety**: Strict data by default, leveraging type system for correctness

## Dependencies Overview

**Core runtime**:
- `patrol`: Sentry protocol types (external package)
- `witch`: Type conversions
- `stm-containers`, `unagi-chan`: Concurrent data structures
- `text`, `time`, `vector`: Standard utilities

**Development only**:
- `tasty`, `tasty-hspec`, `tasty-discover`: Testing
- `tasty-bench`, `weigh`: Benchmarking
- `ghcid`: Fast feedback loop

## Current Project Status

**Stage**: Early scaffolding (v0.0.0)
- ✅ Core abstractions defined and documented
- ✅ Test/benchmark infrastructure configured
- ✅ Plugin system (Transport, Integration) in place
- ⏳ High-level `sentry` package implementation pending
- ⏳ Example integrations/transports pending

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
2. **Use `just` commands**: Prefer `just build`, `just build-core`, and `just ghcid` over direct cabal commands
3. **Use `just ghcid`** for fast feedback during development
4. **Check `hpack-defaults.yaml`** for shared GHC options/extensions
5. **Kent server** is available for integration testing (Flask-based mock)
6. **Type-driven development**: Leverage strict types and GHC warnings
7. **Plugin pattern**: New transports/integrations should implement respective typeclasses
8. **Reference sentry-rust**: When in doubt about design decisions, check how sentry-rust solves similar problems

## Getting Help

- **README**: Basic project information
- **Haddock**: Build docs with `cabal haddock sentry-core`
- **Source code**: Well-documented modules in `sentry-core/library/Sentry/`
- **sentry-rust reference**: https://github.com/getsentry/sentry-rust for architectural inspiration
