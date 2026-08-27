package := "sentry"

# Format cabal, Haskell, and Nix sources in one shot.
format:
  nix fmt

# Validate formatting of cabal, Haskell, and Nix sources.
check-format:
  nix fmt -- --fail-on-change

# Number of jobs for 'cabal-install' to run in parallel when building.
#
# Defaults to the number of logical cores on the host machine.
jobs := ""

# profiling
profiling := "false"
project_file := if profiling != "false" {
  "cabal.project.profiling"
} else {
  "cabal.project"
}

# GHC compilation options, passed thru to 'cabal'
ghc_opts := ""
repl_opts := "-O0 -fobject-code"

build target=package:
  cabal build {{target}} \
    -j{{jobs}} \
    --ghc-options '{{ghc_opts}}'

build-core: (build "sentry-core")

# Fetch the Hackage index, if it's absent.
_ensure-index:
  test -f "$(cabal path --remote-repo-cache)/hackage.haskell.org/01-index.tar" || cabal update

# Build all Haskell targets.
build-all: _ensure-index
  cabal build all --enable-benchmarks \
    -j{{jobs}} \
    --project-file {{project_file}} \
    --ghc-options '{{ghc_opts}}'

# Build all Haskell dependencies.
deps-all: _ensure-index
  cabal build all --enable-benchmarks --only-dependencies \
    -j{{jobs}} \
    --project-file {{project_file}}

# Test a given Haskell target.
test target=package:
  cabal test {{target}} \
    -j{{jobs}} \
    --ghc-options '{{ghc_opts}}'

# Run all Haskell test suites.
test-all: _ensure-index
  cabal test all \
    -j{{jobs}} \
    --project-file {{project_file}} \
    --ghc-options '{{ghc_opts}}'

# Run all CI steps, in the order that CI runs them.
ci: check-format build-all test-all

bench target=package:
  cabal bench {{target}} \
    -j{{jobs}} \
    --project-file {{project_file}} \
    --ghc-options '{{ghc_opts}}' \
    --benchmark-options '+RTS -T'

# Quick smoke run of the harness against an in-process TLS sink (normal,
# non-profiled build). backend: h1 | h2; mode (h1 only): sync | async.
profile-run backend="h2" mode="async" count="5000" queue="1000" payload="0":
  SENTRY_PROFILE_BACKEND={{backend}} cabal run sentry:exe:sentry-profile \
    -j{{jobs}} \
    --ghc-options '{{ghc_opts}}' \
    -- {{mode}} {{count}} {{queue}} {{payload}}

# Run one transport leg under the profiling RTS against a separately-spawned TLS
# backend (so the sink stays out of the client profile)
# producing cost-centre
# 
# backend: h1 | h2.
# 
# Note: profiling perturbs timing, use `profile-time` for wall-clock numbers.
profile-space backend="h2" count="50000" queue="1000" payload="0":
  cabal build sentry:exe:sentry-profile sentry:exe:sentry-sink \
    -j{{jobs}} \
    --project-file cabal.project.profiling
  scripts/profile-space.sh \
    "$(cabal list-bin sentry:exe:sentry-profile --project-file cabal.project.profiling)" \
    "$(cabal list-bin sentry:exe:sentry-sink --project-file cabal.project.profiling)" \
    {{backend}} {{count}} {{queue}} {{payload}}

# Compare HTTP/1.1 vs HTTP/2 wall-clock with hyperfine against a single
# separately-spawned TLS backend.
profile-time count="50000" queue="1000" payload="0":
  cabal build sentry:exe:sentry-profile sentry:exe:sentry-sink -j{{jobs}} --ghc-options '{{ghc_opts}} -O2'
  scripts/profile-time.sh \
    "$(cabal list-bin sentry:exe:sentry-profile)" \
    "$(cabal list-bin sentry:exe:sentry-sink)" \
    {{count}} {{queue}} {{payload}}

ghciwatch target=package:
  ghciwatch \
    --watch {{target}} \
    --command \
      "cabal repl {{target}} \
        -j{{jobs}} \
        --ghc-options '{{ghc_opts}}' \
        --repl-options {{repl_opts}}"
 
ghciwatch-unit target=package:
  ghciwatch \
    --watch {{target}} \
    --test-ghci 'Main.main' \
     --command \
      "cabal repl {{target}}:test:unit \
        -j{{jobs}} \
        --ghc-options '{{ghc_opts}}' \
        --repl-options {{repl_opts}}"
