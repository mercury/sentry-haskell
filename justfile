package := "sentry"

# Number of jobs for 'cabal-install' to run in parallel when building.
#
# Defaults to the number of logical cores on the host machine.
jobs := ""

# local caching directories
cabal_dir := "cabal"
bench_dir := cabal_dir + "/bench"
build_dir := cabal_dir + "/build"
repl_dir := cabal_dir + "/repl"
test_dir := cabal_dir + "/test"

# profiling
profiling := "false"
project_file := if "{{profiling}}" != "false" {
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
    --builddir '{{build_dir}}' \
    --ghc-options '{{ghc_opts}}'

build-core: (build "sentry-core")

test target=package:
  cabal test {{target}} \
    -j{{jobs}} \
    --builddir '{{build_dir}}' \
    --ghc-options '{{ghc_opts}}'

bench target=package:
  cabal bench {{target}} \
    -j{{jobs}} \
    --project-file {{project_file}} \
    --builddir '{{bench_dir}}' \
    --ghc-options '{{ghc_opts}}' \
    --benchmark-options '+RTS -T -p'

# Quick smoke run of the harness against an in-process TLS sink (normal,
# non-profiled build). backend: h1 | h2; mode (h1 only): sync | async.
profile-run backend="h2" mode="async" count="5000" queue="1000" payload="0":
  SENTRY_PROFILE_BACKEND={{backend}} cabal run sentry:exe:sentry-profile \
    -j{{jobs}} \
    --builddir '{{build_dir}}' \
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
    -j{{jobs}} --project-file cabal.project.profiling --builddir '{{bench_dir}}'
  scripts/profile-space.sh \
    "$(cabal list-bin sentry:exe:sentry-profile --project-file cabal.project.profiling --builddir '{{bench_dir}}')" \
    "$(cabal list-bin sentry:exe:sentry-sink --project-file cabal.project.profiling --builddir '{{bench_dir}}')" \
    {{backend}} {{count}} {{queue}} {{payload}}

# Compare HTTP/1.1 vs HTTP/2 wall-clock with hyperfine against a single
# separately-spawned TLS backend.
profile-time count="50000" queue="1000" payload="0":
  cabal build sentry:exe:sentry-profile sentry:exe:sentry-sink -j{{jobs}} --builddir '{{build_dir}}' --ghc-options '{{ghc_opts}} -O2'
  scripts/profile-time.sh \
    "$(cabal list-bin sentry:exe:sentry-profile --builddir '{{build_dir}}')" \
    "$(cabal list-bin sentry:exe:sentry-sink --builddir '{{build_dir}}')" \
    {{count}} {{queue}} {{payload}}

ghciwatch target=package:
  ghciwatch \
    --watch {{target}} \
    --command \
      "cabal repl {{target}} \
        -j{{jobs}} \
        --builddir {{repl_dir}} \
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
