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

# Run the end-to-end profiling harness against a self-spawned kent (normal,
# non-profiled build — use for a quick smoke run or to read the summary line).
profile-run mode count="5000" queue="1000" payload="0":
  cabal run sentry:exe:sentry-profile \
    -j{{jobs}} \
    --builddir '{{build_dir}}' \
    --ghc-options '{{ghc_opts}}' \
    -- {{mode}} {{count}} {{queue}} {{payload}}

# Run the harness under the profiling RTS, producing cost-centre (.prof), heap
# (.hp), and eventlog artifacts, then render them. Uses cabal.project.profiling
# (-O2, -fprof-late). Note: profiling perturbs timing — use `profile-bench` for
# wall-clock numbers.
profile-prof mode count="5000" queue="1000" payload="0" backend="kent":
  mkdir -p profiles
  SENTRY_PROFILE_BACKEND={{backend}} cabal run sentry:exe:sentry-profile \
    -j{{jobs}} \
    --project-file cabal.project.profiling \
    --builddir '{{bench_dir}}' \
    -- {{mode}} {{count}} {{queue}} {{payload}} \
    +RTS -p -hc -l -poprofiles/sentry-profile -olprofiles/sentry-profile.eventlog -RTS
  profiteur profiles/sentry-profile.prof || true
  eventlog2html profiles/sentry-profile.eventlog || true
  @echo "artifacts under profiles/: sentry-profile.prof(.html) sentry-profile.eventlog(.html) sentry-profile.hp"

# Compare sync vs async wall-clock with hyperfine against a single long-lived
# kent (normal build, kent flushed between runs so timing excludes its boot).
# The kent/hyperfine orchestration lives in scripts/profile-bench.sh.
profile-bench count="5000" queue="1000" payload="0":
  cabal build sentry:exe:sentry-profile -j{{jobs}} --builddir '{{build_dir}}' --ghc-options '{{ghc_opts}}'
  scripts/profile-bench.sh \
    "$(cabal list-bin sentry:exe:sentry-profile --builddir '{{build_dir}}')" \
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
