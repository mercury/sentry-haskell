package := "sentry"

# Number of jobs for 'cabal-install' to run in parallel when building.
#
# Defaults to the number of logical cores on the host machine.
jobs := ""

# local caching directories
cabal_dir := "cabal"
build_dir := cabal_dir + "/build"
repl_dir := cabal_dir + "/repl"
test_dir := cabal_dir + "/test"

# GHC compilation options, passed thru 'cabal'
ghc_opts := ""
repl_opts := "-O0 -fobject-code"

build target=package:
  cabal build {{target}} \
    -j{{jobs}} \
    --builddir '{{build_dir}}' \
    --ghc-options '{{ghc_opts}}'

build-core: (build "sentry-core")

ghciwatch target=package:
  ghciwatch \
    --watch {{package}} \
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
