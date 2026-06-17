#!/usr/bin/env bash
#
# Wall-clock comparison of the sync vs async HTTP transports via hyperfine.
#
# Spawns a single long-lived kent, points the sentry-profile harness at it
# through SENTRY_PROFILE_KENT_PORT (so kent boot/teardown stays out of the timed
# region), and flushes kent between runs. Normally invoked via `just
# profile-bench`, which resolves and builds the harness binary first.
#
# Usage:
#   scripts/profile-bench.sh <sentry-profile-binary> [count] [queue] [payload]
#
# Env:
#   KENT_PORT  port to run kent on (default 8765)
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "usage: $0 <sentry-profile-binary> [count] [queue] [payload]" >&2
  exit 2
fi

bin=$1
count=${2:-5000}
queue=${3:-1000}
payload=${4:-0}
port=${KENT_PORT:-8765}

for tool in hyperfine kent-server curl; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "error: '$tool' not found on PATH (enter the nix dev shell)" >&2
    exit 1
  fi
done

# --with-threads: the single-threaded Flask dev server otherwise intermittently
# drops a connection while busy, which would abort hyperfine mid-run.
kent-server run --with-threads --host 127.0.0.1 --port "$port" >/dev/null 2>&1 &
kent_pid=$!
trap 'kill "$kent_pid" 2>/dev/null || true' EXIT

# Wait for kent to accept connections (it boots in well under a second).
for _ in $(seq 1 50); do
  curl -fsS "http://127.0.0.1:$port/api/eventlist/" >/dev/null 2>&1 && break
  sleep 0.1
done

export SENTRY_PROFILE_KENT_PORT="$port"

# The flush just resets kent's (bounded) store between runs; it isn't part of
# the measurement, so retry transient hiccups and never let it abort the bench.
flush="curl -fsS --retry 5 --retry-connrefused -X POST http://127.0.0.1:$port/api/flush/ >/dev/null 2>&1 || true"

hyperfine \
  --warmup 3 \
  --prepare "$flush" \
  --command-name sync "$bin sync $count $queue $payload" \
  --command-name async "$bin async $count $queue $payload"
