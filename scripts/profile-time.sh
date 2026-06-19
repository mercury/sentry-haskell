#!/usr/bin/env bash
#
# Wall-clock comparison of the HTTP/1.1 vs HTTP/2 transports via hyperfine.
#
# Usage:
#   scripts/profile-transports.sh <profile-bin> <sink-bin> [count] [queue] [payload]
#
# Env:
#   SINK_PORT  port to run the sink on (default 8766)
#   OUT        directory for hyperfine exports (default: profiles)
set -euo pipefail

if [[ $# -lt 2 ]]; then
  echo "usage: $0 <profile-bin> <sink-bin> [count] [queue] [payload]" >&2
  exit 2
fi

profile=$1
sink=$2
count=${3:-50000}
queue=${4:-1000}
payload=${5:-0}
port=${SINK_PORT:-8766}
out=${OUT:-profiles}

command -v hyperfine >/dev/null 2>&1 || { echo "error: hyperfine not found (enter the nix dev shell)" >&2; exit 1; }
mkdir -p "$out"

"$sink" "$port" >/dev/null 2>&1 &
sink_pid=$!
trap 'kill "$sink_pid" 2>/dev/null || true' EXIT

# Wait for the sink to accept TCP connections (boots in well under a second).
for _ in $(seq 1 50); do
  (exec 3<>"/dev/tcp/127.0.0.1/$port") 2>/dev/null && { exec 3>&- 3<&-; break; }
  sleep 0.1
done

export SENTRY_PROFILE_SINK_PORT="$port"

hyperfine \
  --warmup 2 --runs 10 \
  --export-markdown "$out/wallclock-tls.md" \
  --export-json "$out/wallclock-tls.json" \
  --command-name 'http1.1 (TLS)' "env SENTRY_PROFILE_BACKEND=h1 $profile async $count $queue $payload +RTS -N -RTS" \
  --command-name 'http2 (TLS)'   "env SENTRY_PROFILE_BACKEND=h2 $profile async $count $queue $payload +RTS -N -RTS"
