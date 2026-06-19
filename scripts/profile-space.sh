#!/usr/bin/env bash
#
# Capture a cost-centre (.prof), heap (.hp) and eventlog profile for ONE
# transport leg under the profiling RTS, against a separately-spawned TLS sink
# so the sink's CPU/allocation never lands in the client profile. Renders the
# .prof and .eventlog to HTML if profiteur / eventlog2html are available.
#
# Usage:
#   scripts/profile-prof.sh <profile-bin> <sink-bin> <h1|h2> [count] [queue] [payload]
#
# Env:
#   SINK_PORT  port to run the sink on (default 8767)
#   OUT        output directory (default: profiles); artifacts are <OUT>/<leg>.*
set -euo pipefail

if [[ $# -lt 3 ]]; then
  echo "usage: $0 <profile-bin> <sink-bin> <h1|h2> [count] [queue] [payload]" >&2
  exit 2
fi

profile=$1
sink=$2
backend=$3
count=${4:-50000}
queue=${5:-1000}
payload=${6:-0}
port=${SINK_PORT:-8767}
out=${OUT:-profiles}
stem="$out/$backend"

case "$backend" in
  h1 | h2) ;;
  *) echo "error: backend must be h1 or h2" >&2; exit 2 ;;
esac

mkdir -p "$out"

"$sink" "$port" >/dev/null 2>&1 &
sink_pid=$!
trap 'kill "$sink_pid" 2>/dev/null || true' EXIT

for _ in $(seq 1 50); do
  (exec 3<>"/dev/tcp/127.0.0.1/$port") 2>/dev/null && { exec 3>&- 3<&-; break; }
  sleep 0.1
done

SENTRY_PROFILE_SINK_PORT="$port" SENTRY_PROFILE_BACKEND="$backend" \
  "$profile" async "$count" "$queue" "$payload" \
  +RTS -N -p -hc -l "-po${stem}" "-ol${stem}.eventlog" -RTS

command -v profiteur >/dev/null 2>&1 && profiteur "${stem}.prof" >/dev/null 2>&1 || true
command -v eventlog2html >/dev/null 2>&1 && eventlog2html "${stem}.eventlog" >/dev/null 2>&1 || true

echo "wrote ${stem}.prof(.html) ${stem}.hp ${stem}.eventlog(.html)"
