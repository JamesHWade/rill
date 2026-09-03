#!/usr/bin/env bash
set -Eeuo pipefail

if [[ "$(cat /tmp/rill-role 2>/dev/null || true)" == "poll" ]]; then
  exit 0
fi

readonly public_port="${PORT:-10000}"
readonly shiny_port="${RILL_SHINY_PORT:-3838}"

curl --fail --silent --show-error --max-time 3 \
  "http://127.0.0.1:${shiny_port}/" >/dev/null
curl --fail --silent --show-error --max-time 3 \
  "http://127.0.0.1:${public_port}/ping" >/dev/null
