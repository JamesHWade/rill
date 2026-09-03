#!/usr/bin/env bash
set -Eeuo pipefail

readonly role="${1:-web}"
if (( $# > 0 )); then
  shift
fi

validate_port() {
  local name="$1"
  local value="$2"

  if [[ ! "$value" =~ ^[1-9][0-9]{0,4}$ ]] || (( value > 65535 )); then
    echo "${name} must be an integer from 1 to 65535." >&2
    exit 64
  fi
}

require_environment() {
  local name
  local -a missing=()

  for name in \
    OAUTH2_PROXY_CLIENT_ID \
    OAUTH2_PROXY_CLIENT_SECRET \
    OAUTH2_PROXY_COOKIE_SECRET \
    OAUTH2_PROXY_OIDC_ISSUER_URL \
    OAUTH2_PROXY_REDIRECT_URL; do
    if [[ -z "${!name:-}" ]]; then
      missing+=("$name")
    fi
  done

  if (( ${#missing[@]} > 0 )); then
    printf 'Missing required web environment variable: %s\n' \
      "${missing[@]}" >&2
    exit 78
  fi
}

run_web() {
  local public_port="${PORT:-10000}"
  local shiny_port="${RILL_SHINY_PORT:-3838}"
  local -a capture_route_args=()
  local shiny_pid
  local proxy_pid=""
  local status

  validate_port PORT "$public_port"
  validate_port RILL_SHINY_PORT "$shiny_port"
  if [[ "$public_port" == "$shiny_port" ]]; then
    echo "PORT and RILL_SHINY_PORT must be different." >&2
    exit 78
  fi
  require_environment

  if [[ -n "${RILL_CAPTURE_TOKEN:-}" ]]; then
    capture_route_args+=(
      '--skip-auth-route=OPTIONS=^/api/v1/captures$'
      '--skip-auth-route=POST=^/api/v1/captures$'
    )
  fi

  printf 'web\n' > /tmp/rill-role

  terminate_children() {
    local -a pids=()

    trap - INT TERM
    if [[ -n "$proxy_pid" ]]; then
      pids+=("$proxy_pid")
    fi
    if [[ -n "${shiny_pid:-}" ]]; then
      pids+=("$shiny_pid")
    fi
    if (( ${#pids[@]} > 0 )); then
      kill -TERM "${pids[@]}" 2>/dev/null || true
      wait "${pids[@]}" 2>/dev/null || true
    fi
  }

  trap 'terminate_children; exit 130' INT
  trap 'terminate_children; exit 143' TERM

  RILL_SHINY_PORT="$shiny_port" \
    Rscript --vanilla /opt/rill/docker/run-web.R &
  shiny_pid=$!

  if ! wait_for_shiny "$shiny_port" "$shiny_pid"; then
    terminate_children
    return 1
  fi

  unset OAUTH2_PROXY_HTTP_ADDRESS OAUTH2_PROXY_UPSTREAMS
  oauth2-proxy \
    --provider=oidc \
    --email-domain='*' \
    --http-address="0.0.0.0:${public_port}" \
    --upstream="http://127.0.0.1:${shiny_port}" \
    --reverse-proxy=true \
    --proxy-websockets=true \
    --set-xauthrequest=true \
    --ping-path=/ping \
    --ready-path=/ready \
    --silence-ping-logging=true \
    "${capture_route_args[@]}" \
    "$@" &
  proxy_pid=$!

  set +e
  wait -n "$shiny_pid" "$proxy_pid"
  status=$?
  set -e
  terminate_children

  if (( status == 0 )); then
    echo "The web process stopped unexpectedly." >&2
    status=1
  fi
  return "$status"
}

wait_for_shiny() {
  local port="$1"
  local pid="$2"
  local attempt

  for (( attempt = 1; attempt <= 120; attempt++ )); do
    if curl --fail --silent --max-time 1 \
      "http://127.0.0.1:${port}/" >/dev/null; then
      return 0
    fi
    if ! kill -0 "$pid" 2>/dev/null; then
      echo "Shiny stopped before becoming ready." >&2
      return 1
    fi
    sleep 1
  done

  echo "Shiny did not become ready within 120 seconds." >&2
  return 1
}

run_poll() {
  if (( $# > 0 )); then
    echo "The poll command does not accept arguments." >&2
    exit 64
  fi
  printf 'poll\n' > /tmp/rill-role
  exec Rscript --vanilla -e 'rill::poll_feeds()'
}

case "$role" in
  web)
    run_web "$@"
    ;;
  poll)
    run_poll "$@"
    ;;
  *)
    echo "Unknown role '$role'. Expected 'web' or 'poll'." >&2
    exit 64
    ;;
esac
