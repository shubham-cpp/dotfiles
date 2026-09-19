#!/usr/bin/env bash
set -euo pipefail

export MALLOC_CONF="${MALLOC_CONF:-background_thread:true,dirty_decay_ms:100,muzzy_decay_ms:100}"

# One NotificationServer. Competing daemons must be gone before qs claims the bus.
pkill -x mako >/dev/null 2>&1 || true
pkill -x dunst >/dev/null 2>&1 || true
pkill -x swaync >/dev/null 2>&1 || true

dir="$(cd "$(dirname "$0")" && pwd)"
if [[ ! -x "$dir/.local/bin/lock-auth" || "$dir/scripts/lock-auth.c" -nt "$dir/.local/bin/lock-auth" ]]; then
    sh "$dir/scripts/build-lock-auth"
fi
for helper in qs-search qs-resources qs-session qs-reminder qs-lock-wait qs-clipboard qs-notification-images; do
    if [[ ! -x "$dir/.local/bin/$helper" ]]; then
        printf 'Missing Go helpers. Run: cd %s && bash scripts/build-go-helpers\n' "$dir" >&2
        exit 1
    fi
done
exec qs -n -p "$dir" "$@"
