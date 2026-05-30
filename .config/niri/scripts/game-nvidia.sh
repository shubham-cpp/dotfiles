#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -eq 0 ]; then
  printf 'usage: %s <command> [args...]\n' "${0##*/}" >&2
  exit 64
fi

export __NV_PRIME_RENDER_OFFLOAD=1
export __GLX_VENDOR_LIBRARY_NAME=nvidia
export __VK_LAYER_NV_optimus=NVIDIA_only

if command -v gamemoderun >/dev/null 2>&1; then
  exec gamemoderun "$@"
fi

exec "$@"
