#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -eq 0 ]; then
  printf 'usage: %s <command> [args...]\n' "${0##*/}" >&2
  exit 64
fi

gamescope_args=(-f --backend sdl --force-grab-cursor)

if [ -n "${GAMESCOPE_WIDTH:-}" ] && [ -n "${GAMESCOPE_HEIGHT:-}" ]; then
  gamescope_args+=(-w "$GAMESCOPE_WIDTH" -h "$GAMESCOPE_HEIGHT" -W "$GAMESCOPE_WIDTH" -H "$GAMESCOPE_HEIGHT")
fi

if [ -n "${GAMESCOPE_REFRESH:-}" ]; then
  gamescope_args+=(-r "$GAMESCOPE_REFRESH")
fi

if [ "${GAMESCOPE_ADAPTIVE_SYNC:-0}" = "1" ]; then
  gamescope_args+=(--adaptive-sync)
fi

if [ "${GAMESCOPE_MANGOAPP:-1}" != "0" ]; then
  gamescope_args+=(--mangoapp)
fi

exec gamescope "${gamescope_args[@]}" -- "$@"
