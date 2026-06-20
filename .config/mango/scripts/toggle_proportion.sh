#!/usr/bin/env bash

set -euo pipefail

monitor="${MANGO_MONITOR:-eDP-1}"
state_dir="${XDG_DATA_HOME:-$HOME/.local/share}/mango"
state_file="$state_dir/toggle_proportion_state"
full_width_tolerance="${MANGO_FULL_WIDTH_TOLERANCE:-32}"
fallback_proportion="${MANGO_SCROLLER_RESTORE_PROPORTION:-0.5}"

monitor_json=$(mmsg get monitor "$monitor")
layout=$(jq -r '.layout_symbol // empty' <<<"$monitor_json")

is_valid_proportion() {
  [[ "$1" =~ ^0(\.[0-9]+)?$|^1(\.0+)?$ ]]
}

if [[ "$layout" = "S" || "$layout" = "VS" ]]; then
  monitor_width=$(jq -r '.width // empty' <<<"$monitor_json")
  client_json=$(mmsg get focusing-client)
  client_width=$(jq -r '.width // empty' <<<"$client_json")

  if ! [[ "$monitor_width" =~ ^[0-9]+$ && "$client_width" =~ ^[0-9]+$ ]]; then
    echo "Could not parse widths (monitor=$monitor_width, client=$client_width)" >&2
    exit 1
  fi

  mkdir -p "$state_dir"

  if (( client_width >= monitor_width - full_width_tolerance )); then
    restore_proportion="$fallback_proportion"
    if [[ -f "$state_file" ]]; then
      saved_proportion=$(cat "$state_file")
      if is_valid_proportion "$saved_proportion"; then
        restore_proportion="$saved_proportion"
      fi
    fi

    echo "Client width is $client_width/$monitor_width - restoring proportion $restore_proportion"
    mmsg dispatch "set_proportion,$restore_proportion"
  else
    current_proportion=$(awk -v client="$client_width" -v monitor="$monitor_width" 'BEGIN { printf "%.3f", client / monitor }')
    if is_valid_proportion "$current_proportion"; then
      echo "$current_proportion" > "$state_file"
    fi

    echo "Client width is $client_width/$monitor_width - setting full proportion"
    mmsg dispatch set_proportion,1.0
  fi
else
  echo "Layout is not scroller (layout = '$layout'), toggling maximize."
  mmsg dispatch togglemaximizescreen,0
fi
