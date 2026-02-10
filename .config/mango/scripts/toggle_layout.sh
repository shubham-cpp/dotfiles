#!/usr/bin/env bash
# Toggle layout script for mangowc
# Usage: toggle_layout.sh <layout_name>
#
# Toggles between the specified layout and the previous layout.
# Falls back to 'tile' if no previous layout state exists.
#
# Supported layouts:
#   tile, grid, vertical_grid, scroller, vertical_scroller,
#   monocle, deck, vertical_deck, center_tile, vertical_tile

set -euo pipefail

# Layout name to mmsg code mapping
declare -A LAYOUT_CODES=(
    ["tile"]="T"
    ["grid"]="G"
    ["vertical_grid"]="VG"
    ["scroller"]="S"
    ["vertical_scroller"]="VS"
    ["monocle"]="M"
    ["deck"]="K"
    ["vertical_deck"]="VK"
    ["center_tile"]="CT"
    ["vertical_tile"]="VT"
    ["tgmix"]="TG"
)

# State file location
STATE_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/mango"
STATE_FILE="$STATE_DIR/toggle_layout_state"

# Print usage and exit
usage() {
    echo "Usage: $0 <layout_name>" >&2
    echo "Supported layouts: ${!LAYOUT_CODES[*]}" >&2
    exit 1
}

# Get layout code from name
get_layout_code() {
    local name="$1"
    local code="${LAYOUT_CODES[$name]:-}"
    if [[ -z "$code" ]]; then
        echo "Error: Unknown layout '$name'" >&2
        echo "Supported layouts: ${!LAYOUT_CODES[*]}" >&2
        return 1
    fi
    echo "$code"
}

# Get current layout code from mmsg
get_current_layout() {
    # mmsg -g -l output format: "eDP-1 T" or similar
    local output
    output=$(mmsg -g -l 2>/dev/null | head -n1)
    if [[ -z "$output" ]]; then
        echo "Error: Failed to get current layout from mmsg" >&2
        exit 1
    fi
    # Extract the layout code (last word)
    echo "${output##* }"
}

# Toggle to the specified layout
toggle_layout() {
    local target_name="$1"

    # Validate argument
    if [[ -z "$target_name" ]]; then
        usage
    fi

    # Get target layout code
    local target_code
    target_code=$(get_layout_code "$target_name") || exit 1

    # Get current layout
    local current_code
    current_code=$(get_current_layout)

    # Create state directory if needed
    mkdir -p "$STATE_DIR"

    if [[ "$current_code" == "$target_code" ]]; then
        # Currently on target layout - revert to previous
        if [[ -f "$STATE_FILE" ]]; then
            local saved_code
            saved_code=$(cat "$STATE_FILE")
            # Validate saved code is non-empty
            if [[ -n "$saved_code" ]]; then
                mmsg -l "$saved_code"
            else
                mmsg -l "T"  # Fallback to tile
            fi
        else
            mmsg -l "T"  # No state saved, fallback to tile
        fi
    else
        # Not on target layout - save current and switch
        echo "$current_code" > "$STATE_FILE"
        mmsg -l "$target_code"
    fi
}

# Main entry point
toggle_layout "$1"
