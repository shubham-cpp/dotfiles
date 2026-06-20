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

# Layout name to mmsg symbol mapping
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

declare -A LAYOUT_NAMES=(
    ["T"]="tile"
    ["G"]="grid"
    ["VG"]="vertical_grid"
    ["S"]="scroller"
    ["VS"]="vertical_scroller"
    ["M"]="monocle"
    ["K"]="deck"
    ["VK"]="vertical_deck"
    ["CT"]="center_tile"
    ["VT"]="vertical_tile"
    ["TG"]="tgmix"
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

is_known_layout() {
    local layout="$1"

    [[ -n "${LAYOUT_NAMES[$layout]:-}" || -n "${LAYOUT_CODES[$layout]:-}" ]]
}

# Get current layout code from mmsg
get_current_layout() {
    local layout
    layout=$(mmsg get all-monitors | jq -r '.monitors[0].layout_symbol // empty')
    if [[ -z "$layout" ]]; then
        echo "Error: Failed to get current layout from mmsg" >&2
        exit 1
    fi
    echo "$layout"
}

set_layout() {
    local layout="$1"
    local name="${LAYOUT_NAMES[$layout]:-$layout}"

    mmsg dispatch "setlayout,$name"
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
            if is_known_layout "$saved_code"; then
                set_layout "$saved_code"
            else
                set_layout "T"  # Fallback to tile
            fi
        else
            set_layout "T"  # No state saved, fallback to tile
        fi
    else
        # Not on target layout - save current and switch
        echo "$current_code" > "$STATE_FILE"
        set_layout "$target_code"
    fi
}

# Main entry point
toggle_layout "${1:-}"
