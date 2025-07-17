#!/usr/bin/env bash
#
# cycle-orient.sh
#
# Cycles plugin:nstack:layout:orientation through
#   left → top → bottom → right → left → …
#
# Usage: cycle-orient.sh
#

# 1) Parse direction argument
direction=${1:-forward}
if [[ "$direction" != "forward" && "$direction" != "backward" ]]; then
  echo "Usage: $0 [forward|backward]"
  exit 1
fi

# 1) Fetch current orientation
current=$(hyprctl getoption plugin:nstack:layout:orientation \
    | awk '/str:/ {print $2}')

# 2) Clean it up: remove any surrounding quotes
current=${current//\"/}

# 3) Define the cycle order
orientations=(left top bottom right)

# 4) Find current in array and compute next index
next="${orientations[0]}"  # default if not found
for i in "${!orientations[@]}"; do
  if [[ "${orientations[i]}" == "$current" ]]; then
    next="${orientations[((i+1) % ${#orientations[@]})]}"
    break
  fi
done

# 5) Apply the new orientation
hyprctl keyword plugin:nstack:layout:orientation "$next"

# 6) (Optional) Print out what we did
echo "Orientation: $current → $next"
