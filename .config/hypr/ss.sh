#!/bin/sh
hyprctl clients -j | jq -r '.[] | select(.pid > 0 and (.hidden | not)) | "\(.at[0]),\(.at[1]) \(.size[0])x\(.size[1])"' | slurp | xargs -r -I {} grim -g "{}" - | swappy -f -
