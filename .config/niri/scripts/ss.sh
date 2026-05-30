#!/usr/bin/env sh
# grim -g "$(slurp)" - | ~/.local/bin/swappy -f -
grim -t ppm -g "$(slurp)" - | ~/.local/bin/satty --filename - --floating-hack
