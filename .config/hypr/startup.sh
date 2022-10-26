#!/usr/bin/env sh


setsid -f waybar
setsid -f swaybg -m stretch -i ~/.config/wall.png
setsid -f foot -e tmux
setsid -f wl-paste --watch cliphist store
