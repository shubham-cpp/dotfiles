#!/usr/bin/env bash
# killall -q polybar
pgrep polybar | xargs -r kill -9
# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done
polybar main -c "$HOME"/.config/polybar/config.ini &
