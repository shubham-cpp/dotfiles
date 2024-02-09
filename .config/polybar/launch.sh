#!/usr/bin/env bash
# killall -q polybar
pgrep polybar | xargs -r kill -9
# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done
BARNAME=main
if [ $DESKTOP_SESSION != "bspwm" ]; then
  BARNAME=xmonad
fi
for m in $(polybar --list-monitors | cut -d":" -f1); do
	MONITOR="$m" polybar "$BARNAME" -c "$HOME"/.config/polybar/config.ini &
done
