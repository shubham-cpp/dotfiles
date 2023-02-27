#!/bin/bash

run() {
	if ! pgrep $1; then
		setsid -f $@
	fi
}
pgrep polybar | xargs -r kill -9
# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done
polybar main -c "$HOME"/.config/polybar/config.ini &
# ~/.config/polybar/launch.sh &
# bash ~/.local/bin/myscripts/if_void &

pkill sxhkd
setsid -f sxhkd

run dunst
wmname LG3D
