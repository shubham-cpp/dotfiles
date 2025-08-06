#!/bin/bash

run() {
	if ! pgrep $1; then
		setsid -f $@
	fi
}

xrdb ~/.config/X11/Xresources
~/.local/bin/myscripts/default_mouse_settings
xset r rate 300 50
xsetroot -cursor_name left_ptr

picom -b
if [ -x /bin/xwallpaper ]; then
  run xwallpaper --stretch ~/.config/wall.png
elif [ -x /bin/feh ]; then
  run feh --bg-fill --no-fehbg -q ~/.config/wall.png
fi

pgrep polybar | xargs -r kill -9
# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done
polybar main -c "$HOME"/.config/polybar/config.ini &
# ~/.config/polybar/launch.sh &
# bash ~/.local/bin/myscripts/if_void &

pkill sxhkd
setsid -f sxhkd

# run dunst
run greenclip daemon
run /usr/lib/xfce4/notifyd/xfce4-notifyd
run /usr/lib/pam_kwallet_init
run kwalletd6
run nm-applet
run /usr/lib/polkit-kde-authentication-agent-1

wmname LG3D
