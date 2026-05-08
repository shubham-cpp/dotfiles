#!/usr/bin/env bash

. "$HOME/.profile"

export XCURSOR_THEME="${XCURSOR_THEME:-Breeze_Light}"
export XCURSOR_SIZE="${XCURSOR_SIZE:-24}"

systemctl --user import-environment XCURSOR_THEME XCURSOR_SIZE
dbus-update-activation-environment --systemd XCURSOR_THEME XCURSOR_SIZE

xrdb -override ~/.config/X11/Xresources

if ! pgrep -x "mako" >/dev/null; then
  # swaync >/dev/null 2>&1 &
  mako >/dev/null 2>&1 &
  # setsid -f /usr/lib/polkit-kde-authentication-agent-1
  setsid -f /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1
  setsid -f gnome-keyring-daemon
fi

if ! pgrep -x "waybar"; then
  # setsid -f sh -c 'echo ~/.config/mango/config.jsonc | entr -n waybar -c ~/.config/mango/config.jsonc' >/tmp/waybar-watch.log
  waybar -c ~/.config/mango/config.jsonc >/tmp/waybar-watch.log 2>&1 &
fi
if ! pgrep -x "swayidle"; then
  swayidle -w -C ~/.config/mango/swayidle-config >/tmp/swayidle-watch.log 2>&1 &
  setsid -f sh -c 'echo ~/.config/mango/config.conf | entr -n mmsg -d reload_config' >/tmp/mango-config-watch.log
fi

if ! pgrep -x "wlsunset"; then
  wlsunset -l 18.5204 -L 73.8567 -t 3500 >/dev/null 2>&1 &
fi
if ! pgrep -x "foot"; then
  foot --server >/tmp/foot-server.log 2>&1 &
  sleep 0.2
fi
if ! pgrep -f "footclient.*tmux" >/dev/null; then
  footclient -e tmux &
fi
if ! pgrep -x "nm-applet"; then
  nm-applet &
fi
if ! pgrep -x "blueman-applet"; then
  blueman-applet &
fi
if ! pgrep -x "awww-daemon"; then
  awww-daemon &
  awww img ~/.config/wall.png &
  # swaybg --image ~/.config/wall.png --mode stretch &
fi

gpu-diag watch &
sleep 2s
setsid -f ~/.local/bin/sway-audio-idle-inhibit

sleep 0.2
# clipboard content manager
wl-paste --type text --watch cliphist store &
sleep 0.2
wl-paste --type image --watch cliphist store &
