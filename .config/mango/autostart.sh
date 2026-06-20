#!/usr/bin/env bash

set -xeuo pipefail

start() {
  local pattern="$1"
  shift

  if pkill -f -- "$pattern" 2>/dev/null; then
    sleep 0.2
  fi

  "$@" &
}

# [ -f "$HOME/.profile" ] && . "$HOME/.profile"

export XCURSOR_THEME="${XCURSOR_THEME:-Breeze_Light}"
export XCURSOR_SIZE="${XCURSOR_SIZE:-24}"

systemctl --user import-environment XCURSOR_THEME XCURSOR_SIZE
dbus-update-activation-environment --systemd XCURSOR_THEME XCURSOR_SIZE

[ -f "$HOME/.config/X11/Xresources" ] && xrdb -override ~/.config/X11/Xresources

# swaync >/dev/null 2>&1 &
start "mako" mako
# setsid -f /usr/lib/polkit-kde-authentication-agent-1
start "polkit-gnome-authentication-agent-1" /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1
systemctl --user start gnome-keyring-daemon.service

if ! pgrep -x "waybar"; then
  waybar -c ~/.config/mango/config.jsonc >/tmp/waybar-watch.log 2>&1 &
fi
if ! pgrep -x "swayidle"; then
  swayidle -w -C ~/.config/mango/swayidle-config >/tmp/swayidle-watch.log 2>&1 &
  setsid -f sh -c 'echo ~/.config/mango/config.conf | entr -n mmsg dispatch reload_config' >/tmp/mango-config-watch.log
fi

if ! pgrep -x "wlsunset"; then
  wlsunset -l 18.5204 -L 73.8567 -T 5800 -t 2700 >/dev/null 2>&1 &
fi
if ! pgrep -x "foot"; then
  foot --server >/tmp/foot-server.log 2>&1 &
  sleep 0.2
fi
if ! pgrep -f "footclient.*tmux" >/dev/null; then
  footclient -e tmux &
fi

start "nm-applet" nm-applet
start "awww-daemon" awww-daemon
(
  sleep 0.5
  awww img "$HOME/.config/wall.png"
) &
command -v gpu-diag >/dev/null 2>&1 && gpu-diag watch &
sleep 2s

[ -x "$HOME/.local/bin/sway-audio-idle-inhibit" ] && start "sway-audio-idle-inhibit" "$HOME"/.local/bin/sway-audio-idle-inhibit

sleep 0.2
# clipboard content manager
wl-paste --type text --watch cliphist store &
sleep 0.2
wl-paste --type image --watch cliphist store &

start "nvsst serve" ~/.local/bin/nvstt serve
