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

export XCURSOR_THEME="${XCURSOR_THEME:-Future-cursors}"
export XCURSOR_SIZE="${XCURSOR_SIZE:-24}"

systemctl --user import-environment XCURSOR_THEME XCURSOR_SIZE
dbus-update-activation-environment --systemd XCURSOR_THEME XCURSOR_SIZE

[ -f "$HOME/.config/X11/Xresources" ] && xrdb -override ~/.config/X11/Xresources

# Notifications: Quickshell NotificationServer (launch.sh kills mako/dunst/swaync).
# start "mako" mako
# setsid -f /usr/lib/polkit-kde-authentication-agent-1
start "polkit-gnome-authentication-agent-1" /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1
systemctl --user start gnome-keyring-daemon.service

# Idle: stasis (Phase A). QS owns the lock; do not also run swayidle.
"$HOME/.config/quickshell/launch.sh" --daemonize
if ! pgrep -x stasis >/dev/null; then
  /usr/local/bin/stasis >/tmp/stasis.log 2>&1 &
fi

if ! pgrep -x "wlsunset"; then
  wlsunset -l 18.5204 -L 73.8567 -T 5800 -t 2700 >/dev/null 2>&1 &
fi
# if ! pgrep -x "foot"; then
#   foot --server >/tmp/foot-server.log 2>&1 &
#   sleep 0.2
# fi
# if ! pgrep -f "footclient.*tmux" >/dev/null; then
#   footclient -e tmux &
# fi

start "kitty" kitty
start "nm-applet" nm-applet
start "swaybg" swaybg --mode stretch -i ~/.config/wall.png

command -v gpu-diag >/dev/null 2>&1 && gpu-diag watch &
sleep 2s

# clipboard content manager
# Untyped wl-paste --watch never delivers image offers; text stays on qs-clipboard.
start "wl-paste.*--watch" \
  wl-paste --type text --watch "$HOME/.config/quickshell/.local/bin/qs-clipboard" watch
start "wl-paste --type image --watch cliphist store" \
  wl-paste --type image --watch cliphist store

start "nvsst daemon" prime-run ~/.local/bin/nvstt daemon
