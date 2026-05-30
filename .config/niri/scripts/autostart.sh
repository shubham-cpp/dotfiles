#!/usr/bin/env bash
set -euo pipefail

start() {
  local pattern="$1"
  shift

  if pkill -f -- "$pattern" 2>/dev/null; then
    sleep 0.2
  fi

  "$@" &
}

start "polkit-gnome-authentication-agent-1" /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1
# start "gnome-keyring-daemon" gnome-keyring-daemon

# start "awww-daemon" awww-daemon
# (
#   sleep 0.5
#   awww img "$HOME/.config/wall.png"
# ) &

# start "waybar -c .*niri/waybar/config.jsonc" waybar \
#   -c "$HOME/.config/niri/waybar/config.jsonc" \
#   -s "$HOME/Documents/dotfiles/.config/waybar/style.css"

# start "nm-applet" nm-applet
# start "mako" mako

start "xrdb -override .*X11/Xresources" xrdb -override "$HOME/.config/X11/Xresources"
# start "swayidle -C .*niri/swayidle-config" swayidle -C "$HOME/.config/niri/swayidle-config"
# start "hypridle -c .*niri/hypridle.conf" hypridle -c "$HOME/.config/niri/hypridle.conf"

# start "wl-paste --type text --watch cliphist store" wl-paste --type text --watch cliphist store
# start "wl-paste --type image --watch cliphist store" wl-paste --type image --watch cliphist store
# start "wlsunset -l 18.5204 -L 73.8567 -t 3500" wlsunset -l 18.5204 -L 73.8567 -t 3500
# start "st -e tmux" st -e tmux

start "foot --server" foot --server
start "nfsm" nfsm
start "qs -c noctalia-shell" qs -c noctalia-shell
# (
#   sleep 2s
#   start "sway-audio-idle-inhibit" "$HOME/.local/bin/sway-audio-idle-inhibit"
# ) &
(
  sleep 2s
  start "footclient -e tmux" footclient -e tmux
) &
