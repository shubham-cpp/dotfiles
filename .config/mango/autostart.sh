#!/usr/bin/env sh

if ! pgrep -x "swaync" >/dev/null; then
  swaync >/dev/null 2>&1 &
fi
if ! pgrep -x "gnome-keyring-daemon"; then
  gnome-keyring-daemon >/dev/null 2>&1 &
  dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=wlroots
  # The next line of command is not necessary. It is only to avoid some situations where it cannot start automatically
  /usr/lib/xdg-desktop-portal-wlr >/dev/null 2>&1 &
fi
# clipboard content manager
wl-paste --type text --watch cliphist store >/dev/null 2>&1 &
wl-paste --type image --watch cliphist store >/dev/null 2>&1 &

if ! pgrep -x "waybar"; then
  setsid -f sh -c 'echo ~/.config/mango/config.jsonc | entr -n waybar -c ~/.config/mango/config.jsonc' >/tmp/waybar-watch.log
fi
if ! pgrep -x "swaybg"; then
  setsid -f sh -c 'echo ~/.config/wall.png | entr -n swaybg -i ~/.config/wall.png' >/tmp/swaybg-watch.log
fi
if ! pgrep -x "swayidle"; then
  setsid -f sh -c 'echo ~/.config/niri/swayidle-config | entr -n swayidle -C ~/.config/niri/swayidle-config' >/tmp/swayidle-watch.log
fi

if ! pgrep -x "wlsunset"; then
  wlsunset -l 18.5204 -L 73.8567 -t 3500 >/dev/null 2>&1 &
fi
if ! pgrep -x "vicinae"; then
  vicinae server &
fi
if ! pgrep -x "foot"; then
  foot tmux &
fi
if ! pgrep -x "nm-applet"; then
  nm-applet &
fi
if ! pgrep -x "blueman-applet"; then
  blueman-applet &
fi

if ! pgrep -x "swayidle"; then
  setsid -f sh -c 'echo ~/.config/mango/config.conf | entr -n mmsg -d reload_config' >/tmp/mango-config-watch.log
fi

setsid -f $HOME/.local/bin/sway-audio-idle-inhibit
