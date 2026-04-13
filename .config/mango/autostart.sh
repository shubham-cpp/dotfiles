#!/usr/bin/env sh

xrdb -override ~/.config/X11/Xresources

if ! pgrep -x "swaync" >/dev/null; then
  swaync >/dev/null 2>&1 &
  setsid -f /usr/lib/polkit-kde-authentication-agent-1
  setsid -f gnome-keyring-daemon
fi

# clipboard content manager
wl-paste --type text --watch cliphist store >/dev/null 2>&1 &
wl-paste --type image --watch cliphist store >/dev/null 2>&1 &

if ! pgrep -x "waybar"; then
  setsid -f sh -c 'echo ~/.config/mango/config.jsonc | entr -n waybar -c ~/.config/mango/config.jsonc' >/tmp/waybar-watch.log
fi
# if ! pgrep -x "swaybg"; then
#   setsid -f sh -c 'echo ~/.config/wall.png | entr -n swaybg -i ~/.config/wall.png' >/tmp/swaybg-watch.log
# fi
if ! pgrep -x "swayidle"; then
  setsid -f sh -c 'echo ~/.config/niri/swayidle-config | entr -n swayidle -C ~/.config/niri/swayidle-config' >/tmp/swayidle-watch.log
  setsid -f sh -c 'echo ~/.config/mango/config.conf | entr -n mmsg -d reload_config' >/tmp/mango-config-watch.log
fi

if ! pgrep -x "wlsunset"; then
  wlsunset -l 18.5204 -L 73.8567 -t 3500 >/dev/null 2>&1 &
fi
if ! pgrep -x "vicinae"; then
  vicinae server &
fi
if ! pgrep -x "wezterm-gui"; then
  # foot tmux &
  gtk-launch kitty
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
fi

sleep 2s
setsid -f ~/.local/bin/sway-audio-idle-inhibit
