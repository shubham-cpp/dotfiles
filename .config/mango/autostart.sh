#!/usr/bin/env sh

if ! pgrep -x "swaync" >/dev/null; then
  swaync >/dev/null 2>&1 &
fi

systemctl --user stop xdg-desktop-portal-gtk xdg-desktop-portal xdg-desktop-portal-hyprland
systemctl --user set-environment XDG_CURRENT_DESKTOP=wlroots
systemctl --user import-environment \
  DISPLAY \
  WAYLAND_DISPLAY \
  XDG_CURRENT_DESKTOP
# dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=wlroots
hash dbus-update-activation-environment 2>/dev/null &&
  dbus-update-activation-environment --systemd \
    DISPLAY \
    SWAYSOCK \
    XDG_CURRENT_DESKTOP=wlroots \
    WAYLAND_DISPLAY

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
  gtk-launch org.wezfurlong.wezterm
fi
if ! pgrep -x "nm-applet"; then
  nm-applet &
fi
if ! pgrep -x "blueman-applet"; then
  blueman-applet &
fi
if ! pgrep -x "swww-daemon"; then
  swww-daemon &
  swww img ~/.config/wall.png &
fi

systemctl --user start xdg-desktop-portal-wlr.service
sleep 2s
systemctl --user start xdg-desktop-portal
sleep 2s
systemctl --user reload-or-restart xdg-desktop-portal.service xdg-desktop-portal-wlr.service &

sleep 2s
setsid -f ~/.local/bin/sway-audio-idle-inhibit
