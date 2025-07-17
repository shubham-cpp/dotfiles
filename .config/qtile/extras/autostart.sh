#!/bin/env bash

run() {
  if ! pgrep -f "$1"; then
    if [ "$XDG_SESSION_TYPE" = "wayland" ]; then
      XDG_CURRENT_DESKTOP=Unity $@ &
    else
      $@ &
    fi
  fi
}

if [ "$XDG_SESSION_TYPE" = "wayland" ]; then
  swaybg --image ~/.config/wall.png --mode stretch &
  wl-paste --type text --watch cliphist store &
  wl-paste --type image --watch cliphist store &
fi
if [ "$XDG_SESSION_TYPE" != "wayland" ]; then
  ~/.local/bin/myscripts/default_mouse_settings
  xset r rate 300 50
  xsetroot -cursor_name left_ptr

  run greenclip daemon
  picom -b

  if [ -x /bin/xwallpaper ]; then
    run xwallpaper --stretch ~/.config/wall.png
  elif [ -x /bin/feh ]; then
    run feh --bg-fill --no-fehbg -q ~/.config/wall.png
  fi
fi

xrdb ~/.config/X11/Xresources

if [ -x /usr/lib/polkit-kde-authentication-agent-1 ]; then
  run /usr/lib/polkit-kde-authentication-agent-1
elif [ -x /usr/libexec/kf5/polkit-kde-authentication-agent-1 ]; then
  run /usr/libexec/kf5/polkit-kde-authentication-agent-1
# elif [ -x "$(which lxpolkit)" ]; then
# 	run lxpolkit
elif [ -x /usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1 ]; then
  run /usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1
elif [ -x /usr/libexec/polkit-gnome-authentication-agent-1 ]; then # polkit-gnome kde
  run /usr/libexec/polkit-gnome-authentication-agent-1
elif [ -x /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 ]; then
  run /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1
fi

/usr/lib/xfce4/notifyd/xfce4-notifyd &
# /usr/lib/pam_kwallet_init &
nm-applet &

systemctl --user import-environment PATH
systemctl --user reload-or-restart xdg-desktop-portal.service xdg-desktop-portal-gtk.service
dbus-update-activation-environment
