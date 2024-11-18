#!/bin/sh

sleep 1
systemctl --user stop xdg-desktop-portal-gtk xdg-desktop-portal xdg-desktop-portal-hyprland
killall waybar
killall swaync
killall kmonad
killall nm-applet
xrdb -override ~/.config/X11/Xresources
dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP
systemctl --user start xdg-desktop-portal-hyprland
sleep 2s
systemctl --user start xdg-desktop-portal
waybar &
swaync &
nm-applet &
swaybg --image ~/.config/wall.png --mode stretch &
if test -f /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1; then
  /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
fi
if test -f /usr/lib/polkit-kde-authentication-agent-1; then
  /usr/lib/polkit-kde-authentication-agent-1 &
fi
systemctl --user reload-or-restart xdg-desktop-portal.service xdg-desktop-portal-hyprland.service &
hyprshade auto &
