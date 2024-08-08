#!/bin/sh

sleep 1
systemctl --user stop xdg-desktop-portal-gtk xdg-desktop-portal xdg-desktop-portal-hyprland 
killall waybar
killall swaync
killall kmonad
killall nm-applet
xrdb -override ~/.config/X11/Xresources
# dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP
systemctl --user start xdg-desktop-portal-hyprland
sleep 2
systemctl --user start xdg-desktop-portal
waybar &
swaync &
nm-applet &
swaybg --image ~/.config/wall.png --mode stretch &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
# kmonad ~/.config/kmonad/mkmonad.kbd &
systemctl --user reload-or-restart xdg-desktop-portal.service  xdg-desktop-portal-hyprland.service &
