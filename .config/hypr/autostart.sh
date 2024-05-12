#!/bin/sh

sleep 1
killall -e xdg-desktop-portal-hyprland
killall -e xdg-desktop-portal-wlr
killall xdg-desktop-portal
killall waybar
killall swaync
killall kmonad
xrdb -override ~/.config/X11/Xresources
/usr/lib/xdg-desktop-portal-hyprland &
sleep 2
/usr/lib/xdg-desktop-portal &
waybar &
swaync &
swaybg --image ~/.config/wall.png --mode stretch &
setsid -f /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1
setsid -f kmonad ~/.config/kmonad/mkmonad.kbd
