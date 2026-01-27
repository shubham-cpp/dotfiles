#!/usr/bin/env bash
gnome-keyring-daemon &
/usr/lib/xdg-desktop-portal-gtk &
/usr/lib/xdg-desktop-portal-gnome &
/usr/lib/polkit-kde-authentication-agent-1 &
sleep 5s
waybar -c ~/.config/niri/waybar/config.jsonc &
swaync &
xrdb -override ~/.config/X11/Xresources

nm-applet &
swaybg --image ~/Pictures/ml4w-wallpapers/mountain_view.jpg --mode stretch &
swayidle -C ~/.config/niri/swayidle-config &
wl-paste --type text --watch cliphist store &
wl-paste --type image --watch cliphist store &

alacritty -e tmux &
if ! pgrep vicinae >/dev/null; then
  vicinae server &
fi

xwayland-satellite &

wlsunset -l 18.5204 -L 73.8567 -t 3500 &
