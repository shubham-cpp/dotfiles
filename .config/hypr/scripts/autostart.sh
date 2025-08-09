#!/usr/bin/env bash
# source ~/.profile
# Theme settings
gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'
gsettings set org.gnome.desktop.interface font-name 'Fira Sans 11'
gsettings set org.gnome.desktop.interface monospace-font-name 'FiraCode Nerd Font Mono 11'
# gsettings set org.gnome.desktop.interface gtk-theme 'Layan-Dark-Solid'
# gsettings set org.gnome.desktop.interface cursor-theme 'Bibata-Modern-Ice'
# gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita-dark'
# gsettings set org.gnome.desktop.interface icon-theme 'Adwaita'
# gsettings set org.gnome.desktop.interface gtk-theme Juno
# gsettings set org.gnome.desktop.wm.preferences theme Juno

if test -f ~/.config/wall.png; then
  swaybg --image ~/.config/wall.png --mode stretch &
fi

if test -f /usr/lib/polkit-kde-authentication-agent-1; then
  /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
elif test -f /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1; then
  /usr/lib/polkit-kde-authentication-agent-1 &
fi

if command -v kwalletd6 >/dev/null; then
  # /usr/lib/pam_kwallet_init
  kwalletd6 &
elif command -v gnome-keyring-daemon >/dev/null; then
  gnome-keyring-daemon &
fi

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
hypridle &
systemctl --user reload-or-restart xdg-desktop-portal.service xdg-desktop-portal-hyprland.service &
# hyprshade auto &

sleep 5s
env XDG_CURRENT_DESKTOP=sway XDG_SESSION_DESKTOP=sway QT_QPA_PLATFORM=wayland flameshot &
