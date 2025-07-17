#!/usr/bin/env bash

arg=${1:-focusnext}
current=$(hyprctl activewindow | awk '/fullscreen:/ {print $2}')
# Clean it up: remove any surrounding quotes
current=${current//\"/}

if test "$current" != "0"; then
  case "$arg" in
  focusnext)
    hyprctl dispatch layoutmsg cyclenext loop
    ;;
  focusprev)
    hyprctl dispatch layoutmsg cycleprev loop
    ;;
  swapnext)
    hyprctl dispatch layoutmsg swapnext loop
    ;;
  swapprev)
    hyprctl dispatch layoutmsg swapprev loop
    ;;
  *)
    echo "Unknown argument = $arg. Expected one-of focusnext, focusprev,swapnext,swapprev"
    exit 1
    ;;
  esac
  exit 0
fi
case "$arg" in
focusnext)
  hyprctl dispatch movefocus d
  ;;
focusprev)
  hyprctl dispatch movefocus u
  ;;
swapnext)
  hyprctl dispatch swapwindow d
  ;;
swapprev)
  hyprctl dispatch swapwindow u
  ;;
*)
  echo "Unknown argument = $arg. Expected one-of focusnext, focusprev,swapnext,swapprev"
  exit 1
  ;;
esac
exit 0
