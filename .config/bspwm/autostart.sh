#!/bin/bash

run() {
    if ! pgrep $1 ; then
        setsid -f $@
    fi
}

~/.config/polybar/launch.sh &
# bash ~/.local/bin/myscripts/if_void &

pkill sxhkd
setsid -f sxhkd

run dunst
wmname LG3D
