#!/usr/bin/env bash

# layout=$(mmsg -g -l | awk '$1 == "eDP-1" { print $3 }')
layout=$(mmsg -g -l | awk '{ print $3 }')

if [ "$layout" = "S" ]; then
  width=$(mmsg -g -x | awk '/^eDP-1 width/ { print $3 }')
  if ! [[ "$width" =~ ^[0-9]+$ ]]; then
    echo "Could not parse width ($width)" >&2
    exit 1
  fi

  if [ "$width" -gt 1900 ]; then
    echo "Width is $width — assuming full proportion → setting 0.5"
    mmsg -d switch_proportion_preset
  else
    echo "Width is $width — setting proportion back to 1.0"
    mmsg -d set_proportion,1.0
  fi
else
  echo "Layout is not scroller (layout = '$layout'), switching to monocle."
  mmsg -l "M"
fi
