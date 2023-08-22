#!/usr/bin/env bash

# based on github.com/gokcehan/lf/wiki/Previews
# thanks to dylanaraps for bash bible

# a margin in pixels needed to prevent image from overlapping top title
SAFE_MARGIN=56
# a threshhold for upscaling images, like pixelart or so
NOFILTER_THRESH=256

# dylan dirname fn
dirname() {
  local tmp=${1:-.}

  [[ $tmp != *[!/]* ]] && {
     printf '/\n'
     return
  }

  tmp=${tmp%%"${tmp##*[!/]}"}

  [[ $tmp != */* ]] && {
     printf '.\n'
     return
  }

  tmp=${tmp%/*}
  tmp=${tmp%%"${tmp##*[!/]}"}

  printf '%s\n' "${tmp:-/}"
}

clear_screen() {
  printf '\e[%sH\e[9999C\e[1J%b\e[1;%sr' \
    "$((LINES - 2))" "${TMUX:+\e[2J}" "$max_items"
}

show_smth() {
  # print three dots while processing stuff
  clear
  printf '\e['$(($COLUMNS / 2 - 1))'C'...

  # process according to mime
  mime_type=$(file -bi "$1")
  case $mime_type in
    image/*)
      IMG=${CACHE:-$1}
      ;;
    video/*)
      clear
      ffmpeg -hide_banner -y -i ${CACHE:-$1} -ss 00:00:01.000 -vframes 1 /tmp/thumb.png
      clear
      sleep 0.1
      IMG="/tmp/thumb.png"
      ;;
    audio/*)
      clear
      shopt -s checkwinsize
                          (
                           :
                             :
      )
      printf '\e['$(($COLUMNS / 2 - ${#1} / 2))'C'$1'\n\n\e[?25l'
      aplay ${CACHE:-$1} &
                        read -N 1 KEY && kill $!
      clear
      printf '\e['$(($COLUMNS / 2 - 1))'C'...
      sleep 0.1
      return 1
      ;;
    *) return 1 ;;
  esac

  w3m_paths=(/usr/{local/,}{lib,libexec,lib64,libexec64}/w3m/w3mi*)
  read -r w3m _ < <(type -p w3mimgdisplay "${w3m_paths[@]}")
  read -r LINES COLUMNS < <(stty size)

  # Get terminal window size in pixels and set it to WIDTH and HEIGHT.
  export $(xdotool getactivewindow getwindowgeometry --shell)

  # Get the image size in pixels.
  read -r img_width img_height < <("$w3m" <<< "5;$IMG")
  orwidth=$img_width
  orheight=$img_height

  # resize image to fit the one of bigger dims
  ((HEIGHT > WIDTH)) && {
    ((img_height = img_height * WIDTH / img_width))
    ((img_width = WIDTH))
  }

  ((WIDTH > HEIGHT)) && {
    ((img_width = img_width * HEIGHT / img_height))
    ((img_height = HEIGHT))
  }

  # if image is very long, avoid overdrawing
  ((img_width > WIDTH)) && {
    ((img_height = img_height * WIDTH / img_width))
    ((img_width = WIDTH))
  }

  ((img_height > HEIGHT)) && {
    ((img_width = img_width * HEIGHT / img_height))
    ((img_height = HEIGHT))
  }

  # safe margins for title showing
  ((img_height > $SAFE_MARGIN * 2 && img_width > $SAFE_MARGIN * 2)) && {
    ((img_width = img_width - $SAFE_MARGIN))
    ((img_height = img_height - $SAFE_MARGIN))
  }

  # if we deal with smoll images, like pixelart, avoid blurry upscaling
  ((orwidth < $NOFILTER_THRESH && orheight < $NOFILTER_THRESH)) && {
    convert $IMG -filter box -resize "$img_width"x"$img_height" /tmp/img.png
    IMG=/tmp/img.png
  }

  # variable needed for centering image.
  HALF_HEIGHT=$(expr $HEIGHT / 2)
  HALF_WIDTH=$(expr $WIDTH / 2)
  HALF_IMG_HEIGHT=$(expr $img_height / 2)
  HALF_IMG_WIDTH=$(expr $img_width / 2)
  X_POS=$(expr $HALF_WIDTH - $HALF_IMG_WIDTH)
  Y_POS=$(expr $HALF_HEIGHT - $HALF_IMG_HEIGHT)

  clear

  # debug positioning
  # echo $X_POS $Y_POS $img_width $img_height
  # sleep 1
  # clear

  # hide the cursor.
  printf '\e[?25l'

  # show the title
  shopt -s checkwinsize
                        (
                         :
                           :
  )
  printf '\e['$(($COLUMNS / 2 - ${#1} / 2))'C'$1

  # display the image in subthread with delay to prevent blinking
  (sleep 0.1 && printf '0;1;%s;%s;%s;%s;;;;;%s\n3;\n4\n' \
     ${X_POS:-0} \
     ${Y_POS:-0} \
     "$img_width" \
     "$img_height" \
    "$IMG" | "$w3m" &> /dev/null) &

  # wait for input
  read -N 1 KEY

  # clear the image.
  printf '6;%s;%s;%s;%s\n3;' \
     "${X_POS:-0}" \
     "${Y_POS:-0}" \
     "$WIDTH" \
     "$HEIGHT" | "$w3m" &> /dev/null

  clear_screen
}

# get the arg basedir (or dir as arg) contents
DIR_PATH=$(dirname $1)
[ -d $1 ] && DIR_PATH=$1
FILES_RAW=($DIR_PATH/*)
FILES=()
# filter it to only supported mimes
for i in "${FILES_RAW[@]}"; do
  mime_type=$(file -bi "$i")
  case $mime_type in
    image/* | video/* | audio/*)
      FILES+=($i)
      ;;
  esac
done
COUNT=${#FILES[@]}
for i in "${!FILES[@]}"; do
  if [[ "${FILES[$i]}" = "${1}" ]]; then
    POS="${i}"
  fi
done
[ -z $POS ] && POS=0
show_smth ${FILES[$POS]}

# cycle through contents array
while true; do
  case $KEY in
    "h" | "j")
      POS=$((POS - 1))
      [ $POS -lt 0 ] && POS=$((COUNT - 1))
      show_smth ${FILES[$POS]}
      ;;
    "k" | "l")
      POS=$((POS + 1))
      [ $POS -eq $COUNT ] && POS=0
      show_smth ${FILES[$POS]}
      ;;
    *)
      exit 0
      ;;
  esac
done
