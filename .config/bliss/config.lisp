;;;; config.lisp

(in-package #:bliss)

(defblock date
  :interval 60
  :icon "📅"
  :command "date '+%a, %d %b - %I:%M'")
(defblock bats
  :interval 3
  :command "dwm_bat")
(defblock volume
  :icon "🔊"
  :interval -1
  :command "pamixer --get-volume-human || echo "
  :signo :rtmin+2)
(defblock brightness
  :icon "💡"
  :interval -1
  :command "brightnessctl g"
  :signo :rtmin+3)
(defblock network
  :interval 5
  :command "dwm_net")
;; Copied from https://github.com/thytom/dwmbar/blob/master/modules/weather
(defblock weather
  :interval 3600
  :command "curl -s v2.wttr.in | grep -e \"Weather\" | sed -n 2p | sed s/Weather://g | sed 's/,//g' | sed 's/+//g' | sed 's/°C.*/°C/' | sed 's/.*m//'")
(defblock mem_mb
  :icon ""
  :interval 3
  :command "free -mh --si | awk  {'print $3'} | head -n 2 | tail -1")
;; Copied from https://www.baeldung.com/linux/get-cpu-usage
(defblock cpu_per
  :icon ""
  :interval 4
  :command "echo \"$(expr 100 - $(vmstat 1 2|tail -1|awk '{print $15}'))%\"")

(setf *separator* "|")
(setf *config-reload-signal* :rtmin+1)
(setf *blocks* (list weather date mem_mb cpu_per bats volume brightness network))
