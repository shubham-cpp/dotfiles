;;;; config.lisp

(in-package #:bliss)

(defblock date
  :icon "📅" :command "date '+%a, %d %b - %H:%M'" :interval 5)
(defblock temp
  :icon "🌡️" :command "acpi -t | awk '{print $4$6}'")
(defblock battery
  :icon "🔋" :command "acpi | awk '{gsub(\",\", \"\"); print $4}'")
(defblock volume
  :icon "🔊"
  :interval -1
  :command "amixer get Master | awk '/Mono:/ {print $4}' | tr -d '[]'"
  :signo :rtmin+2)

(setf *separator* "|")
(setf *config-reload-signal* :rtmin+1)
(setf *blocks* (list date temp volume battery))
