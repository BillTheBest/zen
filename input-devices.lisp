;; todo (v2) : do not hardcode, even on my same machine it sometimes changes !
;; -> device detection (v2) : ask user which of /proc/bus/input/devices are mouse/kbd, or probe it like evdev
(defvar *input-fds*
  `((:kbd   ,(posix-open :pathname "/dev/input/event3" :flags '(o-rdonly)))
    (:mouse ,(posix-open :pathname "/dev/input/event4" :flags '(o-rdonly)))))
