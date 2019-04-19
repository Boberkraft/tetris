
(asdf:defsystem #:utils
  :description "Misc stuff"
  :author "Andrzej Bisewski <andrzej.bisewski@gmail.com>"
  :license  "you can use, just don't make money of it"
  :version "0.0.1"
  :serial t
  :depends-on (#:cepl.sdl2 ; TODO can i delete this?
               #:cepl      ; .
               #:bt-semaphore
               #:alexandria
               #:cepl.skitter.sdl2
               #:skitter.glop)
  :components ((:file "utils")))
