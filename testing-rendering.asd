;;;; testing-rendering.asd


(asdf:defsystem #:testing-rendering
  :description "System for the graphic part of tetris"
  :author "Andrzej Bisewski <andrzej.bisewski@gmail.com>"
  :license  "you can use, just don't make money of it"
  :version "0.0.1"
  :serial t
  :depends-on (#:cepl.sdl2
               #:cepl
               #:nineveh
               #:livesupport
               #:cepl.skitter.sdl2
               #:skitter.glop
               #:harmony-simple
               #:harmony-out123
               #:bt-semaphore
               #:tetris-structures
               #:tetris
               #:utils
               )
  :components (
               (:file "sounds")
               
               (:file "player-functions")
               (:file "testing-rendering")
               ))

