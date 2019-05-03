(asdf:defsystem #:tetris-structures
  :description "Offers tools for emulating tetris that other systems can use"
  :author "Your Name <your.name@example.com>"
  :license  "you can use, just don't make money of it"
  :version "0.0.1"
  :serial t
  :depends-on (#:bt-semaphore
               #:rtg-math
               #:utils)
  :components ((:file "tetris-structures")))

