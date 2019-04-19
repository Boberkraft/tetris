(asdf:defsystem #:server
  :description "Server and client"
  :author "Your Name <your.name@example.com>"
  :license  "you can use, just don't make money of it"
  :version "0.0.1"
  :serial t
  :depends-on (
               #:usocket
               #:bt-semaphore
               #:tetris
               #:alexandria
               #:tetris-structures
               #:testing-rendering
               )
  :components (
               (:file "player-functions")
               (:file "link")
               (:file "server")
               (:file "client")
               ))
