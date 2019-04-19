;; This file is reposible for providing functionality
;; for connecting to a tetris server (defined in server.lisp).
;; It accepts commands send from server, and then executes then on
;; local game of tetris. It might be a console tetris (tetris.lisp)
;; or this colored graphical tetris (testing-rendering.lisp)

(defpackage #:client
  (:use #:cl
        
        )
  (:export :start
           :stop)
  )


(in-package :client)

(defun start ()
  (link:start-client 'start-message))

(defun start-dummy-client ()
  (link:start-client (lambda ()
                       (loop (sleep 0.1)))))

(defun stop ()
  (link:stop-client))


(defun send (message)
  (link:send-data-to-server message))


(defun start-message ()
  (send "left~%")
  (send "left~%")
  (send "left~%")
  (send "right~%")
  (send "drop-down~%")
  )

(defun read-loop (message)
  ;; NOTE: if this game-state is initalized, dont initlaize it again.
  (with-input-from-string (message message)
    (let ((from-who (read message)) ; first
          (command (read message)))    ; second
      (format t "~%  - [Client]: ~s from ~a " command from-who)
      (accept-command from-who command))))

(defmethod accept-command ((player-id string) (command list))
  (tetris:with-player (player-functions:init-player player-id)
    (accept-command (player-functions:init-player player-id) command)))

(defmethod accept-command ((player tetris-structures:player) (command list))
  (let ((game-state (tetris-structures:player-game-state player)))
    (alexandria:switch ((first command) :test #'equalp)
      ("left" (tetris:left))
      ("right" (tetris:right))
      ("down" (tetris:down))
      ("rotate" (tetris:rotate))
      ("drop-down" (tetris:drop-down))
      ("set" (let ((what-to-set (second command))
                   (value (third command)))
               (alexandria:switch (what-to-set :test #'equalp)
                 ("game-map"    (setf (tetris-structures:game-map game-state)  value))
                 ("curr-row"    (setf (tetris-structures:curr-row game-state) value))
                 ("curr-column" (setf (tetris-structures:curr-column game-state) value))
                 ("curr-piece"  (setf (tetris-structures:curr-piece game-state)
                                      (let* ((name (fourth command))
                                             (num-of-rotations (mod (third command) 4))
                                             (piece (tetris:get-piece name)))
                                        ;; rotate it a cuple of times...
                                        (dotimes (_ num-of-rotations)
                                          (setf piece (tetris:rotated-piece piece)))
                                        ;; set it
                                        (setf (tetris-structures:curr-piece game-state) piece))))
                 ("difficulty"  (setf (tetris-structures:difficulty game-state) value))))))))


(defun dummy-client ()
  (loop while link:*client-running*
     do (sleep 0.1)))

#+nil (start-dummy-client)
#+nil (link:create-read-loop 'read-loop)
#+nil (start-message)
#+nil (stop)
