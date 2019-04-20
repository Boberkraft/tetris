;; This file is reposible for providing functionality
;; for connecting to a tetris server (defined in server.lisp).
;; It accepts commands send from server, and then executes then on
;; local game of tetris. It might be a console tetris (tetris.lisp)
;; or this colored graphical tetris (testing-rendering.lisp)

(defpackage #:client
  (:use #:cl)
  (:export :start
           :stop
           :left
           :right
           :start-dummy-client
           :down
           :read-loop
           :rotate
           :super-client
           :drop-down))


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

(defun inject-controls ()
  (testing-rendering:prepere-for-multiplayer)
  (testing-rendering:inject-controls (list (list :left 'aleft)
                                           (list :right 'aright)
                                           (list :down 'adown)
                                           (list :drop-down 'adrop-down)
                                           (list :rotate 'arotate))))

(defun left ()
  (send "left~%"))

(defun right ()
  (send "right~%"))

(defun down ()
  (send "down~%"))

(defun drop-down ()
  (send "drop-down~%"))

(defun rotate ()
  (send "rotate~%"))

(defun initialize ()
  (send "initialize~%"))

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
      (testing-rendering:with-player (player-functions:init-player from-who) ;; NOTE, with player?
        (process-command command)))))


(defmethod process-command ((command list))
  "Every global variable is replaced.  "
  (let ((game-state tetris:*game-state*))
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
                                             ;; rotating by 4 is like rotating by 0
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

(defun super-client ()
  (start-dummy-client)
  (link:create-read-loop 'read-loop)
  (loop (let ((choices '(client:left client:left client:left client:left client:left client:left
                         client:right client:right client:right client:right client:right client:right
                         client:rotate
                         client:rotate
                         client:down client:down client:down client:down client:down client:down client:down client:down
                         client:down client:down client:down client:down client:down client:down client:down client:down
                         client:drop-down)))
          (sleep 0.3)
          (funcall (nth (random (1- (length choices)))
                        choices)))))
#+nil (start-dummy-client)
#+nil (link:create-read-loop 'read-loop)
#+nil (inject-controls)
#+nil (super-client)
#+nil '((left) (right) (down) (drop-down) (rotate))
#+nil (start-message)
#+nil (stop)
