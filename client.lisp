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

(defun simple-messages ()
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
  "Every global variable is replaced. Look at format spec at the server sending function. "
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
                 ("difficulty"  (setf (tetris-structures:difficulty game-state) value))
                 ("next-pieces" (setf (tetris-structures:next-pieces game-state) (mapcar (lambda (piece-name)
                                                                                           (tetris:get-piece piece-name))
                                                                                         value)))
                 ("next-piece" (tetris:add-piece-to-queue value))))))))


(defun super-client ()
  (dotimes (i 30)
    (let ((choices '(client:left client:left client:left client:left client:left client:left
                     client:right client:right client:right client:right client:right client:right
                     client:rotate
                           client:rotate
                           client:down client:down client:down client:down client:down client:down client:down client:down
                           client:down client:down client:down client:down client:down client:down client:down client:down
                           client:drop-down)))
            (sleep 0.15)
            (funcall (nth (random (1- (length choices)))
                          choices)))))

(defun im-a-new-connection-wanting-to-play ()
  (link:start-client "127.0.0.1"
                     5520)
  (link:create-read-loop 'read-loop)
  (initialize)
  (super-client))

#+nil (im-a-new-connection-wanting-to-play)

#+nil (inject-controls)
#+nil (super-client)
#+nil '((left) (right) (down) (drop-down) (rotate) (initialize))
#+nil (start-message)
#+nil (stop)
