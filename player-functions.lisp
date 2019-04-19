(defpackage #:player-functions
  (:use #:cl
        #:tetris-structures)
  (:export :*players*
           :*local-player*
           :*callback-for-hooking-up-callbacks*
           :players
           :host-player
           :add-new-player
           :add-local-player
          
           :find-player
           :with-player
           :init-player))

(in-package :player-functions)

(defparameter *players* nil "List of struct player")
(defparameter *callback-for-hooking-up-callbacks* nil "Provided by testing-redering")
;; The player with id "local" is the host.
;;render stuff


(defparameter *curr-player* nil)

(defgeneric init-player (identificator)
  (:documentation "Retruns initialized player or nil if couldn't"))

(defmethod init-player :before ((player player))
  "Returns player."
  (tetris:reinit-tetris (player-game-state player)))

(defmethod init-player ((player player))
  "Returns player."
  player
  )

(defmethod init-player ((id string))
  "Inits player with such id. If there is no such player, it intializes him.
   Returns this player"
  (let ((player (find-player id)))
    (init-player (if player
                     player
                     (add-new-player id))))
  )

(defmethod init-player ((number number))
  "Inits player with such nth number.
   Returns this player or nil if there is no such player"
  (init-player (nth number *players*)))


;;

(defun find-player (id)
  "If player isn't found, returns nil"
  (find-if (lambda (player)
             (equal id (player-id player)))
           *players*))

(defun add-new-player (id)
  "Adds new player to the game and returns him"
  (let ((player (make-player :id id
                             :game-state (tetris:create-game-state)
                             :render-state (make-instance 'render-state)
                             :number (length *players*))))
    (if *callback-for-hooking-up-callbacks* 
        (funcall *callback-for-hooking-up-callbacks* player)
        (format t "~%No hooking up of callbacks."))
    (setf *players* (append  *players* (list player)))
    player))









