(defpackage #:player-functions
  (:use #:cl
        #:tetris-structures)
  (:export :id
           :make-id
           :id-as-string
           :id-local-p

           :*players*
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

(defparameter *players* nil "List of local struct player")
(defparameter *server-players* nil "List of multiplayer players")
(defparameter *callback-for-hooking-up-callbacks* nil "Provided by testing-redering")
;; The player with id "local" is the host.
;;render stuff


(defparameter *curr-player* nil)

(defstruct id
  as-string
  local-p)



(defgeneric init-player (identificator)
  (:documentation "Retruns initialized player or nil if couldn't"))

(defmethod init-player :before ((player player))
  "Returns player." ;; TODO get rid of such functions.
  (tetris:reinit-tetris (player-game-state player)))

(defmethod init-player ((player player))
  "Returns player."
  player
  )

(defmethod init-player ((id id) )
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

(defmethod init-player ((id string))
  (init-player (make-id :as-string id
                        :local-p t)))
;;

(defun find-player (id)
  "If player isn't found, returns nil"
  (find-if (lambda (player)
             (equal (id-as-string id)
                    (id-as-string (player-id player))))
           (if (id-local-p id)
               *players*
               *server-players*)))


(defun add-new-player (id)
  "Adds new player to the game and returns him"
  (let ((player (make-player :id id
                             :game-state (tetris:create-game-state)
                             :render-state (make-instance 'render-state)
                             :number (length *players*))))
    (if *callback-for-hooking-up-callbacks*
        (funcall *callback-for-hooking-up-callbacks* player)
        (format t "~%No hooking up of callbacks."))
    (if (id-local-p id)
        (setf *players* (append  *players* (list player)))
        (setf *server-players* (append  *server-players* (list player))))
    player))









