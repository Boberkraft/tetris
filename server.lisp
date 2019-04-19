
(defpackage #:serve
  (:use #:cl)
  (:export :start
           :stop
           :*server-))

(in-package :serve)



(defparameter *commands*
  '("left" "right" "down" "rotate" "drop-down") "Contains all of the commands that the client can send.")

(defun server-id (id)
  (concatenate 'string
               "server"
               id))

(defmethod message-with-adressee ((client link:client) message)
  "Makes a string with client-id at the Begining."
  (message-with-adressee (link:client-id client) message))

(defmethod message-with-adressee ((id string) message)
  "Makes a string with id at the Begining.
   For example: 'local \"left\"~%'
   If message is a list '(a \"b\") then: 'local (a \"b\")'.
   Typicly if message is 'set', then the second part is the name of the variable to set, third and so on data
   second value.."
  (concatenate 'string
               (format nil "~w ~w" id  message)
               "~%"))

(defun initialize-client (who-to-initialize)
  (let ((data nil))
    (dolist (player player-functions:*players*)
      (let* ((id (tetris-structures:player-id player))
             (game-state (tetris-structures:player-game-state player))
             (game-map (tetris-structures:game-map game-state))
             (curr-row (tetris-structures:curr-row game-state))
             (curr-column (tetris-structures:curr-column game-state))
             (curr-piece (tetris-structures:curr-piece game-state))
             (rotation (when curr-piece
                         (tetris-structures:piece-rotation curr-piece)))
             ;; NOTE: can some trickster can control this value?
             ;; Nope. It wound be fun throught.... Imagine rotation 99999999 heh
             (shape-name (when curr-piece
                           (tetris-structures:piece-name curr-piece)))
             (difficulty (tetris-structures:difficulty game-state)))
        ;; make big string of message and push
        (push (concatenate 'string
                           (message-with-adressee id (list "set" "game-map" game-map))
                           (message-with-adressee id (list "set" "curr-row" curr-row))
                           (message-with-adressee id (list "set" "curr-column" curr-column))
                           (message-with-adressee id (list "set" "curr-piece" rotation shape-name))
                           (message-with-adressee id (list "set" "difficulty" difficulty)))
              data)))
    (inform-player who-to-initialize (format nil "~{~a~}" data))))

(defun accept-tetris-command (client command)
  (let ((id (link:client-id client)))
    (format t "~%Message from ~a" id)
    (cond ((not (find command *commands* :test #'equalp))
           ;; Is this a good command?
           (format t "~%Unrecognized command ~w" command))
          (t
           ;; Inits player
           (format t "~%Executing command ~w" command)
           (tetris:with-player (player-functions:init-player (player-functions:make-id :as-string id
                                                                                       :local-p nil))
             ;; Changes all of the global variables.
             (process-command-sended-to-server client command)
             (inform-other-players client (list command)))))))

(defun inform-other-players (from-who command)
  "Sends formated message to all client, with given addressee."
  (link:send-data-to-all-clients
   (message-with-adressee from-who command)))

(defun inform-player (to-who message)
  (link:send-data-to-client to-who
                            message))

(defun process-command-sended-to-server (client command)
  "Calls resposible tetris function"
  (alexandria:switch (command :test #'equalp)
    ("left" (tetris:left))
    ("right" (tetris:right))
    ("down" (tetris:down))
    ("rotate" (tetris:rotate))
    ("drop-down" (tetris:drop-down))
    ("initialize" (initialize-client client))))


(defun start ()
  (link:start-server 'accept-tetris-command))

(defun stop ()
  (link:stop-server))


#+ nil (start)
#+ nil (stop)
