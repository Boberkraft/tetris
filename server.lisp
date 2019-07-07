
(defpackage #:serve
  (:use #:cl)
  (:export :start
           :stop
           :*server-))

(in-package :serve)



(defparameter *commands*
  '("left" "right" "down" "rotate" "drop-down" "initialize") "Contains all of the commands that the client can send.")



(defmethod message-with-adressee ((client link:client) message)
  "Makes a string with client-id at the Begining."
  (message-with-adressee (link:client-id client) message))

(defmethod message-with-adressee ((id player-functions:id) message)
  "Makes a string with client-id at the Begining."
  (message-with-adressee (player-functions:id-as-string id) message))

(defmethod message-with-adressee ((id string) message)
  "Makes a string with id at the Begining.
   For example: 'local \"left\"~%'
   If message is a list '(a \"b\") then: 'local (a \"b\")'.
   Typicly if message is 'set', then the second part is the name of the variable to set, third and so on data
   second value.."
  (concatenate 'string
               (format nil "~w ~a" id
                       (replace-endlines-to-spaces (format nil "~s" message)))
               "~%"))

(defun replace-endlines-to-spaces (data)
  "Moves throught tree and replaces all newlines in strings to space"
  (cond ((stringp data) (substitute #\Space #\Newline
                                    data))
        ((listp data) (mapcar #'replace-endlines-to-spaces
                              data))
        (t data)))

#|
;;; Unused and untested https://stackoverflow.com/questions/4366668/str-replace-in-lisp
;; TODO: move to some ultils file.

(defun replace-all (string what-to-replace with-what &key (test #'char=))
  "Returns a new string with with-what instead of what-to-replace."
  (with-output-to-string (out)
    (loop with old-lenght = (length what-to-replace)
       for old-pos = 0 then (+ pos old-lenght)
       for pos = (search what-to-replace string
                         :start2 old-pos
                         :test test)
       do (write-string string out
                        :start old-pos
                        :end (or pos
                                 (length string)))
       when pos do (write-string with-what out)
       while pos)))
|#

(defun initialize-client (who-to-initialize)
  "Sends state of all of the players to who-to-initalize. He is given messages as such:
   id (\"set\" \"game-moge\" ((- - - -) (- - - -))) etc.
   id (\"set\" \"curr-row\" 0)
   id (\"set\" \"curr-piece\" 2 't)"
  (let ((data nil))
    (dolist (player player-functions:*server-players*)
      (let* ((id (tetris-structures:player-id player))
             (game-state (tetris-structures:player-game-state player))
             (game-map (tetris-structures:game-map game-state))
             (curr-row (tetris-structures:curr-row game-state))
             (curr-column (tetris-structures:curr-column game-state))
             (curr-piece (tetris-structures:curr-piece game-state))
             (next-pieces (tetris-structures:next-pieces game-state))
             (rotation (if curr-piece
                           (tetris-structures:piece-rotation curr-piece)
                           0))
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
                           (message-with-adressee id (list "set" "difficulty" difficulty))
                           (message-with-adressee id (list "set" "next-pieces" (mapcar (lambda (piece)
                                                                                         (tetris-structures:piece-name piece))
                                                                                       next-pieces))))
              data)))
    '(format t  "~{~a~}" data)
    (inform-player who-to-initialize (format nil "~{~a~}" data))))
#+ nil (initialize-client "xd")

(defun accept-tetris-command (client command)
  (let ((id (link:client-id client)))
    (format t "~%Message from ~a" id)
    (cond ((not (find command *commands* :test #'equalp))
           ;; Is this a good command?
           (format t "~%Unrecognized command ~w" command))
          (t
           ;; Inits player
           (format t "~%Executing command ~w" command)
           ;; Changes all of the global variables.
           (tetris:with-player (player-functions:init-player (player-functions:make-id :as-string id
                                                                                       :local-p nil))
             (process-command-sended-to-server client command)
             (inform-other-players client (list command)))))))

(defun inform-other-players (from-who command)
  "Sends FORMATED message to all client, with given addressee."
  (link:send-data-to-all-clients
   (message-with-adressee from-who command)))

(defun inform-player (to-who message)
  "Sends UNFORMATED message to a client."
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


(defun start (port)
  (link:start-server port 'accept-tetris-command))

(defun stop ()
  (link:stop-server))


#+ nil (start 5519)
#+ nil (stop)
