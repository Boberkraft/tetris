(defpackage #:link
  (:use #:cl)

  (:export ;; server
           :start-server
           :stop-server
           :send-data-to-all-clients
           :send-data-to-client
           :client
           :client-id
           :client-player
           :server-running-p

           ;; client
           :*client-running*
           :*server-stream*
           :start-client
           :stop-client
           
           :send-data-to-server
           :create-read-loop
           ))

(in-package :link)

;; FIXME, printing info in threads can led to race condition. Nothing serious.
;; i sound have made a special logging function.




;;server
(defparameter *server-running* nil)
;; TODO move this to tetris
(defparameter *server-lock* (bt:make-lock) "Used so only one thread can symulate tetris at a time.")
(defparameter *change-clients-lock* (bt:make-lock) "Used so only one thread can add/remove clients at a time.")
(defparameter *clients* nil "instances of client")
(defparameter *connection-id* 0) ;; unused for now.

;; NOTE:
;; Maybe i should be giving an unique id to each connection,
;; so they can authenticate themself later?

;; Right now im authenticating connection based on their ip and port.
;; If its the same, then the player is the same ;

(defstruct client
  id
  connection ;; accepted socket from usocket
  (data-to-send nil) ; list of strings
  (lock (bt:make-lock))
  (player nil))


(defmethod put-data-to-send ((client client) (data string))
  (bt:with-lock-held ((client-lock client))
    (push data (client-data-to-send client))))

(defmethod get-data-to-send ((client client))
  (bt:with-lock-held ((client-lock client))
    (pop (client-data-to-send client))))

(defmethod send-data-to-client ((client client) (data string))
  (bt:with-lock-held ((client-lock client))
    (format (usocket:socket-stream (client-connection client))
            data)
    (force-output (usocket:socket-stream (client-connection client))))
  (format t "~% - [Server]: sended to ~a: ~a - " (client-id client) data))

(defun add-new-client (connection)
  "Adds clients to database and returns created client"
  (let ((client (make-client :id (make-id (usocket:get-peer-address connection)
                                          (usocket:get-peer-port connection))
                             ;; example id: "#(127 0 0 1)55470"
                             :connection connection)))
    (bt:with-lock-held (*change-clients-lock*)
      (push client *clients*))
    client))

(defun remove-client (client)
  "Removes client from database."
  (bt:with-lock-held (*change-clients-lock*)
    (setf *clients*
          (remove client *clients* :test #'eq))))

(defun send-data-to-all-clients (message)
  "Sends message to all of the clients."
  (bt:with-lock-held (*change-clients-lock*)
    (dolist (client *clients*)
      (send-data-to-client client message))))


(defun make-id (&rest lst)
  ;;TODO: Maybe it should be replecable/changeble/schadowable? Maybe it is?
  "Converts given list to string. Used as id"
  (format nil "~{~a~}" lst))

(defun server-running-p ()
  *server-running*)

(defun start-simple-server (port callback)
  "Listen on port for messange, and call callback with recived input"
  (usocket:with-socket-listener (socket "127.0.0.1" port)
    (loop while *server-running*
       do (progn
            #+ nil (format t "~% - [Server]: waiting for connection -")
            ;; make a new thread for this connection.
            (when (and (usocket:wait-for-input socket :timeout 1) ;; SETS SOCKET
                       ;; wait for 1 second, returns nil if timeouted.
                       *server-running*)
              (let ((connection (usocket:socket-accept socket)))
                (bt:make-thread
                 (lambda ()
                   (usocket:with-connected-socket (connection connection)
                       (handle-connection connection callback)))
                 :name (format nil "<SERVER: Thread for handling ~a>" connection) )))))))

(defun handle-connection (connection callback)
  "Handles the communication."
  (let ((client (add-new-client connection)))
    (unwind-protect
         (progn (format t "~% - [Server]: connection ~w accepted - " (client-id client))
                ;; Strip/trim all of the Spaces and Newlines from end and beginning.
                (handler-case (loop
                                 ;; read data
                                 (let ((data (read-line (usocket:socket-stream (client-connection client)))))
                                   ;; read-line is blocking, can signal EOF
                                   (bt:with-lock-held (*server-lock*)
                                     (funcall callback client
                                              (string-trim '(#\Space #\Newline)
                                                           data)))))
                  ;; FIXME add more exceptions!
                  (end-of-file (c) ; connection closed
                    (declare (ignore c))
                    (format t "~% - [Server]: connection ~w closed - " (client-id client)))))
      ;; clean-up
      (remove-client client))))


(defun start-server (port function)
  (setf *server-running* t)
  (format t "~% - [Server]: STARTING -")
  (bt:make-thread
   (lambda ()
     (unwind-protect
          (progn (loop while *server-running*
                    do (progn
                         (start-simple-server port function))))
       (stop-server))
     (format t "~% - [Server] STOPING - "))
   :name "<SERVER main thread>"))

(defun stop-server ()
  "Resets all server variables"
  (setf *server-running* nil))

;;; ---------------------------------------------- CLIENT

;;client
(defparameter *client-running* nil "True if the client is running")
(defparameter *server-stream* nil "Stream that connects to the server. Nil if it doesnt exists.")
(defparameter *data-to-send* nil "List of data to send.")
(defparameter *read-loop-lock* (bt:make-lock))
(defparameter *unreaded-data* nil)

(defun start-client (function ip port)
  (if (not *client-running*)
      (progn
        (bt:make-thread (lambda ()
                          (simple-client ip port function)))
        (setf *client-running* t))
      (format t "~% - [Client]: ALREADY RUNNING -")))

(defun stop-client ()
  "Deinitializes all variables."
  (format t "~% - [Client]: STOPPING -")
  (setf *client-running* nil)
  (setf *server-stream* nil))

(defun connected-p ()
  "Retruns true if connection between server and client is established."
  (and *client-running*
       *server-stream*))

;;; ---------- Imperetive style. Unused and UNSTESTED
(defun create-read-loop ()
  (bt:make-thread (lambda ()
                    (loop while *client-running*
                       do (funcall 'read-loop)))
                  :name "<CLIENT functional read-loop>"))

(defun read-loop ()
  (when (connected-p)
    (let ((data (read link:*server-stream*)))
      (bt:with-lock-held (*read-loop-lock*)
        (push data *unreaded-data*)))))

(defun is-there-data-to-read ()
  (null *unreaded-data*))

(defun get-data ()
  (bt:with-lock-held (*read-loop-lock*)
    (pop *unreaded-data*)))

;;; ----------------
;; Functional style.
(defun create-read-loop (callback)
  "Creates a standalone thread that calls callback while the client is running."
  (bt:make-thread (lambda ()
                    (loop while *client-running*
                       do (when *server-stream* ; call only if the connection is established.
                            (on-message-loop callback))))
                  :name "<CLIENT functional read-loop>"))


(defun on-message-loop (callback)
  "Calls callback on every new message recived from server."
  ;; blocking
  (let ((data (read-line link:*server-stream*)))
    '(sleep 1)
    '(format t "~% - [Client]: recived: ~a" data)
    (funcall callback data)))

(defun send-data-to-server (message)
  (handler-case
      (progn (format *server-stream* message)
             (force-output *server-stream*))))

(defun simple-client (ip port callback)
  "Connect to a server and send a messange."
  (format t "~% - [Client]: STARTING - ")
  ;; FIXME: do it without with this with-xxx stuff and just close the connection at stop.
  ;; so this sleep can be removed. Then sending data is simpyfied and you can just pass socket stream.
  (unwind-protect
       (usocket:with-client-socket (socket stream ip port)
         (format t "~% - [Client]: connected - ")
         (setf *server-stream* stream)
         (loop while *client-running*
            do (funcall callback)))
    ;; something broke (prob. socket in use or something)
    (progn (stop-client))))


;; (handler-bind ((error (lambda (c)
;;                         (declare (sb-ext:disable-package-locks 'accept))
;;                         (print c)
;;                         (sb-ext:unlock-package :sb-impl)
;;                         (invoke-restart 'sb-impl::accept))))

;;   (ql:quickload :nineveh))
