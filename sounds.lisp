(defpackage #:sounds
  (:use :cl
        )
  (:export :init-sound-system
           :play-background-music
           :stop
           :play-hit-sound
           :resume
           :pausedp
           :toggle-on-off
           :change-volume))
(in-package :sounds)

(defvar *background-music* nil)
(defvar *hit-sound* nil)

(defparameter *stopped* nil)
(defparameter *is-background-playing* nil)
(defparameter *background-source* nil)
(defparameter *server* nil)
(defparameter *volume* 1 "From 0 to 1")
;;TODO: MAKE IT IN BUFFERS AAAAAA

(defun pausedp ()
  *stopped*)

(defun init-sound-system ()
  (setf *server* (harmony-simple:initialize :output-spec '(harmony-out123:out123-drain))))

(defun play-background-music ()
  (when (and (not *is-background-playing*)
             (not *stopped*))
    (setf *background-source* (harmony-simple:play #p"music/chibi-tech Smugface Mafia.mp3" :music))
    (setf *is-background-playing* t)
    (reload-volume)))

(defun stop ()
  (setf *stopped* t)
  (when *background-source*
    (harmony:pause *background-source*)))

(defun resume ()
  (when *stopped*
    (play-background-music))
  (setf *stopped* nil))

(defun change-volume (val)
  (setf *volume* (cond ((> val 1) 1)
                       ((< val 0) 0)
                       (t val)))
  (reload-volume))

(defun reload-volume ()
  "Setf *volume* to current background"
  (setf (harmony-simple:volume *background-source*)
        *volume*))

(defun toggle-on-off ()
  "Toggles between ON and OFF.
   Resumes when stopped, stopps when playinh"
  (if (pausedp)
      (resume)
      (stop)))

(defun play-hit-sound ()
  "The sound of block hitting"
  (harmony-simple:play #p"music/kick.mp3" :sfx))
