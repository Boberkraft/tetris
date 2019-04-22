(defpackage #:sounds
  (:use :cl)
  (:export :init-sound-system
           :play-song ;; FIXME
           :play-next-song
           :stop
           :play-hit-sound
           :resume
           :pausedp
           :music-on-off
           :change-volume))
(in-package :sounds)

(defstruct music-queue
  (current-number nil)
  album
  (volume 1)
  (source nil)
  (playingp nil))

(defmethod music-queue-change-volume ((queue music-queue) (volume number))
  "Changes the volume of current song to a value between 0 and 1."
  (when (music-queue-playingp queue)
    (let ((new-volume (cond ((> volume 1) 1) ;
                            ((< volume 0) 0) ; in-between
                            (t volume))))    ;
      (setf (music-queue-volume queue) new-volume)
      (music-queue-reload-volume queue))))

(defmethod music-queue-reload-volume ((queue music-queue))
  "Sets volume to value given in queue."
  (setf (harmony-simple:volume (music-queue-source queue))
        (music-queue-volume queue)))

(defmethod music-queue-stop-playing ((queue music-queue))
  (when (music-queue-playingp queue)
    (when harmony-simple:*server*
      (harmony:pause (music-queue-source queue))))
  (setf (music-queue-playingp queue) nil))

(defmethod music-queue-play-current-song ((queue music-queue))
  (music-queue-play-nth-song queue
                             (music-queue-current-number queue)))

(defmethod music-queue-play-next-song ((queue music-queue))
  (let* ((next-number (1+ (music-queue-current-number queue)))
         (num (if (> (length (music-queue-album queue)) next-number)
                  next-number
                  0)))
    (music-queue-play-nth-song queue num)))

(defmethod music-queue-play-nth-song ((queue music-queue) (num number))
  "Plays the nth song from album. If it doesnt exists, do nothing."
  (when (and (< num
                (length (music-queue-album queue))) ; in limit?
             harmony-simple:*server*) ; is initialized?
    (when (music-queue-playingp queue)
      (print (music-queue-source queue))
      (print "pausing")
      (harmony:stop (music-queue-source queue)) ; NOTE can be a memory leak.
      (print (music-queue-source queue)))
    (print num)
    (let* ((record (nth num (music-queue-album queue)))
           (type (first record))
           (name (second record)))
      (setf (music-queue-source queue) (harmony-simple:play name type )) ; play new
      (setf (music-queue-current-number queue) num)
      (setf (music-queue-playingp queue) t))
    (music-queue-reload-volume queue))) ; set volume to good value.

(defmethod music-queue-play-sound ((queue music-queue))
  (let* ((record (nth (music-queue-current-number queue)
                      (music-queue-album queue)))
         (type (first record))
         (name (second record)))
    (setf (music-queue-source queue) (harmony-simple:play name type))))

;;;; -------------------------------------------------------------------
(defparameter *server* nil "Harmony server.")
(defparameter *bg-queue* (make-music-queue :current-number 0
                                           :album nil)
  "Instance of music-queue. Contains all of the background songs.")

(defparameter *hit-queue* (make-music-queue :current-number 0
                                            :album nil)
  "Instance of music-queue. Contains sounds for block hitting.")
;;TODO: MAKE IT IN BUFFERS


(defun pausedp ()
  "Retrns true if background music is playing."
  (not (music-queue-playingp *bg-queue*)))


(defun init-sound-system ()
  (setf *bg-queue* (make-music-queue :current-number 0
                                     :album '((:sfx #p"music/Alex - Analog Electronic Synth Sound 01.mp3")
                                              (:sfx #p"music/banks.mp3")
                                              (:sfx #p"music/chibi-tech Smugface Mafia.mp3")
                                              (:sfx #p"music/Chipzel Focus.mp3")
                                              (:sfx #p"music/Chipzel To The Sky.mp3"))))
  (setf *hit-queue* (make-music-queue :current-number 0
                                      :album '((:sfx #p"music/kick.mp3"))))
  (setf *server* (harmony-simple:initialize :output-spec '(harmony-out123:out123-drain))))


(defun play-song (number)
  "Starts background song"
  (print number)
  (music-queue-play-nth-song *bg-queue* number))

(defun play-next-song ()
  (music-queue-play-next-song *bg-queue*))

(defun stop ()
  "Stops background song"
  (music-queue-stop-playing *bg-queue*))

(defun resume ()
  (play-background-music))

(defun change-volume (val)
  (music-queue-change-volume *bg-queue* val))

(defun music-on-off ()
  "Toggles between ON and OFF.
   Resumes when stopped, stopps when playing"
  (if (music-queue-playingp *bg-queue*)
      (resume)
      (stop)))

(defun play-hit-sound ()
  "The sound of block hitting"
  (music-queue-play-sound *hit-queue*))
