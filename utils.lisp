(defpackage #:utils
  (:use :cl
        :rtg-math
        )
  (:export :stepper-can-p
           :stepper-reset
           :now
           :get-color-v-for-block
           :on-key-down
           :on-key-up
           :on-key))


(in-package :utils)


(defun make-stepper (time)
  "Useful helper. (Stepper-can-p) returns True only if internal timer reached 0.
   (stepper-reset) sets this timer to the default value of 'to-wait."
  (let ((old-time 0)
        (to-wait time))
    (lambda (&optional reset)
      '(print (- (now) old-time))
      (cond
        (reset (setf old-time (now))) ;reset timer
        (t
         (< to-wait (- (now) old-time))
         )))))

(defparameter *stepper* (make-stepper 0.3))

(defun stepper-reset ()
  (funcall *stepper* t))
(defun stepper-can-p ()
  (funcall *stepper*))

(defun get-color-v-for-block (sym)

  (alexandria:switch (sym :test #'string=)
    ("A"    (v! 0.27 0.54 0.4 ))
    ("B"    (v! 1.0  0.54 0.65))
    ("C"    (v! 1.0  0.69 0.23))
    ("D"    (v! 0.71 0.29 0.15))
    ("E"    (v! 0.56 0.16 0.0 ))
    ("F"    (v! 0.0  0.69 0.23))
    ("G"    (v! 0.71 0.29 1   ))
    ("H"    (v! 0.27 0.4  0.7 ))
    ("X"    (v! 0    1    0   ))
    ("*"    (v! 1    0    0   ))
    ("-"    (v! 1.01 0.50 1.00))
    ("grey" (v! 0.1  0.1  0.1 ))
    ("+"    (v! 0.05 0.05 0.05))
    (t (error (format nil "color for block: ~w not found!" sym)))))

(defun now ()
  (/ (float (get-internal-real-time))
     500))



(defparameter *active-listeners* nil "Alist containing  pairs KEY . (callback EVENT-LISTINER)")

(defun add-event-listener-to-system (key callback special-callback)
  "adds event to system, replacing old both use the same calback function"

  (let* ((value (assoc key *active-listeners*))
         (old-callback (cadr value))
         (old-event-listener (caddr value))
         )

    (when  (equalp old-callback ;; check if functions are queal
                   callback)   ;; if yes, then this callback is already added.
      (skitter:stop-listening old-event-listener)
      (setf *active-listeners*
            (remove key *active-listeners* :key #'car)))
    (let ((listener (skitter:listen-to ;; create listener
                     special-callback
                     ;; see on-key-up
                     ;; or on-key-down.
                     ;; If this wasnt some weird shit then you can do
                     ;; (skitter:make-event-listener 'callback) and it serves repl well.
                     (skitter:keyboard 0)
                     :button key)))
      (push (cons key (list callback listener))
            *active-listeners*))))


(defun on-key-up (key callback &optional more-data-p)
  "Calls callback when key is unpressed.
   For example:  (on-key-up skittle.glop.keys:r 'rotate)
              or (on-key-up skittle.glop.keys:r #'rotate).
  The first version offers ability to redefine funcion.
  If more-data-p is true then callback are going to get 4 arguments."
  (format t "Registering new key UP ~a with ~a~%" key callback)
  (add-event-listener-to-system key
                                callback
                                (lambda (pressed &rest rest)
                                  (when (not pressed)
                                    (if more-data-p
                                        (funcall callback rest)
                                        (funcall callback))))))

(defun on-key-down (key callback &optional more-data-p)
  "Calls callback when key is first time pressed. Resets on unpressing"
  (format t "Registering new key DOWN ~a with ~a~%" key callback)
  (add-event-listener-to-system key
                                callback
                                (let ((last-time nil)) ;; stores last value of pressed
                                  (lambda (pressed &rest rest)
                                    (when (and (not last-time)
                                               pressed)
                                      (if more-data-p
                                          (funcall callback rest)
                                          (funcall callback)))
                                    (setf last-time pressed)))))

(defun on-key (key callback &optional more-data-p)
  "Calls callback when key is being holded. Resets on unpressing"
  (format t "Registering new key ~a with ~a~%" key callback)
  (add-event-listener-to-system key
                                callback
                                ;; stores last value of pressed
                                (lambda (pressed &rest rest)
                                  (declare (ignore pressed))
                                  (if more-data-p
                                      (funcall callback rest)
                                      (funcall callback)))))
