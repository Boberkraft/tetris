(defparameter *vertex-stream* nil)
(defparameter *array* nil)
(defparameter *loop* 0.0)

;; note the use of implicit uniform capture with *loop*
;; special vars in scope can be used inline. During compilation
;; cepl will try work out the type. It does this by seeing if the
;; symbol is bound to a value, and if it is it checks the type of
;; the value for a suitable matching varjo type
(defun-g vertex-stage ((v-pos :vec4))
  (let* ((pos ))
    (values v-pos
            v-pos)))

(defun-g plot ((st :vec3))
  (let* ((x (v:s~ st :x))
         (y (v:s~ st :y)))
    (if (< x 0.5)
        (v! 1 0 0)
        (if (> x 0.75)
            (v! 0 1 0)
            (+ (* (v! (smoothstep 0.5 0.75 x)(smoothstep 0.5 0.75 x)(smoothstep 0.5 0.75 x))
                  (v! 0 1 0))
               (* (- 1 (* (v! (smoothstep 0.5 0.75 x)(smoothstep 0.5 0.75 x)(smoothstep 0.5 0.75 x)) ))
                  (v! 1 0 0)))))))

(defun-g fragment-stage ((v-pos :vec4))
  (let* ((pos (v:s~ v-pos :xyz))
    
         (color (/ (+ (v! 1 1 1)
                      pos)
                   2))
         (x (v:s~ color :x))
         
         (color (plot color) ))
    color))


;; Also showing that we can use gpu-lambdas inline in defpipeline-g
;; It's not usually done as reusable functions are generally nicer
;; but I wanted to show that it was possible 
(defpipeline-g lol ()
  (vertex-stage :vec4)
  (fragment-stage :vec4))

(defun step-demo ()
  (step-host)
  (update-repl-link)
  (clear)
  (map-g #'lol *vertex-stream*)
  (swap))

(let ((running nil))
  (defun run-loop ()
    (setf running t)
    (setf *array* (make-gpu-array (list (v! -1  1 0 1)
                                        (v! -1 -1 0 1)
                                        (v!  1 -1 0 1)
                                        (v!   -1    1 0 1)
                                        (v!   1    -1 0 1)
                                        (v!   1     1 0 1)
                                        )))
    (setf *vertex-stream* (make-buffer-stream *array*))
    (loop :while (and running (not (shutting-down-p))) :do
       (continuable (step-demo))))
  (defun stop-loop () (setf running nil)))

(run-loop)
