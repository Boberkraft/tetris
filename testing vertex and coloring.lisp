(ql:quickload :nineveh)
(ql:quickload :cepl)
(ql:quickload :cepl.sdl2)
(ql:quickload :rtg-math)
(use-package :nineveh)
(use-package :cepl)
(use-package :rtg-math)

(defparameter *vertex-stream* nil)
(defparameter *array* nil)
(defparameter *fbo* nil)
(defparameter *fbo-tex* nil)
(defparameter *fbo-tex-sampler* nil)
(defparameter *loop* 0.0)

;; note the use of implicit uniform capture with *loop*
;; special vars in scope can be used inline. During compilation
;; cepl will try work out the type. It does this by seeing if the
;; symbol is bound to a value, and if it is it checks the type of
;; the value for a suitable matching varjo type
(defun-g vertex-stage ((v-pos :vec4))
  (let* ((pos 0))
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

(defun-g second-vertex-stage ((v-pos :vec4))
  (values))

(defun-g second-fragment-stage ((v-pos :vec4)
                                &uniform
                                (sampler :sampler-2d))
  (let* ((v-pos (v:+ (v! 1 1 1 0)
                     v-pos))
         (v-pos (v:/ v-pos
                     2)) ;; normalize: (v + 1) /2
         (x (v:s~ v-pos :x))
         (y (v:s~ v-pos :y)))
    (if nil ;; t or nil to choose between sampler or dark green.
     (v:s~ (texture sampler (v! x y)) :xyz)
     (v!  0 0.4 0.3))))


(defpipeline-g new-pipe ()
  (vertex-stage :vec4)
  (second-fragment-stage :vec4))

(defun step-demo ()
  (step-host)
  (setf (resolution (current-viewport))
        (surface-resolution (current-surface (cepl-context))))
  (with-fbo-bound (*fbo*) ; cały rendering do tego fbo
    (clear)
    (map-g #'lol *vertex-stream*))
  ;; teraz w (pull-g (attachment *fbo* 0)) i w (texref *fbo-tex*) mamy wartosci
  (clear)
  (map-g #'new-pipe *vertex-stream*
         :sampler *fbo-tex-sampler*)
  (swap))


(defun init ()
  (unless *fbo*
    (setf *fbo* (make-fbo 0))) ; tworzę fbo z 1 atachementem koloru
  (unless *fbo-tex*
    ;; biorę ten 1 at. koloru i zamieniam go na teksture
    (setf *fbo-tex* (gpu-array-texture (attachment *fbo* 0))))
  (unless *fbo-tex-sampler*
    (setf *fbo-tex-sampler* (sample *fbo-tex*)))
  (unless *array*
    (setf *array* (make-gpu-array (list (v! -0.6  0.8 0 1)
                                        (v! -1 -1 0 1)
                                        (v!  1 -1 0 1)
                                        (v! -1  1 0 1)
                                        (v!  1 -1 0 1)
                                        (v!  1  0.5 0 1)
                                        ))))
  (unless *vertex-stream*
    (setf *vertex-stream* (make-buffer-stream *array*))))


(def-simple-main-loop play (:on-start #'init)
  (step-demo))
