;;;; testing-rendering.lisp




;; you can play with (play :start).
;; The controls  are:
;; - (a s d) ,
;; - r for rotating,
;; - space for droping down,
;; - q to quit,
;; - m to mute music. (TODO)

(defpackage #:testing-rendering
  (:use #:cl
        #:cepl
        #:rtg-math
        #:nineveh
        #:varjo
        #:vari
        #:cepl.skitter.sdl2
        #:utils
        #:tetris-structures)
  (:export :init-player
           :with-player
           :inject-controls
           :play
           :prepere-for-multiplayer))

(in-package :testing-rendering)

#+nil (progn
        (declaim (optimize (debug 3)))
        (swank:set-default-directory "c:\\Users\\Bobi\47)

\Desktop\\lispu\\projekty
\\testing-rendering\\")
        (push #p"C:/Users/Bobi/Desktop/lispu/projekty/testing-rendering/" asdf:*central-registry*)
        (ql:quickload :testing-rendering)
        (in-package #:testing-rendering))

;;cepl stuff
(defvar *buf-stream* nil)
(defvar *gpu-arr* nil)
(defparameter *frames* nil)
(defclass frame ()
  ((fbo
    :accessor frame-fbo)
   (tex
    :accessor frame-tex)
   (sampler
    :accessor frame-sampler)
   (in-use
    :documentation ""
    :initform nil
    :accessor frame-in-use-p)))

(defmethod initialize-instance :after ((c frame) &rest args)
  (declare (ignore args))
  (format t "~% Making new fbo.")
  (setf (frame-fbo c) (make-fbo 0 ))
  (setf (frame-tex c) (gpu-array-texture (attachment (frame-fbo c) 0)))
  (setf (frame-sampler c) (sample (frame-tex c))))

(defmethod use-frame ((c frame))
  (setf (frame-in-use-p c) t))

(defmethod free-frame ((c frame))
  (setf (frame-in-use-p c) nil))

(defmethod frame-is-free-p ((c frame))
  (not (frame-in-use-p c)))

(defun make-some-frames (how-many)
  (loop for frame in *frames*
     do (free (frame-fbo frame)))
  (setf *frames*
        (loop for x below how-many
           collect (make-instance 'frame))))

(defun get-free-frame ()
  (let ((fr (find-if 'frame-is-free-p *frames*)))
    (if fr
        (progn (use-frame fr)
               (clear (frame-fbo fr))
               fr)
        (error "No free frame found!"))))

(defmacro with-frame (name &body body)
  `(let ((,name (get-free-frame)))
     ,@body
     (free-frame ,name)))

(defmacro with-frames (names &body body)
  (let ((lamba-list (loop for name in names
                       collect (list name (list 'get-free-frame))))
        (free-list (loop for name in names
                      collect (list 'free-frame name))))
    `(let (,@lamba-list)
       ,@body
       ,@free-list)))

(defmacro with-player (player &body body)
  "Replaces all the variables so the game is tricked into thinking that this player is the only player"
  `(let ((*curr-player* ,player)
         (*render-state* (player-render-state ,player)))
     (tetris:with-player ,player
       ,@body)))


(defparameter *local-player* nil "Gets input from this player")
(defparameter *render-state* nil "Instance of rendering-state. All functions are drawing from this thing.")
(defparameter *curr-player* nil "")
(defparameter *playing-multiplayer* nil)
(defparameter *controls* nil "")
(setf *render-state* (make-instance 'render-state))
(setf player-functions:*callback-for-hooking-up-callbacks*
      'register-callbacks)

;;;;;;;;;;;;;;;;;;;;;;;; TETRIS BLOCKS

(defun-g some-vert-stage-z ((vert g-pnt)
                            &uniform (now :float)
                            (perspective :mat4)
                            (cords :vec3)
                            (block-color :vec3)
                            (width :int)
                            (height :int))
  (let* ((pos (pos vert))
         (pos (+ pos
                 (v! 0.5 -0.5 0.5)))
         (pos (* pos
                 (v! width height 0)))
         (pos (+ pos cords ))
         (pos (+ pos (v! 0
                         0
                         (+ 0
                            -11)))))
    (values (* perspective (v! pos 1))
            (+ (/ 1
                  5)
               (* block-color
                  (+ 0 (z (pos vert))
                     (if (= 0 (z cords))
                         1.0
                         1.0
                         ))))
            )))


(defun-g some-frag-stage ((color :vec3))
  (v! color
      1)) ; alpha for blending

(defpipeline-g some-pipeline ()
  (some-vert-stage-z g-pnt)
  (some-frag-stage :vec3))

;;;;;;;;;;;;;;;;;;;; BACKGROUND

(defun-g wall-vert-stage ((vert g-pnt)
                          &uniform (now :float)
                          (perspective :mat4)
                          (cords :vec3)
                          (block-color :vec3)
                          (width :int)
                          (height :int))
  (let* ((pos (v:s~ (pos vert) :xyz))
         (org-pos (v:s~ pos :xy))
         (pos (+ pos
                 (v! 1 -1 0)
                 (v! 0 0 0)))
         (pos (* pos
                 (v! 5 10 1)))
         (pos (+ pos
                 cords))
         (cords2 (v! pos 1))
         )
    (values (* perspective (v! pos 1))
            (/ (+ org-pos (v! 1 1))
               2))))

(defun-g wall-frag-stage ((pos :vec2)
                          &uniform
                          (multipler :float)
                          (color :vec3))
  ;;#+nil(v! (perlin-noise (* (v:swizzle color :xy) 1))
  ;;         0)
  (let* ((pos (v:s~ pos :yyy))
         (grey-scale (- (v! 1 1 1) pos))

         (colored-vector (* grey-scale
                            color))
         (grey-background (/ grey-scale 5)))

    (v! (+ (* grey-background (- 1 multipler))
           (* colored-vector multipler))
        1))) ; alhpa for blending

(defpipeline-g wall-pipeline ()
  (wall-vert-stage g-pnt)
  (wall-frag-stage :vec2))

(defun-g ghost-vert-stage ((v-pos :vec4))
  (values v-pos
          v-pos))

(defun-g ghost-frag-stage ((v-pos :vec4)
                           &uniform
                           (block-sampler :sampler-2d)
                           (bg-sampler :sampler-2d))
  (let* ((cords (/ (+ (v:s~ v-pos :xy)
                      (v! 1 1))
                   2)) ;; normalize
         (block-color (texture block-sampler cords))
         (bg-color (texture bg-sampler cords)))
    (if (= 0 (v:s~ block-color :w))
        bg-color
        (v! (* (v! 0.8 0.8 0.8)
               (- 0.7
                  (v:s~ (* bg-color
                           2) :y)))
            1)))) ; alpha for blending

(defpipeline-g ghost-block-pipeline ()
  (ghost-vert-stage :vec4)
  (ghost-frag-stage :vec4))


(defun-g combine-3-fbo-frag-stage ((v-pos :vec4)
                                   &uniform
                                   (org :sampler-2d)
                                   (1st :sampler-2d)
                                   (2nd :sampler-2d)
                                   (3th :sampler-2d))
  (let* ((cords (/ (+ (v:s~ v-pos :xy)
                      (v! 1 1))
                   2)) ;; normalize
         (v1 (texture 1st cords))
         (a1 (v:s~ v1 :w))
         (v2 (texture 2nd cords))
         (a2 (v:s~ v2 :w))
         (v3 (texture 3th cords))
         (a3 (v:s~ v3 :w)))
    (cond ((= a1 1) v1)
          ((= a2 1) v2)
          ((= A3 1) v3)
          (t (v! 0 0 0 0)))))

(defpipeline-g combine-3-fbo ()
  (ghost-vert-stage :vec4)
  (combine-3-fbo-frag-stage :vec4))


(defun-g vert-pass-fbo ((v-pos :vec4))
  (values v-pos
          v-pos))

(defun-g frag-pass-fbo ((v-pos :vec4)
                        &uniform
                        (fbo :sampler-2d))
  (let ((uv (/ (+ (v:s~ v-pos :xy)
                  (v! 1 1))
               2)))
    (texture fbo uv)))

(defpipeline-g print-fbo ()
  (vert-pass-fbo :vec4)
  (frag-pass-fbo :vec4))

(defun-g frag-combine ((v-pos :vec4)
                       &uniform
                       (sampler0 :sampler-2d)
                       (sampler1 :sampler-2d))
  (let ((uv (/ (+ (v:s~ v-pos :xy)
                  (v! 1 1))
               2)))
    (+ (texture sampler0 uv)
       (texture sampler1 uv))))

(defpipeline-g combine-2-fbo ()
  (vert-pass-fbo :vec4)
  (frag-combine :vec4))
;;;;;
(defun my-tr (x y z)
  "Translation function that puts stuff at the back"
  (v:+ (v! (v:+ (v! (+ -5 ;; -5 is center
                       (let* ((width (* (length player-functions:*players*)
                                        20))
                              (constant (/ width
                                           2)))
                         (+ (- constant)
                            (/ 20 2)
                            (* 20 (player-number *curr-player*))
                            )
                         ))
                    10) ;(v! -4.5 10)
                (v! x
                    (* -1 y)))
           (+ -10 z))
       (v!
        0
        0 0)))

;; ------    PLAYER INPUT


(defun advanced-repl ()
  "Handles user input"
  ;; LEFT
  (when (or (keyboard-button (keyboard) key.a)
            (keyboard-button (keyboard) key.left))
    (if *playing-multiplayer*
        (injected-controls :left)
        (tetris:left))
    (stepper-reset))
  ;; DOWN
  (when (or (keyboard-button (keyboard) key.s)
            (keyboard-button (keyboard) key.down))
    (if *playing-multiplayer*
        (injected-controls :down)
        (tetris:down))
    (stepper-reset))
  ;; RIGHT
  (when (or (keyboard-button (keyboard) key.d)
            (keyboard-button (keyboard) key.right))
    (if *playing-multiplayer*
        (injected-controls :right)
        (tetris:right))
    (stepper-reset)))

(defun rotate ()
  (format t "~%Rotating")
  (if *playing-multiplayer*
      (injected-controls :rotate)
      (tetris:rotate)))

(defun drop-down ()
  (if *playing-multiplayer*
      (injected-controls :drop-down)
      (tetris:drop-down)))

(defun music-on-off ()
  (sounds:music-on-off))

(defun play-background-music (&optional (what-song nil))
  "Plays next song, or one from the album."
  (format t "Song nr. ~a" what-song)
  (if what-song
      (sounds:play-song (1- what-song))
      (sounds:play-next-song)))

;;;;;;;;; --------  DRAWNINI


(defun draw ()
  (step-host)
  '(format t "~% ~{~a ~}" (loop for frame in *frames*
                           collect  (frame-is-free-p frame)
                             ))


  (let ((changed (not (equalp (resolution (current-viewport))
                              (surface-resolution (current-surface (cepl-context)))))))
    (setf (resolution (current-viewport))
          (surface-resolution (current-surface (cepl-context))))
    (when changed
      '(print (format nil "changed~a" (random 1230)))
      (make-frames)))
  (clear)
  (when (stepper-can-p)
    (advanced-repl))
  (with-frames (frame-global)
    (dolist (player player-functions:*players*)
      (progn
        (with-player (player-functions:init-player player)
          (draw-box 0
                    0
                    0
                    (get-color-v-for-block 'a))
          (let ((now (now)))
            (setf (animation-timer *render-state*) (+ (animation-timer *render-state*)
                                                      (- now (time-before-draw *render-state*)))
                  (time-before-draw *render-state*) (now)))
          (with-frames (background-fbo
                        shadow-fbo
                        background-and-shadow-fbo
                        pieces-and-next-pieces-fbo
                        current-piece-fbo ;; FIXME this and pieces-and-next-pieces-fbo
                        result-fbo)       ;; can bo one. ye?
            ;; draws wall to fbo
            (with-fbo-bound ((frame-fbo background-fbo))
              (draw-wall tetris:+width+  tetris:+height+
                         (animation-color *render-state*)
                         (animation-timer *render-state*)))

            ;; draws shadow block to fbo
            (when (curr-piece tetris:*game-state*)
              (with-fbo-bound ((frame-fbo shadow-fbo))
                (let* ((ghost-piece (tetris:get-current-ghost-piece))
                       (ghost-shape (piece-shape ghost-piece))
                       (ghost-col (piece-column ghost-piece))
                       (ghost-row (piece-row ghost-piece)))

                  (loop for row below (length (tetris:get-current-shape))
                     do (loop for column below (length (car (tetris:get-current-shape)))
                           for s = (tetris:symbol-at column
                                                     row
                                                     ghost-shape)
                           when (not (eql s '-))
                           do (draw-box (+ ghost-col column)
                                        (+ ghost-row row)
                                        0
                                        (get-color-v-for-block (tetris:get-current-color))))))))
            (with-fbo-bound ((frame-fbo background-and-shadow-fbo))
              ;; (textures shadow block + copies background) to fbo
              (map-g #'ghost-block-pipeline (get-quad-stream-v2)
                     :bg-sampler (frame-sampler background-fbo)
                     :block-sampler (frame-sampler shadow-fbo)))

            ;; draws current piece
            (with-fbo-bound ((frame-fbo current-piece-fbo))
              (loop for row below (length (tetris:get-current-shape))
                 do (loop for column below (length (car (tetris:get-current-shape)))
                       for s = (tetris:symbol-at column
                                                 row
                                                 (tetris:get-current-colored-shape))
                       when (not (eql s '-))
                       do (draw-box (+ (curr-column tetris:*game-state*) column)
                                    (+ (curr-row tetris:*game-state*) row)
                                    0
                                    (get-color-v-for-block s)))))

            ;; draw 2 next shapes + map to fbo
            (with-fbo-bound ((frame-fbo pieces-and-next-pieces-fbo))
              ;; draws 2 next pieces
              (let ((offset 1))
                (loop
                   for num below 2 ; this is limit
                   for shape in (mapcar
                                 (lambda (piece)
                                   (tetris:get-colored-shape piece))
                                 (tetris:get-next-pieces :limit 2))
                   ;; TODO maybe it might now work?
                   do (progn (loop for row below (length shape)
                                do (loop for column below (length (car shape))
                                      for s = (tetris:symbol-at column
                                                                row
                                                                shape)
                                      when (not (eql s '-))
                                      do (draw-box (+ tetris:+width+ column 1)
                                                   (+ row 1 offset )
                                                   0
                                                   (get-color-v-for-block s))))
                             (setf offset (+ 1 offset (length shape))))))
              ;; draw map to
              (loop for row below tetris:+height+
                 do (loop for column below tetris:+width+
                       for s = (tetris:symbol-at column
                                                 row
                                                 (game-map tetris:*game-state*))
                       ;; #TODO just create get-current-map etc.
                       if (eq s '-)
                       do      '(draw-box column row -1 (get-color-v-for-block s))
                       else do (progn
                                 '(draw-box column row -2 (get-color-v-for-block '-))
                                 (draw-box column row 0 (get-color-v-for-block s))))))
            (with-fbo-bound ((frame-fbo result-fbo))
              (map-g #'combine-3-fbo (get-quad-stream-v2)
                     :1st (frame-sampler current-piece-fbo)
                     :2nd (frame-sampler pieces-and-next-pieces-fbo)
                     :3th (frame-sampler background-and-shadow-fbo)))

            ;; combines global buffer with current tetris map,
            (with-fbo-bound ((frame-fbo result-fbo))
              (map-g #'combine-2-fbo (get-quad-stream-v2)
                     :sampler0 (frame-sampler frame-global)
                     :sampler1 (frame-sampler result-fbo)))
            ;; then
            ;; saves everything in global buffer with other maps.
            (with-fbo-bound ((frame-fbo frame-global))
              (map-g #'print-fbo (get-quad-stream-v2)
                     :fbo (frame-sampler result-fbo)))))))
    ;; draw global buffer
    (map-g #'print-fbo (get-quad-stream-v2)
           :fbo (frame-sampler frame-global)))

  (when (not *playing-multiplayer*)
    (player-functions:init-player *local-player*)) ;; used only in local
  (swap))


(defun draw-wall (width height color time)
  (map-g #'wall-pipeline (get-quad-stream-v2)
         :now (now)
         :width (+ width 2)
         :height (+ height 2)
         :perspective (rtg-math.projection:perspective
                       (x (resolution (current-viewport)))
                       (y (resolution (current-viewport)))
                       0.1
                       30f0
                       60f0)
         :cords (my-tr 0 0 -11.001)
         :multipler (calculate-multipler time)
         :color color
         ))

(defun calculate-multipler (time)
  (/ (- 2 (if (> time 2)
              2f0
              time))
     2f0))

(defun draw-box (column row depth color)
  (map-g #'some-pipeline *buf-stream*
         :now (now)
         :width 1
         :height 1
         :perspective (rtg-math.projection:perspective
                       (x (resolution (current-viewport)))
                       (y (resolution (current-viewport)))
                       0.1
                       30f0
                       60f0)
         :cords (my-tr column row depth)
         :block-color color
         ))

(defun set-background-animation-timer (color)
  (format t "~%Setting new animation: ~a" color)
  (format t "~%Playing sound~%")
  (sounds:play-hit-sound)
  (setf (animation-timer *render-state*) 0f0
        (animation-color *render-state*) (get-color-v-for-block color)))


(defun init ()


  (sounds:init-sound-system)
  #+nil (sounds:play-background-music)

  ;; init input
  (delete-event-listeners)
  (on-key-down key.space 'drop-down)
  (on-key-down key.return 'drop-down)
  (on-key-down key.r 'rotate)
  (on-key-down key.up 'rotate)
  (on-key-down key.m 'music-on-off)
  (on-key-down key.m 'sounds:play-hit-sound)
  ;;creates 'named lamdas' for music keys.
  ;;https://stackoverflow.com/questions/22975732/lisp-dynamically-define-functions
  (loop for (key num) in (list (list key.1 1)
                               (list key.2 2)
                               (list key.3 3)
                               (list key.4 4)
                               (list key.5 5)
                               (list key.6 6))
     do (let ((key-f-name (make-symbol (format nil "play-music-on-key-~a" num)))
              (number num))
          ;; create this function.
          (setf (fdefinition key-f-name)
                (lambda ()
                  (format t "num:~a" number)
                  (play-background-music number)))
          ;; link it.
          (on-key-down key key-f-name)))

  ;; cepl stuff. Binds buffer stream containing vertexes for 3d cube.
  (unless *buf-stream*
    (destructuring-bind (vert index)
        (nineveh.mesh.data.primitives:box-gpu-arrays)
      (setf *buf-stream*
            (make-buffer-stream vert :index-array index))))
  (make-frames))

(defun init-multiplayer ()
  (setf *playing-multiplayer* t)
  (init))

(defun init-singleplayer ()
  (setf *playing-multiplayer* nil)
  (init))

(defun make-frames ()
  (make-some-frames 21))


;;; ------------------------------- UNUSABLE OUTSIDE EMACS -----------------------------
#+nil (progn
        (def-simple-main-loop play (:on-start #'init-singleplayer)
          (draw))
        (def-simple-main-loop multi  (:on-start #'init-multiplayer)
          (draw)))
;;; -------------------------------------------------------------------------------------

(defun register-callbacks (player)
  (setf (multiplayer-p (player-game-state player)) *playing-multiplayer*)
  (setf (piece-touched (callbacks (player-game-state player))) 'set-background-animation-timer))


(defun main-start ()
  (cepl:repl)
  (bt:make-thread (lambda ()
                    (loop (draw)))))
(defun main ()
  #+nil (tetris:create-computer)
  (loop while (not (tetris-structures:game-over tetris:*game-state*))
     do (draw)))


;;;;; meta
(defun inject-controls (controls)
  (setf *controls* controls))

(defun injected-controls (control)
  (handler-case (funcall (second (assoc control *controls*)))
    (undefined-function (c)
      (declare (ignore c))
      (error "You haven't injected controls."))))



(defmethod player-functions:init-player ((player player))
  ;; there is :before.

  (setf *render-state* (player-render-state player)
        *curr-player* player)
  player)

(defun add-and-init-local-player () ;;unused
  (setf *local-player*
        (player-functions:init-player "local")))







#+ nil (handler-bind ((sb-kernel:simple-package-error (lambda (c)
                                                        (declare (ignore c))
                                                        (invoke-restart 'sb-ext:accept))))

         (ql:quickload :nineveh))

(defun cycle-players ())
