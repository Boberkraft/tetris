;; with this file you can play tetris.

;; You need to create a game-state (create-game-state)
;; and then (reinit-tetris /created-game-state/). Everyting is setten up and you
;; can use all of the non-meta functions like (down), (show-map) etc.


;; you can play with (simple-repl), or (simple-repl-with-computer).
;; The movement keys are:
;; - (a s d) ,
;; - r for rotating,
;; - space for droping down,
;; - q to quit.


(defpackage #:tetris
  (:use #:bt-semaphore
        #:cl
        #:tetris-structures)
  (:export :+width+
           :+height+
           :game-state
           :*game-state*
           :*callbacks*
           :*misc*
           :left
           :right
           :down
           :init-tetris
           :reinit-tetris
           :create-game-state-and-reinit
           :drop-down
           :get-piece
           :rotated-piece
           :create-game-state
           :get-current-colored-shape
           :get-ghost-piece
           :get-current-ghost-piece
           :get-current-color
           :get-colored-shape
           :rotate
           :create-computer
           :symbol-at
           :get-next-pieces
           :get-current-shape
           :game-map
           :create-map
           :new-player
           :with-game-state
           :with-player
           ))

#+nil (progn
        (declaim (optimize (debug 3)))
        (swank:set-default-directory "c:\\Users\\Bobi\\Desktop\\lispu\\projekty\\testing-rendering\\")
        (push #p"C:/Users/Bobi/Desktop/lispu/projekty/testing-rendering/" asdf:*central-registry*)
        (ql:quickload :tetris)
        (in-package #:tetris))

(in-package :tetris)

(defvar +width+ 10 "width of the map")
(defvar +height+ 20 "height of the map")

(defvar *pieces* nil "contains all of the available pieces")

(defparameter *game-state* nil "Instance of rendering-state. All functions are drawing from this thing")
(defparameter *callbacks* nil "Alias. See game-state")
(defparameter *misc* nil "Alias. See game-state")

(setf *pieces*
      (list (make-piece
             :name 'o
             :color 'a
             :shape '((x x)
                      (x x)))
            (make-piece
             :name 't
             :color 'b
             :shape '((x x x)
                      (- x -)))
            (make-piece
             :name 'j
             :color 'c
             :shape '((- x)
                      (- x)
                      (x x)))
            (make-piece
             :name 'z-inf
             :color 'd
             :shape '((- x x)
                      (x x -)))
            (make-piece
             :name 'l
             :color 'e
             :shape '((x -)
                      (x -)
                      (x x)))
            (make-piece
             :name 'o
             :color 'f
             :shape '((x x -)
                      (- x x)))
            (make-piece
             :name 'i
             :color 'g
             :shape '((x -)
                      (x -)
                      (x -)
                      (x -)))))


(defun create-map ()
  (loop for rows below +height+
     collect (loop for column below +width+
                collect '-)))



(defun get-current-shape ()
  (when (curr-piece *game-state*)
    (piece-shape (curr-piece *game-state*))))

(defun get-current-colored-shape ()
  "Returns colored shape or nil"
  (when (curr-piece *game-state*)
    (get-colored-shape (curr-piece *game-state*))))

(defmethod get-colored-shape ((piece piece))
  "returns 'x replaces with color. Not in place!"
  (subst (piece-color piece) 'x (piece-shape piece)))

(defun get-next-pieces (&key (limit nil))
  (if limit
      (subseq (next-pieces *game-state*) 0 limit)
      (next-pieces *game-state*)))

(defmethod get-piece ((s symbol))
  "By name"
  (find-if (lambda (piece)
             (eql (piece-name piece)
                  s))
           *pieces*))

(defmethod get-piece ((num number))
  "By number"
  (nth num *pieces*))

(defun get-current-piece ()
  (curr-piece *game-state*))

(defun get-current-color ()
  (piece-color (curr-piece *game-state*)))

(defun get-random-piece-number ()
  (let* ((num-of-pieces (length *pieces*))
         (choice (random num-of-pieces)))
    choice))

(defun get-random-piece ()
  (get-piece (get-random-piece-number)))

(defun populate-next-pieces ()
  (setf (next-pieces *game-state*)
        (loop for x below 5
           collect  (get-random-piece))))




(defun get-current-ghost-piece ()
  "Returns piece with set column and row slots that's are cords for ghost piece's shape
   or nil"
  (let ((ghost-shape (piece-shape (curr-piece *game-state*)))
        (ghost-piece (copy-piece (curr-piece *game-state*))) ;; Cords to be changed
        (curr-row (curr-row *game-state*))
        (curr-column (curr-column *game-state*)))
    (loop for row from curr-row to +height+ ; goes down
       when (or (will-shape-touch-others ghost-shape curr-column row)
                (will-shape-touch-floor ghost-shape curr-column row))
       do (return-from get-current-ghost-piece
            (progn 
              (setf (piece-column ghost-piece) curr-column
                    (piece-row ghost-piece) (1- row))
              ghost-piece)))
    nil))

;; '((- x -)(x x x)(x x x)) => '((x x -)(x x x)(x x -))
;;
;;    - x -        x x -
;;    x x x   ->   x x x
;;    x x x        x x -
(defmethod rotated-piece ((piece piece))
  "Returns new rotated piece"
  (let ((copy (copy-piece piece)))
    (setf (piece-shape copy)
          (mapcar 'reverse
                  (apply #'mapcar #'list (piece-shape copy))))
    (setf (piece-rotation copy)
          (mod (piece-rotation copy) 4)) ;; Rotating by 4 is the same as 0
    copy))

(defmethod rotated-piece ((piece null))
  nil)

(defun rotate-current-piece ()
  "Rotates curren piece. Does nothing when nil"
  (when (null (curr-piece *game-state*))
    (return-from rotate-current-piece nil))
  (let* ((rotated-piece (rotated-piece (curr-piece *game-state*)))
         (shape (piece-shape rotated-piece)))
    (unless (or (will-shape-touch-others shape (curr-column *game-state*) (curr-row *game-state*))
                (will-shape-touch-floor shape (curr-column *game-state*) (curr-row *game-state*))
                (will-shape-touch-walls shape (curr-column *game-state*) (curr-row *game-state*)))
      (setf (curr-piece *game-state*)
            rotated-piece))))


(defmacro symbol-at (col row 2d-list &optional check)
  "Checks if col and row are out of bouds. If check is false, then this think can be used for
  (setf (symbol-at [...]) something)"
  (if check
      `(if (or (< ,col 0)
               (< ,row 0)
               (<= (length ,2d-list), row)
               (<= (length (car ,2d-list)) ,col))
           '- ;; out of bouts
           (nth ,col
                (nth ,row ,2d-list)))
      `(nth ,col
            (nth ,row ,2d-list))))

(defun will-shape-touch-others (shape start-col start-row)
  (loop for row below (length shape)
     do (loop for column below (length (car shape))
           for shape-symbol-at = (symbol-at column
                                            row
                                            shape t)
           for map-symbol-at = (symbol-at (+ column start-col)
                                          (+ row start-row)
                                          (game-map *game-state*) t)
           when (and (not (eql shape-symbol-at '-))
                     (not (eql map-symbol-at '-)))
           do (progn
                #+nil(format t "~a ~a" shape-symbol-at map-symbol-at)
                (return-from will-shape-touch-others t))))
  nil)


(defun will-shape-touch-floor (shape start-col start-row)
  (declare (ignore start-col))
  (loop for row from (1- (length shape)) downto 0
     when (find-if (lambda (symbol-at) (not (eql '- symbol-at)))
                   (nth row shape)) ; block starts on this row
     do (return-from will-shape-touch-floor
          (<= +height+ (+ start-row row)))))

(defun will-shape-touch-walls (shape start-col start-row)
  (declare (ignore start-row))
  ;; find x to se where to shape starts
  ;; if nil is not NUMBEr then its because tha shapes has no pieces
  (labels ((find-first-x (shape)
             (loop for column below (length (car shape))
                do (loop for row below (length shape)
                      for shape-symbol = (symbol-at column
                                                    row
                                                    shape)
                      if (not (eq shape-symbol '-))
                      do (return-from find-first-x column)))))
    (let ((x-start-left (find-first-x shape))
          (width  (- (length (car shape))
                     (find-first-x (mapcar 'reverse shape)))))
      (if (or (> 0 (+ start-col x-start-left)) ;left site
              (<  +width+ (+ start-col width)) ;right site
              )
          t
          nil)))) ;; yes i know



(defun add-shape-to-map (shape start-col start-row)
  (loop for row below (length shape)
     do (loop for column below (length (car shape))
           for shape-symbol = (symbol-at column
                                         row
                                         shape)
           when (and (not (eql shape-symbol '-)) ;; not empty
                     ;; checks whatever symbol is stil inbouds of map
                     (not (null (symbol-at (+ start-col column)
                                           (+ start-row row)
                                           (game-map *game-state*) t))))
           do (setf (symbol-at (+ start-col column)
                               (+ start-row row)
                               (game-map *game-state*))
                    shape-symbol))))


(defun remove-shape-from-map (shape start-col start-row)
  (loop for row below (length shape)
     do (loop for column below (length (car shape))
           for shape-symbol = (symbol-at column
                                         row
                                         shape)
           when (not (eq shape-symbol '-)) ;; eq. 'x. Byp
           do (setf (symbol-at (+ start-col column)
                               (+ start-row row)
                               (game-map *game-state*))
                    '-))))

(defun add-new-next-piece (piece)
  "Puts new piece at the end of queue, removes first piece"
  (setf (next-pieces *game-state*) (append (rest (next-pieces  *game-state*))
                                           (list piece))))

(defun generate-new-piece ()
  (format t "~%Setting new piece.")
  ;; sets piece
  (setf (curr-piece *game-state*) (first (next-pieces *game-state*)))
  ;; center
  (setf (curr-column *game-state*) (floor (/ +width+ 2))
        (curr-row *game-state*) 0)
  ;; removes piece and generates new
  (let* ((new-piece (get-random-piece)))
    (format t "~%Generating new piece.")
    (add-new-next-piece new-piece)))

(defun remove-current-piece-from-map ()
  (remove-shape-from-map (get-current-colored-shape)
                         (curr-column *game-state*)
                         (curr-row *game-state*)
                         ))

(defun add-current-piece-to-map ()
  (add-shape-to-map (get-current-colored-shape)
                    (curr-column *game-state*)
                    (curr-row *game-state*)))


(defun test-number-of-complete-lines ()
  (add-shape-to-map '((x x x x x x x x x x x)) 0 8)
  (add-shape-to-map '((x x x x x x x x x x x)) 0 9)
  (show-map)
  (format t "~%number of complete lines: ~a " (number-of-complete-lines))
  '(assert (equal (list 8 9) (number-of-complete-lines))))

(defun number-of-complete-lines ()
  "returns number of made lines, counting from the bottom"
  ;; reverse the map and return the position of the first row that contains symbol -
  (length (get-complete-lines)))

(defun get-complete-lines ()
  "Returns list containing complete lines"
  (let ((lines nil)
        (line-num 0))
    (mapc (lambda (row)
            (when (every (lambda (symbol) (not (eq symbol '-)))
                         row)
              (push line-num lines))
            (incf line-num))
          (game-map *game-state*))
    lines))

(defun hilight-complete-lines ()
  "Sets new map with hilighted lines. Replaces all not '- with a '* "
  (let* ((lines (get-complete-lines))
         (hilighted-line (loop for x below +width+ collect '*)))
    (mapc (lambda (row-num)
            (setf (nth row-num (game-map *game-state*))
                  hilighted-line))
          lines)
    lines))

(defun remove-complete-lines ()
  "Sets new map wothout completed lines. Removes all not '- in a line"
  (format t "~%Removing lines")
  (let* ((lines (get-complete-lines)))
    (setf (game-map *game-state*) (append (loop ;add empty lines on top
                                             for y below (length lines) collect (loop for x below +width+ collect '-))
                                          (loop ;collect not empty lines
                                             for row below +height+
                                             when (not (find row lines))
                                             collect (nth row (game-map *game-state*)))))))

(defun move-shape (delta-column delta-row)
  (let ((new-column (+ delta-column (curr-column *game-state*)))
        (new-row (+ delta-row (curr-row *game-state*))))
    (cond ((or (will-shape-touch-others (piece-shape (curr-piece *game-state*))
                                        new-column new-row)
               (will-shape-touch-floor  (piece-shape (curr-piece *game-state*))
                                        new-column new-row))
           ;;; restart piece --
           (format t "~%Floor or piece touched!")
           (add-current-piece-to-map) ; put piece on the spot
           (funcall (piece-touched *callbacks*) ; a cool animation
                    (piece-color (curr-piece *game-state*)))
           (setf (curr-piece *game-state*) nil) ;to gen new piece

           )
          ;;; do nothing --
          ((will-shape-touch-walls (piece-shape (curr-piece *game-state*)) new-column new-row)
           (format t "~%Wall touched!")
           )
          ;;; move down --
          (t (format t "~%Moving shape [X: ~a] [Y: ~a]" delta-column delta-row)
             (setf (curr-row *game-state*) new-row)
             (setf (curr-column *game-state*) new-column)))
    (when (get-complete-lines)
      (push '(lambda ()
              (remove-complete-lines)) (events *game-state*)) ;; execute later
      (hilight-complete-lines)
      (format t "~%Hilighting completed lines")
      (funcall (piece-touched *callbacks*) ; a cool animation 
               '*))))

(defun execute-all-events ()
  (mapc (lambda (event) (funcall (eval event)))
        (events *game-state*))
  (setf (events *game-state*) nil))

(defun left ()
  (game-tick -1 0))
(defun right ()
  (game-tick +1 0))
(defun down ()
  (game-tick 0 1))
(defun up ()
  (game-tick 0 -1))
(defun stop ()
  (game-tick 0 0))
(defun drop-down ()
  (format t "~%Dropping down")
  (loop while (curr-piece *game-state*)
     do (game-tick 0 1)))

(defun rotate ()
  (rotate-current-piece)
  (game-tick 0 0))

(defun increase-difficulty ()
  (setf (difficulty *game-state*)
        (* (difficulty *game-state*)
           0.999)))

(defun game-tick (d-x d-y)
  (bt:with-lock-held ((lock *misc*))
    (execute-all-events)
    (when (null (curr-piece *game-state*))
      (generate-new-piece)
      (when (will-shape-touch-others (piece-shape (curr-piece *game-state*))
                                     (curr-column *game-state*)
                                     (curr-row *game-state*))
        (game-over-screen)
        (init-tetris)
        (return-from game-tick)))
    (move-shape d-x d-y)))

(defun computer-loop ()
  (loop while (not (game-over *game-state*))
     do (progn (sleep (difficulty *game-state*))
               (increase-difficulty)
               (down)
               (show-map))))

(defun create-computer ()
  (bt:make-thread #'computer-loop))

(defun simple-repl-with-computer ()
  (create-computer)
  (simple-repl))

(defun simple-repl ()
  (loop
     ;;(format t " A - LEFT ~% S - DOWN ~% D - RIGHT~% Input: ")
     (case (read-char)
       (#\a (left))
       (#\s (down))
       (#\d (right))
       (#\r (rotate))
       (#\Space (drop-down))
       (#\q (return-from simple-repl) (setf (game-over *game-state*) t)))
     (show-map)))

(defun show (2d-list)
  (labels ((slashes-to-spaces (lst)
             (mapcar (lambda (l) (substitute #\SPACE '- l)) lst)))
    (format *standard-output* "~%|~{~a ~}|" (loop for i below +width+  collect #\=))
    (format *standard-output* "~{~%|~{~a ~}|~}" (slashes-to-spaces 2d-list))
    (format *standard-output* "~%|~{~a ~}|" (loop for i below +width+  collect #\=)))
  )


(defun show-map ()
  (format t "~%Current shape:")
  (if (curr-piece *game-state*)
      (show (get-current-colored-shape))
      (format t "NIL"))
  (format t "~%[X: ~2a] [Y: ~2a]"
          (curr-column *game-state*)
          (curr-row *game-state*))
  (add-current-piece-to-map)
  (show (game-map *game-state*))
  (remove-current-piece-from-map))

(defun game-over-screen ()
  (setf (game-over *game-state*) t)
  (format t "~%GAME OVER!")
  ;; TODO gamover callback?
  (setf (game-map *game-state*)
        (create-map))
  )



'(simple-repl)

'(init-tetris)


;;;---------------------  META STUFF

(defun create-game-state ()
  "Creates new game-state and returns it."
  (let ((game-state *game-state*)
        (was-before *game-state*)) ; if *game-state* is nil. 

    ;;set new
    (reinit-tetris (make-instance 'game-state))
    (setf (game-map *game-state*) (create-map)) ;; TODO: it shound be in lisp-structures
    (populate-next-pieces)                      ;; <--- (this)  but i dont know how to do it.
    ;;reinit back

    (prog1
        *game-state*
      (when was-before
        ;; this if is for, when game-state is  nil.
        (reinit-tetris game-state)))))

(defun reinit-tetris (game-state)
  "Reinits variables to point at this game-state. Returns game-state"
  (setf *game-state* game-state
        *callbacks* (callbacks *game-state*)
        *misc* (misc *game-state*))
  game-state)

(defmacro with-game-state (game-state &body body)
  "Reinits variables to point at this game-state. Dynamic scope"
  `(let ((*game-state* ,game-state)
         (*callbacks* (callbacks *game-state*))
         (*misc* (misc *game-state*)))
     ,@body))

(defmacro with-player (player &body body)
  "Reinits variables to point at this player. Dynamic scope"
  `(with-game-state (player-game-state ,player)
     ,@body))



;; very cool code!
;; i think it implements mapcan.
;; (reduce (lambda (list-a new-one)
;;           (mapcar (lambda (a b) (append (if (atom b)
;;                                          (list b)
;;                                          b)
;;                                      (if (atom a)
;;                                          (list a)
;;                                          a)))
;;                     list-a new-one))
;;           '((- x -)(x x x)(x x x)))
