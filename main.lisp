(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib :bt :3d-vectors))

(in-package :raylib-user)

;; are we debugging
(defparameter *debug* t)

(defparameter *screen-height* 1080) 
(defparameter *screen-width* 1920)

;; deceleration due to friction
(defparameter +friction+ -15)
;; deceleration due to friction when slidding
(defparameter +slide-friction+ -10)
;; maximum speed a player can move at
(defparameter +max-speed+ 10)
;; horizontal acceleration of player
(defparameter +x-acc+ 15)
;; vertical acceleration of player (gravity)
(defparameter +y-acc+ -15)

(defgeneric draw (obj)
  (:documentation "renders an object to the screen"))

(defgeneric update (obj dt)
  (:documentation "updates the position and stuff of an object in the game"))

(defclass player ()
  ((pos :initform (vec2 0 100) :accessor :pos)
   (bounds :initform (vec2 20 50) :accessor :bounds)
   (acc :initform (vec2 0 0) :accessor :acc)
   (vel :initform (vec2 0 0) :accessor :vel)))

(defclass obstacle ()
  ((pos :initarg :pos :initform (vec2 0 0) :accessor :pos)
   (bounds :initarg :bounds :initform (vec2 20 50) :accessor :bounds)))


(defmethod draw ((obj player))
  (draw-rectangle
    (floor (vx (:pos obj)))
    (floor (- *screen-height* (vy (:pos obj))))
    (floor (vx (:bounds obj)))
    (floor (vy (:bounds obj)))
    +darkgreen+))

(defmethod draw ((obj obstacle))
  (draw-rectangle
    (floor (vx (:pos obj)))
    (floor (- *screen-height* (vy (:pos obj))))
    (floor (vx (:bounds obj)))
    (floor (vy (:bounds obj)))
    +gray+))

;; get the collisions between an object and a list of
;; other objects
(defun get-collisions (obj lst)
  (mapcar #'(lambda (o)
        (collision-rect obj o)) lst))

(defgeneric collision-rect (p ob)
  (:documentation "get a rectangle of the collision of two objects"))

(defmethod collision-rect ((p player) (ob obstacle))
  (let ((r (get-collision-rec
             (make-rectangle 
               :x (floor (vx (:pos p)))
               :y (floor (- *screen-height* (vy (:pos p))))
               :width (floor (vx (:bounds p)))
               :height (floor (vy (:bounds p))))
             (make-rectangle 
               :x (floor (vx (:pos ob)))
               :y (floor (floor (- *screen-height* (vy (:pos ob)))))
               :width (floor (vx (:bounds ob)))
               :height (floor (vy (:bounds ob)))))))
    (if (and (> (rectangle-width r) 0) (> (rectangle-height r) 0))
        r
        nil)))

;; create our player
(defparameter *player* (make-instance 'player))
;; all collision shapes
(defparameter *obstacles*
  ;; floor
  (list (make-instance 'obstacle
           :pos (vec2 -30 100) 
           :bounds (vec2 2000 100) )
        (make-instance 'obstacle
           :pos (vec2 200 200) 
           :bounds (vec2 100 30))))

;; sort collisions by direction
;;(defun collision-dir (p r)
;;  (let* ((px (vx (:pos p)))
;;         (py (vy (:pos p)))
;;         (px1 (+ px (vx (:bounds p))))
;;         (py1 (+ py (yx (:bounds p))))
;;         (cx (rectangle-x r))
;;         (cy (rectangle-y r))
;;         (cx1 (+ cx (rectangle-width r)))
;;         (cy1 (+ cy (rectangle-height r))))
;;    ;; get vectors hehe
;;  )

(defmethod update ((obj player) dt)
  ;; gather inputs
  (let* ((idir (vec2 0 0))
        (collisions (get-collisions *player* *obstacles*))
        (grounded (car collisions)))
    (if grounded
        (if (or (is-key-down +key-space+) (is-key-down +key-w+)) 
            (setf (vy (:vel obj)) 10)
            (setf (vy (:vel obj)) 0))
        (setf (vy idir) 1))
    (cond
      ((is-key-down +key-a+) (setf (vx idir) -1))
      ((is-key-down +key-d+) (setf (vx idir) 1)))
    (setf (:acc obj) (nv* idir (vec2 +x-acc+ +y-acc+) dt))
    ;; should we add friction?
    (if (and grounded (< (vlength (:acc obj)) 0.1) (> (vlength (:vel obj)) 0))
        (nv+ (:acc obj)
             (vec2 (* (vx (vscale (:vel obj) 1))
                      +friction+ dt)
                   0))))
  ;; limit the horizontal speed
  (nvlimit (nv+ (:vel obj) (:acc obj)) +max-speed+)
  ;; floor velocity if close to zero
  (when (< (vlength (:vel obj)) 0.1)
      (setf (:vel obj) (vec2 0 0)))
  ;; update position
  (nv+ (:pos obj) (:vel obj)))

(defun draw-debug ()
  (let ((mpos (get-mouse-position))
        (col (get-collisions *player* *obstacles*)))
    (mapcar #'(lambda (r)
                (if r
                    (draw-rectangle-rec r +red+)
                    nil)) col)
    (draw-text (format nil "mpos ~a ~a" (vector2-x mpos) (- *screen-height* (vector2-y mpos)))
               20 10 20 +lightgray+))
    (draw-text (format nil "pos ~a ~a" (vx (:pos *player*)) (vy (:pos *player*)))
               20 30 20 +lightgray+)
    (draw-text (format nil "vel: ~a ~a" (vx (:vel *player*)) (vy (:vel *player*)))
               20 50 20 +lightgray+)
    (draw-text (format nil "acc: ~a ~a" (vx (:acc *player*)) (vy (:acc *player*)))
               20 70 20 +lightgray+))

(defun game-loop ()
  (update *player* (get-frame-time))
  (with-drawing
    (clear-background +raywhite+)
    (mapcar #'draw *obstacles*)
    (draw *player*)
    (when *debug*
      (draw-debug))
    ))

(defun main ()
  (with-window (*screen-width* *screen-height* "mjr game")
      (set-target-fps 60) ; Set our game to run at 60 FPS
      (loop
        (if (window-should-close) (return))
        (game-loop))))

(if *debug*
    (bt:make-thread
      (lambda ()
        (main)))
    (main))
