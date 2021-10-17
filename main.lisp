(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib :bt :3d-vectors))

(in-package :raylib-user)

;; are we debugging
(defparameter *debug* t)
;; deceleration due to friction
(defparameter +friction+ -15)
;; deceleration due to friction when slidding
(defparameter +slide-friction+ -10)
;; maximum speed a player can move at
(defparameter +max-speed+ 10)
;; horizontal acceleration of player
(defparameter +x-acc+ 15)
;; vertical acceleration of player (gravity)
(defparameter +y-acc+ 15)

(defgeneric draw (obj)
  (:documentation "renders an object to the screen"))

(defgeneric update (obj dt)
  (:documentation "updates the position and stuff of an object in the game"))

(defclass player ()
  ((pos :initform (vec2 0 850) :accessor :pos)
   (bounds :initform (vec2 20 50) :accessor :bounds)
   (acc :initform (vec2 0 0) :accessor :acc)
   (vel :initform (vec2 0 0) :accessor :vel)))

(defclass obstacle ()
  ((rect :initarg :rect
         :initform (make-rectangle
                     :x 0 :y 0
                     :width 0 :height 0)
         :accessor :rect)))


(defmethod draw ((obj player))
  (draw-rectangle-lines
    (floor (vx (:pos obj)))
    (floor (vy (:pos obj)))
    (floor (vx (:bounds obj)))
    (floor (vy (:bounds obj)))
    +darkgreen+))

(defgeneric collisionp (p ob)
  (:documentation "check for collision between a player and an obstacle"))
(defmethod collisionp ((p player) (ob obstacle))
  (check-collision-recs
    (make-rectangle 
      :x (floor (vx (:pos *player*)))
      :y (floor (vy (:pos *player*)))
      :width (floor (vx (:bounds *player*)))
      :height (floor (vy (:bounds *player*))))
    (:rect (car *obstacles*))))

(defmethod draw ((obj obstacle))
  (draw-rectangle-rec (:rect obj) +gray+))

;; create our player
(defparameter *player* (make-instance 'player))
;; all collision shapes
(defparameter *obstacles*
  `(,(make-instance 'obstacle
         :rect (make-rectangle
           :x 0 :y 980
           :width 1920 :height 100))))


(defmethod update ((obj player) dt)
  ;; gather inputs
  (let ((idir (vec2 0 0))
        (grounded (collisionp obj (car *obstacles*))))
    (if grounded
        (if (is-key-down +key-w+)
            (setf (vy (:vel obj)) -10)
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
    (draw-fps 20 20)
    (draw-text (format nil "vel: ~a ~a" (vx (:vel *player*)) (vy (:vel *player*)))
               20 50 20 +lightgray+)
    (draw-text (format nil "acc: ~a ~a" (vx (:acc *player*)) (vy (:acc *player*)))
               20 70 20 +lightgray+))

(defun game-loop ()
  (update *player* (get-frame-time))
  (with-drawing
    (when *debug*
      (draw-debug))
    (clear-background +raywhite+)
    (mapcar #'draw *obstacles*)
    (draw *player*)))

(defun main ()
  (let ((screen-width 1920)
        (screen-height 1080))
    (with-window (screen-width screen-height "mjr game")
      (set-target-fps 60) ; Set our game to run at 60 FPS
      (loop
        (if (window-should-close) (return))
        (game-loop)))))

(if *debug*
    (bt:make-thread
      (lambda ()
        (main)))
    (main))
