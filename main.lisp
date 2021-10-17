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

(defgeneric draw (obj)
  (:documentation "renders an object to the screen"))

(defgeneric update (obj dt)
  (:documentation "updates the position and stuff of an object in the game"))

(defclass player ()
  ((pos :initform (vec2 0 0) :accessor :pos)
   (acc :initform (vec2 0 0) :accessor :acc)
   (vel :initform (vec2 0 0) :accessor :vel)))

(defclass obstacle ()
  ((pos :initform (vec2 0 0) :accessor :pos)
   (bounds :initform (vec2 0 0) :accessor :bounds)))


(defmethod draw ((obj player))
  (draw-rectangle-lines
    (floor (vx (:pos obj)))
    (floor (vy (:pos obj)))
    20
    100
    +darkgreen+))

;; create our player
(defparameter *player* (make-instance 'player))

(defmethod update ((obj player) dt)
  (let ((idir (vec2 0 0)))
    (cond
      ((is-key-down +key-a+) (setf (vx idir) -1))
      ((is-key-down +key-d+) (setf (vx idir) 1)))
    (setf (:acc obj) (nv* idir +x-acc+ dt))
    (if (and (< (vlength (:acc obj)) 0.1) (> (vlength (:vel obj)) 0))
        (nv+ (:acc obj)
             (vec2 (* (vx (vscale (:vel obj) 1))
                      +friction+ dt)
                   0))))
  (nvlimit (nv+ (:vel obj) (:acc obj)) +max-speed+)
  (when (< (vlength (:vel obj)) 0.1)
      (setf (:vel obj) (vec2 0 0)))
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
