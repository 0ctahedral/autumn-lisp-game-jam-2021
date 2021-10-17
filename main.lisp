(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib :3d-vectors))

(in-package :raylib-user)

(defclass player ()
  ((pos :initform (vec2 0 0) :accessor :pos)
   (acc :initform (vec2 0 0) :accessor :acc)
   (vel :initform (vec2 0 0) :accessor :vel)))

;; create our player
(defparameter *player* (make-instance 'player))

(defun update-player ()
  (let ((idir (vec2 0 0)))
    (cond
      ((is-key-down +key-a+) (setf (vx idir) -2))
      ((is-key-down +key-d+) (setf (vx idir) 2)))
    ;; TODO: if grounded then we want to add friction
    (setf (:acc *player*) idir)
    ;; friction
    (if (> (vlength (:vel *player*)) 0)
        (nv- (:acc *player*) (nvunit (vec2 (vx (:vel *player*)) 0)))))
  (nvlimit (nv+ (:vel *player*) (:acc *player*)) 10)
  (nv+ (:pos *player*) (:vel *player*)))

(defun draw-player ()
  (draw-rectangle-lines
    (rational (vx (:pos *player*)))
    (rational (vy (:pos *player*)))
    20
    100
    +darkgreen+))

(defun game-loop ()
  (update-player)
  (with-drawing
    (clear-background +raywhite+)
    (draw-fps 20 20)
    (draw-text (format nil "axis0: ~a" (get-gamepad-axis-movement 1 0))
               20 50 20 +lightgray+)
    (draw-player)))

(defun main ()
  (let ((screen-width 800)
        (screen-height 450))
    (with-window (screen-width screen-height "mjr game")
      (set-target-fps 60) ; Set our game to run at 60 FPS
      (loop
        (if (window-should-close) (return))
        (game-loop)))))

(main)

