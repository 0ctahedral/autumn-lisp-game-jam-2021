(require :cl-raylib)

(defpackage :raylib-user
 (:use :cl :raylib))

(in-package :raylib-user)

(defparameter *player-pos* (make-vector2 :x 100 :y 100))
(defparameter *player-size* (make-vector2 :x 30 :y 100))

(defun draw-player ()
  (draw-rectangle-lines
      (vector2-x *player-pos*)
      (vector2-y *player-pos*)
      (vector2-x *player-size*)
      (vector2-y *player-size*)
      +darkgreen+))

(defun game-loop ()
  (with-drawing
    
    (clear-background +raywhite+)
    (draw-fps 20 20)
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
