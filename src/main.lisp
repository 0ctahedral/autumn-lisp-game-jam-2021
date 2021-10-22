(cl:in-package :froggy)

;; this will go into package stuff
(defvar *screen-height* 1080) 
(defvar *screen-width* 1920)
(defvar *origin* (vec2 0 0))
(defparameter *background-color* (vec4 0.9 0.9 0.9 1.0))

(defgeneric render (object))

(gamekit:defgame froggy ()
  ((universe :initform (make-universe :2d))
   (body :initform nil))
  (:viewport-width *screen-width*)
  (:viewport-height *screen-height*)
  (:viewport-title "froggy wit da glocky"))

(defmethod gamekit:draw ((app froggy))
  ;; background
  (draw-rect *origin* *screen-width* *screen-height*
             :fill-paint *background-color*)
  (with-pushed-canvas ()
    (translate-canvas 300 500)
    (rotate-canvas (/ pi 4))
    (draw-polygon *player-shape-vertices*
                  :stroke-paint (vec4 0.0 1.0 0.0 1)
                  :thickness 3.0)  )
  )

(defmethod post-initialize ((this froggy))
  (with-slots (universe body) this
    (setf body (make-rigid-body universe))
    (setf (gravity universe) (vec2 0 -10))))

(defmethod act ((this froggy))
  (with-slots (universe body) this
    (observe-universe universe 0.10)
    (format t "~&Body position: ~A" (body-position body))))
