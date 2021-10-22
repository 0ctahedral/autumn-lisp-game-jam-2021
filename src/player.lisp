(cl:in-package :froggy)

(defclass player ()
  ((body :initform nil)
   (shape :initform nil)
   ))

(defparameter *player-shape-vertices* (list (gamekit:vec2 -50 50)
                                            (gamekit:vec2 50 50)
                                            (gamekit:vec2 50 -50)
                                            (gamekit:vec2 -50 -50))) 

(defmethod initialize-instance :after ((this player) &key universe)
  (identity))
;;(defmethod initialize-instance :after ((this player) &key universe)
;;  (with-slots (body shape) this
;;    (setf body (make-rigid-body universe)
;;          shape (make-polygon-shape universe
;;                                    *player-shape-vertices*
;;                                    :body body
;;                                    :substance this))))

(defun make-player (universe)
  (make-instance 'player :universe universe))

(defmethod render ((this player))
  (draw-polygon *player-shape-vertices*
                :fill-paint (vec4 0.8 0.8 0.8 1)))
