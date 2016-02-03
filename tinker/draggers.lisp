;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;

(in-package #:lpsg-tinker)

;;; Classes for calculating displacement and rotation during a user mouse-drag event.

(defclass translate-dragger ()
  ((start-mouse-point :initarg :start-mouse-point)
   (viewport :initarg :viewport)         ; (vec4 left bottom width height)
   (perspective-matrix :initarg :perspective-matrix)
   (model-matrix :initarg :model-matrix)))

(defmethod initialize-instance :after ((obj translate-dragger) &key start-mouse-point)
  (setf (slot-value obj 'start-mouse-point) (vec3 start-mouse-point)))

(defgeneric current-displacement (dragger new-mouse))

(defmethod current-displacement ((dragger translate-dragger) new-mouse)
  (with-slots (start-mouse-point viewport perspective-matrix model-matrix)
      dragger
    (let* ((start-mouse-world
            (unproject start-mouse-point model-matrix perspective-matrix viewport))
           (new-mouse-point (vec3 new-mouse 0.0))
           (new-mouse-world (unproject new-mouse-point model-matrix perspective-matrix viewport)))
      (vec- new-mouse-world start-mouse-world))))
