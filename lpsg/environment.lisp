;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

(defclass environment ()
  ((shader-program :accessor shader-program :initarg :shader-program)
   (attribute-map :accessor attribute-map :initform nil
                  :documentation "list of (symbol glsl-name) where glsl-name is a string")))

(defmethod gl-finalized-p ((obj environment))
  (gl-finalized-p (shader-program obj)))

(defmethod gl-finalize ((obj environment) &optional errorp)
  (gl-finalize (shader-program obj) errorp))

(defun attribute-array-location (name environment)
  (let ((glsl-attrib-name (cadr (member name (attribute-map environment) :key #'car))))
    (unless glsl-attrib-name
      return nil)
    (let ((attrib (member glsl-attrib-name (vertex-attribs (shader-program environment))
                         :key #'car :test #'string=)))
      (cadr attrib))))
