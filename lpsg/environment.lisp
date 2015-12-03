;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

(defclass environment (graphics-state)
  ((attribute-map :accessor attribute-map :initform nil :initarg :attribute-map
                  :documentation "list of (symbol glsl-name) where glsl-name is a string")
   (effect :accessor effect :initarg :effect :documentation "back pointer to effect object")))

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
