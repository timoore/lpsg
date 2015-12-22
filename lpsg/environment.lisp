;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

(defclass environment ()
  ((attribute-map :accessor attribute-map :initform nil :initarg :attribute-map
                  :documentation "list of (symbol glsl-name) where glsl-name is a string")
   (effect :accessor effect :initarg :effect :documentation "back pointer to effect object")
   (gl-state :accessor gl-state :initarg :gl-state)
   (uniform-sets :accessor uniform-sets :initarg :uniform-sets :initform nil)))

(defmethod gl-finalized-p ((obj environment))
  (gl-finalized-p (program (gl-state obj))))

(defmethod gl-finalize ((obj environment) &optional errorp)
  (gl-finalize (program (gl-state obj)) errorp))

(defun attribute-array-location (name environment)
  (let ((glsl-attrib-name (cadr (member name (attribute-map environment) :key #'car))))
    (unless glsl-attrib-name
      (return-from attribute-array-location nil))
    (let ((attrib (member glsl-attrib-name (vertex-attribs (program (gl-state environment)))
                         :key #'car :test #'string=)))
      (cadr attrib))))
