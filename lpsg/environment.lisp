;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

;;; The inputs of an ENVIRONMENT are the usets. The INVALIDATE-COMPUTATION/COMPUTE protocol is used
;;; to update them. The usets are also stored seperately for simplicity (or no good reason...)

(defclass environment ()
  ((attribute-map :accessor attribute-map :initform nil :initarg :attribute-map
                  :documentation "list of (symbol glsl-name) where glsl-name is a string")
   (effect :accessor effect :initarg :effect :documentation "back pointer to effect object")
   (gl-state :accessor gl-state :initarg :gl-state
             :documentation "the state to apply when rendering")
   (uniform-sets :accessor uniform-sets :initarg :uniform-sets :initform nil
                 :documentation "uset values (from inputs) used to update the environment's shader
uniforms")
   (visiblep :accessor visiblep :initarg :visiblep :initform t
             :documentation "Controls the visibility of the associated bundle.")
   (renderer :accessor renderer :initarg :renderer :documentation "the renderer"))
  (:documentation "class that controls rendering of shapes."))

(defmethod gl-finalized-p ((obj environment))
  (gl-finalized-p (gl-state obj)))

(defmethod gl-finalize ((obj environment) &optional errorp)
  (gl-finalize (gl-state obj) errorp))

(defun attribute-array-location (name environment)
  (let ((glsl-attrib-name (cadr (member name (attribute-map environment) :key #'car))))
    (unless glsl-attrib-name
      (return-from attribute-array-location nil))
    (let ((attrib (member glsl-attrib-name (vertex-attribs (glstate-program (gl-state environment)))
                         :key #'car :test #'string=)))
      (cadr attrib))))


