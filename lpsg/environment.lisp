;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

;;; The inputs of an ENVIRONMENT are the usets. The INVALIDATE-COMPUTATION/COMPUTE protocol is used
;;; to update them. The usets are also stored seperately for simplicity (or no good reason...)

(defclass environment (consumer-node)
  ((attribute-map :accessor attribute-map :initform nil :initarg :attribute-map
                  :documentation "list of (symbol glsl-name) where glsl-name is a string")
   (effect :accessor effect :initarg :effect :documentation "back pointer to effect object")
   (gl-state :accessor gl-state :initarg :gl-state)
   (uniform-sets :accessor uniform-sets :initarg :uniform-sets :initform nil)
   (renderer :accessor renderer :initarg :renderer)))

(defmethod gl-finalized-p ((obj environment))
  (gl-finalized-p (gl-state obj)))

(defmethod gl-finalize ((obj environment) &optional errorp)
  (gl-finalize (gl-state obj) errorp))

(defun attribute-array-location (name environment)
  (let ((glsl-attrib-name (cadr (member name (attribute-map environment) :key #'car))))
    (unless glsl-attrib-name
      (return-from attribute-array-location nil))
    (let ((attrib (member glsl-attrib-name (vertex-attribs (program (gl-state environment)))
                         :key #'car :test #'string=)))
      (cadr attrib))))

(defmethod invalidate-calculation ((node environment) source input-name)
  (declare (ignore source input-name))
  (push (lambda (renderer)
          (declare (ignore renderer))
          (loop
             for (input-name) in (inputs node)
             do (input-value node input-name)))
        (predraw-queue (renderer node))))
