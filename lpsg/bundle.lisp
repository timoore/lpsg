;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

(defclass render-bundle ()
  ((attribute-set :accessor attribute-set :initarg :attribute-set)
   (shape :accessor shape :initarg :shape)
   ;; environment? Is gl-state for the moment
   (gl-state :reader gl-state :initarg :gl-state)))

(defclass render-bundle ()
  ((attribute-set :accessor attribute-set :initarg :attribute-set)
   (shape :accessor shape :initarg :shape)
   ;; environment? Is gl-state for the moment
   (gl-state :reader gl-state :initarg :gl-state)))

(defmethod gl-finalized-p ((obj render-bundle))
  (and (gl-finalized-p (attribute-set obj))
       (gl-finalized-p (gl-state obj))))

(defmethod gl-finalize ((obj render-bundle) &optional (errorp t))
  (let ((gl-state (gl-state obj)))
    (unless (gl-finalized-p gl-state)
      (gl-finalize gl-state errorp))
    (let* ((program (program gl-state))
           (attrs (vertex-attribs program))
           (attribute-set (attribute-set obj)))
      (loop
         for binding in (array-bindings attribute-set)
         for (name) = binding
         for vertex-attrib = (find name attrs :key #'car :test #'string=)
         do (when vertex-attrib
              ;; XXX Test format of vertex attribute
              ;; set attribute location from program
              (setf (caddr binding) (cadr vertex-attrib))))
      (unless (gl-finalized-p attribute-set)
        (gl-finalize attribute-set errorp)))))


(defmethod draw-bundle ((renderer renderer) bundle)
  (bind-state renderer (gl-state bundle))
  (let ((attr-set (attribute-set bundle))
        (drawable (drawable (shape bundle))))
    (gl:bind-vertex-array (vao attr-set))
    (if (element-binding attr-set)
        (let ((index-offset (offset (element-binding attr-set))))
          (%gl:draw-elements (mode drawable)
                             (number-vertices drawable)
                             (buffer-type (element-binding attr-set))
                             (cffi:inc-pointer (cffi:null-pointer) index-offset)))
        (gl:draw-arrays (mode drawable) 0 (number-vertices drawable)))))
