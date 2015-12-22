;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

(defclass render-bundle ()
  ((attribute-set :accessor attribute-set :initarg :attribute-set)
   (shape :accessor shape :initarg :shape)
   (environment :accessor environment :initarg :environment)
   ;; environment? Is gl-state for the moment
   ))

(defmethod gl-finalized-p ((obj render-bundle))
  (and (gl-finalized-p (attribute-set obj))
       (gl-finalized-p (environment obj))))

(defmethod gl-finalize ((obj render-bundle) &optional (errorp t))
  (let* ((env (environment obj))
         (gl-state (gl-state env)))
    (unless (gl-finalized-p env)
      (gl-finalize env errorp))
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
  (let* ((env (environment bundle))
         (gl-state (gl-state env)))
    (bind-state renderer gl-state)
    (loop
       with program = (program gl-state)
       for uset in (uniform-sets env)
       do (upload-uset-to-program uset program))
    (let ((attr-set (attribute-set bundle))
          (drawable (drawable (shape bundle))))
      (gl:bind-vertex-array (id (vao attr-set)))
      (if (element-binding attr-set)
          (let ((index-offset (offset (element-binding attr-set))))
            (%gl:draw-elements (mode drawable)
                               (vertex-count drawable)
                               (index-type drawable)
                               (cffi:inc-pointer (cffi:null-pointer) index-offset)))
          (gl:draw-arrays (mode drawable) 0 (number-vertices drawable))))))
