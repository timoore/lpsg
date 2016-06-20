;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

(defclass render-bundle ()
  ((attribute-set :accessor attribute-set :initarg :attribute-set)
   (shape :accessor shape :initarg :shape)
   (environment :accessor environment :initarg :environment))
  (:documentation "Class that ties together attributes and a graphics environment.

A @c(render-bundle) is the lowest level object processed by a @c(renderer) that produces graphics output
via OpenGL. It groups the @c(attribute) objects from a @c(shape) into an @c(attribute-set) that can be bound quickly
in OpenGL. It also stores an @c(environment) that holds all the graphics state needed to render the
geometry of the associated @c(shape). "))

(defmethod gl-finalized-p ((obj render-bundle))
  (and (gl-finalized-p (attribute-set obj))
       (gl-finalized-p (environment obj))))

(defmethod gl-finalize ((obj render-bundle) &optional (errorp t))
  (let* ((env (environment obj))
         (gl-state (gl-state env)))
    (gl-finalize env errorp)
    (let* ((program (glstate-program gl-state))
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
      (gl-finalize attribute-set errorp)
      t)))


(defmethod draw-bundle ((renderer standard-renderer) bundle)
  (let* ((env (environment bundle))
         (gl-state (gl-state env)))
    (unless (visiblep env)
      (return-from draw-bundle nil))
    (bind-state renderer gl-state)
    (loop
       with program = (glstate-program gl-state)
       for uset in (uniform-sets env)
       do (upload-uset-to-program uset program))
    (let ((attr-set (attribute-set bundle))
          (drawable (drawable (shape bundle))))
      (gl:bind-vertex-array (id (vao attr-set)))
      (if (element-binding attr-set)
          (let ((index-offset (offset (element-binding attr-set))))
            (%gl:draw-elements-base-vertex (mode drawable)
                                           (vertex-count drawable)
                                           (index-type drawable)
                                           (cffi:inc-pointer (cffi:null-pointer) index-offset)
                                           (base-vertex drawable)))
          (gl:draw-arrays (mode drawable) 0 (number-vertices drawable))))))
