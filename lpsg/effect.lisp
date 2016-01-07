;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;; An effect represents the rendered appearance of a shape. It is responsable for creating bundles
;;; and their environments and putting them in the appropriate render queues. The effect object
;;; contains graphics environments. The SIMPLE-EFFECT class only has one environment, but other
;;; effects might have different environments for different passes.

(in-package #:lpsg)

(defclass effect ()
  ((attribute-map :accessor attribute-map :initarg :attribute-map :initform nil
                  :documentation "Alist that maps from vertex attribute names
  to indices or NIL for overall binding. XXX Not clear if this will be used.")
   )
  (:documentation "Class that represents the rendered appearance of a shape.

 EFFECT is responsable for creating bundles and their environments and putting them in the
  appropriate render queues. The effect object contains graphics environments. The SIMPLE-EFFECT
  class only has one environment, but other effects might have different environments for different
  passes."))

(defclass simple-effect (effect)
  ((attribute-map :accessor attribute-map :initform nil :initarg :attribute-map
                  :documentation "list of (symbol glsl-name) where glsl-name is a string")
   (gl-state :accessor gl-state :initarg :gl-state)
   (uset-names :accessor uset-names :initarg :uset-names :initform nil))
  (:documentation "This class supports effects which are simply the application of OpenGL state,
with uset parameters, to a shape."))

(defmethod submit-with-effect (shape renderer (effect simple-effect))
  (let* ((env (make-instance 'environment
                             :attribute-map (attribute-map effect)
                             :gl-state (gl-state effect)
                             :renderer renderer
                             :inputs (inputs shape)
                             :uniform-sets (mapcar (lambda (uset-name)
                                                     (input-value shape uset-name))
                                                   (uset-names effect))))
         (attr-map (attribute-map env))
         (attrib-set (make-instance 'attribute-set)))
    ;; Enqueue initial update of usets
    (invalidate-calculation env nil nil)
    ;; Make attribute set from shape and drawable attributes The actual vertex
    ;; attribute index for an attribute may not be known until the shader
    ;; program is linked, so make it invalid for now and let gl-finalize sort
    ;; it out. If an attribute is not supported by the environment, no problem;
    ;; just ignore it.
    (setf (array-bindings attrib-set)
          (mapcan (lambda (entry)
                    (let ((gl-name (cdr (assoc (car entry) attr-map))))
                      (if gl-name
                          (list (list gl-name (cdr entry) -1))
                          nil)))
                  (attributes shape)))
    (when (typep (drawable shape) 'indexed-drawable)
      (setf (element-binding attrib-set) (element-array (drawable shape))))
    (let ((bundle (make-instance 'render-bundle
                                 :attribute-set attrib-set :shape shape :environment env)))
      (push bundle (bundles shape))
      (push bundle (finalize-queue renderer))
      ;; For now, just use one render-stage / render-queue
      (unless (render-stages renderer)
        (setf (render-stages renderer)
              (list (make-instance 'render-stage
                                   :render-queues (list (make-instance 'render-queue))))))
      (let ((rq (car (render-queues (car (render-stages renderer))))))
        (add-bundle rq bundle)))))

