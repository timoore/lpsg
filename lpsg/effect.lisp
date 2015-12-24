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

;;; This shares a lot of slots with the environment class; should the two classes be more
;;; integrated?
(defclass simple-effect (effect)
  ((environment :accessor environment :initarg :environment))
  (:documentation "This class supports effects which are simply the application of OpenGL state,
with uset parameters, to a shape. XXX probably can't work as is."))

(defmethod submit-with-effect (shape renderer (effect simple-effect))
  (let* ((env (environment effect))
         (attr-map (attribute-map env))
         (attrib-set (make-instance 'attribute-set)))
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
      (push bundle (finalize-queue renderer))
      ;; For now, just use one render-stage / render-queue
      (unless (render-stages renderer)
        (setf (render-stages renderer)
              (list (make-instance 'render-stage
                                   :render-queues (list (make-instance 'render-queue))))))
      (let ((rq (car (render-queues (car (render-stages renderer))))))
        (add-bundle rq bundle)))))


