;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;; An effect represents the rendered appearance of a shape. It is responsable for creating bundles
;;; and their environments and putting them in the appropriate render queues. The effect object
;;; contains graphics environments. The SIMPLE-EFFECT class only has one environment, but other
;;; effects might have different environments for different passes.

(in-package #:lpsg)

(defclass effect ()
  ((attribute-map :accessor attribute-map :initarg :attribute-map :initform nil
                  :documentation "Alist that maps from vertex attribute names
  to indices or NIL for overall binding. XXX Not clear if this will be used.")))

;;; This shares a lot of slots with the environment class; should the two classes be more
;;; integrated?
(defclass simple-effect (effect)
  ((environment :accessor environment :initarg environment)))

(defmethod submit-with-effect (shape renderer (effect simple-effect))
  (let* ((env (environment effect))
         (bundle (make-instance 'render-bundle :shape shape :gl-state env))
         (attr-map (attribute-map env)))
    ;; Make attribute set from shape and drawable attributes
    ;; The actual vertex attribute index for an attribute may not be known until the shader program
    ;; is linked, so make it invalid for now and let gl-finalize sort it out.
    ;; If an attribute is not supported by the environment, no problem; just
    ;; ignore it.
    (setf (array-bindings bundle)
          (mapcar (lambda (entry)
                    (let ((gl-name (cdr (assoc (car entry) attr-map))))
                      (if gl-name
                          `((,gl-name ,(cdr entry) -1))
                          nil)))
                  (attributes shape)))
    (when (typep (drawable shape) 'indexed-drawable)
      (setf (element-binding bundle) (element-array (drawable shape))))
    ;; For now, just use one render-stage / render-queue
    (unless (render-stages renderer)
      (setf (render-stages renderer)
            (list (make-instance 'render-stage
                                 :render-queues (list (make-instance 'render-queue))))))
    (let ((rq (car (render-queues (render-stages renderer)))))
      (add-bundle rq bundle))))

