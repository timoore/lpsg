;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;; An effect represents the rendered appearance of a shape. It is responsable for creating bundles
;;; and their environments and putting them in the appropriate render queues. The effect object
;;; contains graphics environments. The SIMPLE-EFFECT class only has one environment, but other
;;; effects might have different environments for different passes.

(in-package #:lpsg)

(defclass effect ()
  ())

;;; This shares a lot of slots with the environment class; should the two classes be more
;;; integrated?
(defclass simple-effect (effect)
  ((environment :accessor environment :initarg environment)))

(defmethod submit-with-effect (shape renderer (effect simple-effect))
  (let ((bundle (make-instance 'render-bundle :shape shape :gl-state (environment effect))))
    ;; Make attribute set from shape and drawable attributes
    (setf (array-bindings bundle) (mapcar (lambda (entry)
                                            (cons nil (cdr entry)))
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

