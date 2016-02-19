;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

(define-gl-object texture ()
  ((target :accessor target)))

(defclass graphics-state ()
  ((texture-bindings)
   (program :accessor program :initarg :program :initform nil))
  (:documentation "Class that stores all OpenGL state."))

(defmethod gl-finalized-p ((obj graphics-state))
  (gl-finalized-p (program obj)))

(defmethod gl-finalize ((obj graphics-state) &optional errorp)
  (gl-finalize (program obj) errorp))

(defgeneric bind-state (renderer state))

(defmethod bind-state ((renderer renderer) (state graphics-state))
  (with-slots (current-state)
      renderer
    (when (eq current-state state)
      (return-from bind-state nil))
    (let* ((old-program (and current-state (program current-state)))
           (new-program (program state)))
      (unless (eq new-program old-program)
        (gl:use-program (id new-program)))
        ;; XXX bindings
      (setf (current-state renderer) state))))

