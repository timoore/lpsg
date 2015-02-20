;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;; An effect represents the rendered appearance of a shape. It is responsable for creating bundles
;;; and their environments and putting them in the appropriate render queues.

(defclass effect ()
  ())

(defclass simple-effect (effect)
  ((shader-program :accessor shader-program :initarg :shader-program)))
