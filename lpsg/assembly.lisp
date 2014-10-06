(in-package #:lpsg)

(defclass assembly ()
  ((children :accessor children :initarg :children :initform nil)))

(defmethod add-object ((parent assembly) object)
  (push object (children parent)))

