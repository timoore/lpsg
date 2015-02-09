(in-package #:lpsg)

;;; An assembly is a collection of shapes and transforms. Effects too?

(defclass assembly ()
  ((children :accessor children :initarg :children :initform nil)))

(defmethod add-object ((parent assembly) object)
  (push object (children parent)))

(defun do-shapes (assembly func)
  (loop
     for obj in (children assembly)
     if (typep obj 'shape)
     do (funcall func obj)))
