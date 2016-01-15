;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

(defmacro with-allocator ((allocator &rest allocator-args) &body body)
  (multiple-value-bind (forms declarations)
      (alexandria:parse-body body)
    `(let ((,allocator (make-instance ,@allocator-args)))
       ,@declarations
       (open-allocator ,allocator)
       (unwind-protect
            (progn
              ,@forms)
         (close-allocator ,allocator)))))
