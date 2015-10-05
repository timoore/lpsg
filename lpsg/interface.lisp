;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;; Early defgeneric forms for LPSG.

(in-package #:lpsg)

(defgeneric submit (assembly renderer))

(defgeneric submit-with-effect (shape renderer effect))


(defgeneric add-object (parent child))

(defgeneric compute-buffer-allocation (shape &key base-offset)
  (:documentation "Compute the storage needed by the attributes of a
  shape. This function assigns values to the BUFFER-AREA slots of each
  attribute, but doesn't create or assign a buffer object.

  XXX For now it is assumed that all data is placed in one buffer object
  starting at BASE-OFFSET."))

