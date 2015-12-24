;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;; Early defgeneric forms for LPSG.

(in-package #:lpsg)

(defgeneric submit (assembly renderer))

(defgeneric submit-with-effect (shape renderer effect)
  (:documentation "Submit SHAPE to RENDERER.

This function creates all the bundles necessary to render SHAPE with the appearance defined by
EFFECT."))

(defgeneric add-object (parent child))

(defgeneric compute-buffer-allocation (shape allocator)
  (:documentation "Compute the storage needed by the attributes of a
  shape and allocate their buffers in the BUFFER-AREA slots of each of the shape's
  attributes.."))
