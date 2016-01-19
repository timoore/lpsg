;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;; Early defgeneric forms for LPSG.

(in-package #:lpsg)

(defgeneric submit (object renderer)
  (:documentation "Submit OBJECT to RENDERER."))

(defgeneric submit-with-effect (shape renderer effect)
  (:documentation "Submit SHAPE to RENDERER.

This function creates all the bundles necessary to render SHAPE with the appearance defined by
EFFECT. Usually the effect is stored in the shape, so this method doesn't need to be called
directly; (submit shape renderer) is equivalent.")) 

(defgeneric retract (object renderer)
  (:documentation "Remove OBJECT from consideration by RENDERER. This may deallocate graphics API
resources."))

(defgeneric retract-with-effect (shape renderer effect)
  (:documentation "Called by RETRACT with a shape argument."))

(defgeneric add-object (parent child))

(defgeneric compute-buffer-allocation (shape allocator)
  (:documentation "Compute the storage needed by the attributes of a
  shape and allocate their buffers in the BUFFER-AREA slots of each of the shape's
  attributes.."))
