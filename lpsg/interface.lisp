;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;; Early defgeneric forms for LPSG.

(in-package #:lpsg)

(define-protocol-class renderer ()
  ((:generic open-renderer (renderer)
    (:documentation "Intialize @cl:parameter{renderer} for rendering.

The OpenGL context that will be used to do all rendering must be current when this is called.  This
method verifies that the context can support the rendering done by lpsg; that is, the version of
OpenGL supports the features needed by lpsg or has extensions that support them. It also records
parameters and capabilities of the OpenGL implementation, such as the number of texture
units."))
   (:generic close-renderer (renderer &key deallocate-objects)
    (:documentation "Close @cl:parameter{renderer} for rendering.

If @cl:parameter{deallocate-objects} is @c{t}, then all OpenGL objects that are still allocated by
LPSG will be explicitly deallocated. The default, @c{nil}, doesn't deallocate these objects; it
assumes that the context will soon be destroyed." )) 
   (:generic submit (object renderer)
    (:documentation "Submit OBJECT to RENDERER."))
   (:generic submit-with-effect (shape renderer effect)
    (:documentation "Submit SHAPE to RENDERER.

This function creates all the bundles necessary to render SHAPE with the appearance defined by
EFFECT. Usually the effect is stored in the shape, so this method doesn't need to be called
directly; (submit shape renderer) is equivalent."))

   (:generic retract (object renderer)
    (:documentation "Remove OBJECT from consideration by RENDERER. This may deallocate graphics API
resources."))
   (:generic retract-with-effect (shape renderer effect)
    (:documentation "Called by RETRACT with a shape argument.")))
  (:documentation "The class responsible for all rendering."))



 





(defgeneric add-object (parent child))

(defgeneric compute-buffer-allocation (shape allocator)
  (:documentation "Compute the storage needed by the attributes of a
  shape and allocate their buffers in the BUFFER-AREA slots of each of the shape's
  attributes.."))

(define-protocol-class shape ()
  ((:accessor attributes
              :documentation "Alist of (name . attribute). The names are later mapped to a vertex binding index.")
   (:accessor effect)
   ;; XXX uset computation nodes?
   (:accessor usets)
   (:accessor drawable)))
