;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;; Early defgeneric forms for LPSG.

(in-package #:lpsg)

(define-protocol-class renderer ()
  ((:generic open-renderer (renderer)
    (:documentation "Intialize @cl:param(renderer) for rendering.

The OpenGL context that will be used to do all rendering must be current when generic function
called.  This generic function verifies that the context can support the rendering done by lpsg;
that is, the version of OpenGL supports the features needed by lpsg or has extensions that support
them. It also records parameters and capabilities of the OpenGL implementation, such as the number
of texture units."))
   (:generic close-renderer (renderer &key deallocate-objects)
    (:documentation "Close @cl:param(renderer) for rendering.

If @cl:param(deallocate-objects) is true, then all OpenGL objects that are still allocated by
LPSG will be explicitly deallocated. The default, @c(nil), doesn't deallocate these objects; it
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
   (:accessor drawable))
  (:documentation "Protocol class for rendered objects."))

(define-protocol-class render-queue ()
  ((:generic add-rendered-object (render-queue object)
    (:documentation "Add @cl:param(object) to @cl:param(render-queue).

The order in which objects in the queue are rendered is undefined. This function is used in the
implementation of SUBMIT-WITH-EFFECT."))
   (:generic remove-rendered-object (render-queue object)
    (:documentation "Remove @cl:param(object) from @cl:param(render-queue)."))

   (:generic map-render-queue (render-queue function)
    (:documentation "Call @cl:param(function) on each object stored in the
@cl:param(render-queue)."))

   (:generic find-if-queue (predicate render-queue)
    (:documentation "Search for an object in @cl:param(render-queue) that satisfies
@cl:param(predicate)"))
   (:accessor graphics-state :documentation "A graphics-state object pushed before the traversal of
the queue's objects, and popped after.")
   (:generic draw-queue (renderer renderer-queue)
    (:documentation "Draw each item in @cl:param(render-queue). If a render queue object is found
    in @cl:param(render-queue), then @c(draw-queue) is called recurisvely on it.")))
  (:documentation "A container class for objects, including @c(render-queue) objects too.

This class contains the objects that are traversed to render a scene. This class does not guarantee
a traversal order for objects in the queue. Subclasses of this class might sort the objects to
obtain an optimal order, or in fact guarantee an order."))

(define-protocol-class gl-object ()
  ((:accessor id :documentation "The OpenGL ID of an object.")
   (:accessor gl-proxy :documentation "Object for accessing the associated
OpenGL object"))
  (:documentation "Class representing any OpenGL object."))

(defgeneric gl-finalize (obj &optional errorp)
  (:documentation "Allocate any OpenGL resources needed for @cl:param(obj) and perform any
tasks needed to use it (e.g. link a shader program).

Returns @c(t) if finalize actions were performed, @c(nil) otherwise.

This is called when the renderer's OpenGL context is current. The renderer is accessible in
@c(*renderer*)."))

(defgeneric gl-finalized-p (obj)
  (:documentation "Returns @c(t) if object has already been finalized."))

(defgeneric gl-destroy (obj)
  (:documentation "Deallocate an OpenGL object."))

(defgeneric update-effect (effect)
  (:method-combination progn)
  (:documentation "Perform necessary actions to prepare usets for the next frame, including
obtaining updated values and loading them into buffers. Effects should define progn methods on this
generic function."))
