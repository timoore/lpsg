;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;; Oh no, it's a scene graph!!!
;;;
;;; An LPSG scene graph is a tree of nodes. The leaves contain shapes (more than one shape)? The
;;;internal nodes contain lists of other nodes an a transform representing a coordinate
;;;system. Each transform is an incremental computation node. These transforms are chained together
;;;and evenentually drive the model uset of a shape's effect.
;;;
;;; A scene graph can be submitted to a renderer, which results in all the shape leaves being
;;;submitted.
;;;
;;; A node cannot be shared i.e., have more than one parent.
;;;
;;; A scene graph can be rendered by multiple cameras. The shapes will be submitted to multiple
;;;queues. Should the camera matrix be hooked up to every single shape, or is it time to think
;;;about global usets?

(in-package #:lpsg-scene)


(lpsg:define-uset camera (("projectionMatrix" :float-mat4
                                              projection-matrix :accessor projection-matrix)
                          ("cameraMatrix" :float-mat4
                                          camera-matrix :accessor camera-matrix)
                          ("cameraMatrixInverse" :float-mat4
                                                 camera-matrix-inverse
                                                 :accessor camera-matrix-inverse)))

(lpsg:define-uset model-uset (("modelMatrix" :float-mat4
                                             model-matrix :accessor model-matrix)
                              ("modelMatrixInverse" :float-mat4
                                                    model-matrix-inverse
                                                    :accessor model-matrix-inverse)))

(defclass transform ()
  ((local-matrix :input-accessor local-matrix :initarg :local-matrix
                 :initform (sb-cga:identity-matrix))
   (parent-matrix :input-accessor parent-matrix :initarg :parent-matrix
                  :initform (sb-cga:identity-matrix))
   (world-matrix :compute-function world-matrix)
   (world-matrix-inverse :compute-function world-matrix-inverse))
  (:metaclass compute-class))

(defmethod world-matrix ((node transform))
  (sb-cga:matrix* (parent-matrix node) (local-matrix node)))

(defmethod world-matrix-inverse ((node transform))
  (sb-cga:inverse-matrix (world-matrix node)))

(defclass node (transform)
  ())

(defmacro define-node-class (name superclasses slots &rest options)
  `(defclass ,name (,@superclasses node)
     ,slots
     ,@options
     (:metaclass compute-class)))

(define-node-class leaf ()
  ())

(define-node-class group ()
  ((children :accessor children :initarg :children :initform nil)))

(defgeneric add-child (parent child))

(defmethod add-child ((parent group) (child node))
  (connect child 'parent-matrix group 'world-matrix)
  (push child (children parent)))

(defgeneric remove-child (parent child))

(defmethod remove-child ((parent group) (child node))
  (disconnect child 'parent-matrix group)
  (setf (children parent) (delete child (children parent))))

(defgeneric map-children (fn node))

(defmethod map-children (fn (node leaf))
  nil)

(defmethod map-children (fn (node group))
  (mapc fn (children group)))

(define-node-class shape-node (leaf)
  ((shape :accessor shape :initform nil)
   (submitted :accessor submitted :initform nil :documentation "?")
   (model-uset :compute-function model-uset)
   (%model-uset :accessor %model-uset :initform (make-instance 'model-uset))))

(defmethod model-uset ((node shape-node))
  (let ((uset (%model-uset node)))
    (setf (model-matrix uset) (world-matrix node)
          (model-matrix-inverse uset) (world-matrix-inverse node))
    uset))

(define-node-class camera-node (view-mixin camera)
  ((projection-matrix :input-accessor projection-matrix :initarg :projection-matrix)
   (projection-matrix-inverse :compute-function projection-matrix-inverse)))

(defmethod view-matrix ((node camera-node))
  (world-matrix-inverse node))

(defmethod projection-matrix-inverse ((camera camera-node))
  (inverse-matrix (projection-matrix camera)))

(define-node-class light ()
  ())

(defclass scene ()
  ((cameras)
   (lights)
   (groups :documentation "?")))

(defgeneric submit-graph (root renderer) &key camera-stage)

(defgeneric retract-graph (root renderer) &key camera-stage)

