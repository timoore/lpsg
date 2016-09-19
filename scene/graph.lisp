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

(defclass transform ()
  ((local-matrix :input-accessor local-matrix :initarg :local-matrix
                 :initform (sb-cga:identity-matrix))
   (parent-matrix :input-accessor parent-matrix :initarg :parent-matrix
                  :initform (sb-cga:identity-matrix))
   (world-matrix :compute-function world-matrix)
   (world-matrix-inverse :compute-function world-matrix-inverse))
  (:metaclass compute-class))

(defmethod world-matrix ((node transform))
  (sb-cga:matrix* parent-matrix local-matrix))

(defmethod world-matrix-inverse ((node transform))
  (sb-cga:inverse-matrix (world-matrix node)))

(defclass node ()
  ((transform :accessor transform :initform (make-instance 'transform))))

(defclass leaf (node)
  ())

(defclass group (node)
  ((children :accessor children :initarg :children :initform nil)))

(defgeneric add-child (parent child))

(defmethod add-child ((parent group) (child node))
  (connect (transform child) 'parent-matrix (transform group) 'world-matrix)
  (push child (children parent)))

(defgeneric remove-child (parent child))

(defmethod remove-child ((parent group) (child node))
  (disconnect (transform child) 'parent-matrix (transform group))
  (setf (children parent) (delete child (children parent))))

(defgeneric map-children (fn node))

(defmethod map-children (fn (node leaf))
  nil)

(defmethod map-children (fn (node group))
  (mapc fn (children group)))

(defclass shape-node (leaf)
  ((shape :accessor shape :initform nil)
   (submitted :accessor submitted :initform nil :documentation "?")))

(defgeneric submit-graph (root renderer) &key camera-stage)

(defgeneric retract-graph (root renderer) &key camera-stage)
