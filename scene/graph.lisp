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

(in-package #:lpsg)
