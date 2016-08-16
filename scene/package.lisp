;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;

(defpackage #:lpsg-scene
  ;; Some symbols conflict with cl-opengl's.
  (:shadowing-import-from #:sb-cga
                          #:translate
                          #:rotate
                          #:scale)
  (:shadowing-import-from #:kit.math
                          #:frustum)
  (:use #:cl #:gl #:sb-cga #:kit.math #:lpsg)
  (:export
   #:camera
   #:view-matrix
   #:view-matrix-inverse
   #:projection-matrix
   #:projection-matrix-inverse
   #:aimed-camera-mixin
   #:aim-camera
   #:eye
   #:target
   #:up
   #:fov-camera-mixin
   #:fovy
   #:aspect
   #:near
   #:far
   #:set-perspective-params
   #:simple-camera
   #:left
   #:bottom
   #:right
   #:top
   #:set-ortho-params
   #:simple-ortho-camera
   #:ortho-camera-mixin
   #:view-node-mixin
   #:view-matrix-node
   #:view-matrix-inverse-node
   #:projection-node-mixin
   #:projection-matrix-node
   #:projection-matrix-inverse-node
   #:translate-dragger
   #:rotate-dragger
   #:current-world-transform
   ;; utilities
   #:make-cube-shape

   ))
