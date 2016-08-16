;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;

(in-package #:lpsg-scene)

(defconstant +pif+ (float pi 1.0))

(lpsg::define-protocol-class camera ()
  ((:reader view-matrix)
   (:writer %view-matrix)
   (:reader view-matrix-inverse)
   (:writer %view-matrix-inverse)
   (:reader projection-matrix)
   (:writer %projection-matrix)
   (:reader projection-matrix-inverse)
   (:writer %projection-matrix-inverse)))

(defgeneric aim-camera (camera eye target up))

(defclass view-mixin ()
  ((view-matrix :compute-function view-matrix)
   (view-matrix-inverse :compute-function view-matrix-inverse))
  (:metaclass compute-class))

(defclass aimed-camera-mixin (view-mixin)
  ((eye :input-accessor eye :initarg :eye)
   (target :input-accessor target :initarg :target)
   (up :input-accessor up :initarg :up))
  (:default-initargs :eye (sb-cga:vec 0.0 0.0 0.0) :target (sb-cga:vec 0.0 0.0 -1.0)
                     :up (sb-cga:vec 0.0 1.0 0.0))
  (:metaclass compute-class))

(defmethod aim-camera ((camera aimed-camera-mixin) eye target up)
  (setf (eye camera) eye)
  (setf (target camera) target)
  (setf (up camera) up)
  camera)

(defmethod view-matrix ((camera aimed-camera-mixin))
  (look-at (eye camera) (target camera) (up camera)))

(defmethod view-matrix-inverse ((camera aimed-camera-mixin))
  (inverse-matrix (view-matrix camera)))

(defclass projection-mixin ()
  ((projection-matrix :compute-function projection-matrix)
   (projection-matrix-inverse :compute-function projection-matrix-inverse))
  (:metaclass compute-class))

(defmethod projection-matrix-inverse ((camera projection-mixin))
  (inverse-matrix (projection-matrix camera)))

(defclass fov-camera-mixin (projection-mixin)
  ((fovy :input-accessor fovy :initarg :fovy)
   (aspect :input-accessor aspect :initarg :aspect)
   (near :input-accessor near :initarg :near)
   (far :input-accessor far :initarg :far))
  (:default-initargs :fovy (/ +pif+ 4.0) :aspect 1.0 :near 1.0 :far 100.0)
  (:metaclass compute-class))

(defgeneric set-perspective-params (camera fovy aspect near far))

(defmethod set-perspective-params ((camera fov-camera-mixin) fovy aspect near far)
  (setf (fovy camera) fovy)
  (setf (aspect camera) aspect)
  (setf (near camera) near)
  (setf (far camera) far)
  camera)

(defmethod projection-matrix ((camera fov-camera-mixin))
  (perspective-matrix (fovy camera) (aspect camera) (near camera) (far camera)))

(defclass simple-camera (aimed-camera-mixin fov-camera-mixin camera)
  ()
  (:metaclass compute-class))

(defclass ortho-camera-mixin (projection-mixin)
  ((left :input-accessor left :initarg :left)
   (right :input-accessor right :initarg :right)
   (bottom :input-accessor bottom :initarg :bottom)
   (top :input-accessor top :initarg :top)
   (near :input-accessor near :initarg :near)
   (far :input-accessor far :initarg :far))
  (:default-initargs :left -1.0 :right 1.0 :bottom -1.0 :top 1.0 :near -1.0 :far 1.0)
  (:metaclass compute-class))

(defgeneric set-ortho-params (camera left right bottom top near far))

(defmethod set-ortho-params ((camera ortho-camera-mixin) left right bottom top near far)
  (setf (left camera) left)
  (setf (right camera) right)
  (setf (bottom camera) bottom)
  (setf (top camera) top)
  (setf (near camera) near)
  (setf (far camera) far)
  camera)

(defmethod projection-matrix ((camera ortho-camera-mixin))
  (ortho-matrix (left camera) (right camera) (bottom camera) (top camera)
                (near camera) (far camera)))


(defclass simple-ortho-camera (aimed-camera-mixin ortho-camera-mixin camera)
  ()
  (:metaclass compute-class))

(defclass simple-fov-camera (aimed-camera-mixin fov-camera-mixin camera)
  ()
  (:metaclass compute-class))
