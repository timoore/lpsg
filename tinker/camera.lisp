;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;

(in-package #:lpsg-tinker)

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
  ((view-matrix :reader view-matrix :writer (setf %view-matrix))
   (view-matrix-inverse :reader view-matrix-inverse :writer (setf %view-matrix-inverse))))

(defclass aimed-camera-mixin (view-mixin)
  ((eye :accessor eye)
   (target :accessor target)
   (up :accessor up))
  (:default-initargs :eye (sb-cga:vec 0.0 0.0 0.0):target (sb-cga:vec 0.0 0.0 -1.0)
                     :up (sb-cga:vec 0.0 1.0 0.0)))

(defmethod aim-camera ((camera aimed-camera-mixin) eye target up)
  (setf (eye camera) eye)
  (setf (target camera) target)
  (setf (up camera) up)
  camera)

(defmethod aim-camera :after ((camera view-mixin) eye target up)
  (let* ((viewmat (look-at eye target up))
         (viewmatinv (inverse-matrix viewmat)))
    (setf (%view-matrix camera) viewmat)
    (setf (%view-matrix-inverse camera) viewmatinv)))

(defmethod initialize-instance :after ((obj aimed-camera-mixin) &key eye target up)
  (aim-camera obj eye target up))

(defclass projection-mixin ()
  ((projection-matrix :reader projection-matrix :writer (setf %projection-matrix))
   (projection-matrix-inverse :reader projection-matrix-inverse
                              :writer (setf %projection-matrix-inverse))))

(defclass fov-camera-mixin (projection-mixin)
  ((fovy :accessor fovy)
   (aspect :accessor aspect)
   (near :accessor near)
   (far :accessor far))
  (:default-initargs :fovy (/ +pif+ 4.0) :aspect 1.0 :near 1.0 :far 100.0))

(defgeneric set-perspective-params (camera fovy aspect near far))

(defmethod set-perspective-params ((camera fov-camera-mixin) fovy aspect near far)
  (setf (fovy camera) fovy)
  (setf (aspect camera) aspect)
  (setf (near camera) near)
  (setf (far camera) far)
  camera)

(defmethod initialize-instance :after ((obj fov-camera-mixin) &key fovy aspect near far)
  (set-perspective-params obj fovy aspect near far))

(defmethod set-perspective-params :after ((camera projection-mixin) fovy aspect near far)
  (let* ((perspmat (perspective-matrix fovy aspect near far))
         (perspmat-inverse (inverse-matrix perspmat)))
    (setf (%projection-matrix camera) perspmat)
    (setf (%projection-matrix-inverse camera) perspmat-inverse))
  camera)

(defclass simple-camera (camera aimed-camera-mixin fov-camera-mixin)
  ())

(defclass ortho-camera-mixin (projection-mixin)
  ((left :accessor left :initarg :left)
   (right :accessor right :initarg :right)
   (bottom :accessor bottom :initarg :bottom)
   (top :accessor top :initarg :top)
   (near :accessor near :initarg :near)
   (far :accessor far :initarg :far))
  (:default-initargs :left -1.0 :right 1.0 :bottom -1.0 :top 1.0 :near -1.0 :far 1.0))

(defgeneric set-ortho-params (camera left right bottom top near far))

(defmethod set-ortho-params ((camera ortho-camera-mixin) left right bottom top near far)
  (setf (left camera) left)
  (setf (right camera) right)
  (setf (bottom camera) bottom)
  (setf (top camera) top)
  (setf (near camera) near)
  (setf (far camera) far)
  camera)

(defmethod initialize-instance :after ((obj ortho-camera-mixin)
                                       &key left right bottom top near far)
  (set-ortho-params obj left right bottom top near far))

(defmethod set-ortho-params :after ((camera projection-mixin) left right bottom top near far)
  (let* ((projmat (ortho-matrix left right bottom top near far))
         (projmat-inverse (inverse-matrix projmat)))
    (setf (%projection-matrix camera) projmat)
    (setf (%projection-matrix-inverse camera) projmat-inverse)))

(defclass simple-ortho-camera (camera aimed-camera-mixin ortho-camera-mixin)
  ())

(defclass simple-fov-camera (camera aimed-camera-mixin fov-camera-mixin)
  ())

;;; Until we have incremental nodes with real CLOS slots, we do things this way.

(defclass view-node-mixin (view-mixin)
  ((view-matrix-node :accessor view-matrix-node)
   (view-matrix-inverse-node :accessor view-matrix-inverse-node)))

(defmethod initialize-instance :after ((obj view-mixin) &key)
  (setf (view-matrix-node obj) (make-instance 'input-value-node))
  (setf (view-matrix-inverse-node obj) (make-instance 'input-value-node)))

(defmethod (setf %view-matrix) :after (new-val (obj view-mixin))
  (setf (value (view-matrix-node obj)) new-val))

(defmethod (setf %view-matrix-inverse) :after (new-val (obj view-mixin))
  (setf (value (view-matrix-inverse-node obj)) new-val))

(defclass projection-node-mixin (projection-mixin)
  ((projection-matrix-node :accessor projection-matrix-node)
   (projection-matrix-inverse-node :accessor projection-matrix-inverse-node)))

;;; around method so that more specific methods can set the matrices.
(defmethod initialize-instance :after ((obj projection-node-mixin) &key)
  (setf (projection-matrix-node obj) (make-instance 'input-value-node))
  (setf (projection-matrix-inverse-node obj) (make-instance 'input-value-node)))

(defmethod (setf %projection-matrix) :after (new-val (obj projection-mixin))
  (setf (value (projection-matrix-node obj)) new-val))

(defmethod (setf %projection-matrix-inverse) :after (new-val (obj projection-mixin))
  (setf (value (projection-matrix-inverse-node obj)) new-val))
