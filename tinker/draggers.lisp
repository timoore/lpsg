;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;

(in-package #:lpsg-tinker)

;;; Classes for calculating displacement and rotation of a camera during a user mouse-drag event.
;;;
;;; Mouse coordinates are in viewport pixel coordinates i.e., 0,0 is at lower left.

(defclass user-view-mixin ()
  ((viewport :initarg :viewport)         ; (vec4 left bottom width height)
   (perspective-matrix :initarg :perspective-matrix)
   (view-matrix :initarg :view-matrix)
   (view-matrix-inverse))
  (:documentation "The camera parameters to use during the drag operation."))

(defmethod initialize-instance :after ((obj user-view-mixin) &key view-matrix)
  (setf (slot-value obj 'view-matrix-inverse) (inverse-matrix view-matrix)))

(defgeneric current-world-transform (dragger new-mouse)
  (:documentation "Returns the current transform produced by dragging the mouse to
@cl:param{new-mouse}, in the world coordinate system, when the dragger is active. This transform
should be applied to the camera parameters to obtain its new orientation."))


(defclass translate-dragger (user-view-mixin)
  ((start-mouse-point :initarg :start-mouse-point))
  (:documentation "Class for tracking pointer motion in terms of world coordinates."))

(defmethod initialize-instance :after ((obj translate-dragger) &key start-mouse-point)
  (setf (slot-value obj 'start-mouse-point) (vec3 start-mouse-point)))

(defgeneric current-displacement (dragger new-mouse))

(defmethod current-displacement ((dragger translate-dragger) new-mouse)
  (with-slots (start-mouse-point viewport perspective-matrix view-matrix)
      dragger
    (let* ((start-mouse-world
            (unproject start-mouse-point view-matrix perspective-matrix viewport))
           (new-mouse-point (vec3 new-mouse 0.0))
           (new-mouse-world (unproject new-mouse-point view-matrix perspective-matrix viewport)))
      (vec- new-mouse-world start-mouse-world))))

(defmethod current-world-transform ((dragger translate-dragger) new-mouse)
  (with-slots (start-mouse-point viewport perspective-matrix view-matrix view-matrix-inverse)
      dragger
    (let* ((start-mouse-camera (unproject start-mouse-point
                                          +identity-matrix+
                                          perspective-matrix
                                          viewport))
           (mouse-point (vec3 new-mouse 0.0))
           (mouse-camera (unproject mouse-point +identity-matrix+ perspective-matrix viewport)))
      (matrix* view-matrix-inverse
               (translate (vec- start-mouse-camera mouse-camera))
               view-matrix))))

;;; Rotational Dragger - arcball with Holroyd variation: see
;;; http://www.diku.dk/~kash/papers/DSAGM2002_henriksen.pdf

(defun quat-normalize (q)
  (let ((norm (sqrt (reduce #'+ q :key (lambda (x) (* x x))))))
    (map 'kit.math::quaternion (lambda (x) (/ x norm)) q)))

;;; From http://lolengine.net/blog/2014/02/24/quaternion-from-two-vectors-final
(defun quat-between-vecs (u v)
  (let* ((norm-u2-v2 (sqrt (* (dot-product u u) (dot-product v v))))
         (real-part (+ norm-u2-v2 (dot-product u v))))
    ;; If vectors are opposite, choose arbitrary rotation axis
    (if (< real-part (* 1.0e-6 norm-u2-v2))
        (quat-normalize (if (> (abs (aref u 0)) (abs (aref u 2)))
                            (kit.math::quaternion 0.0 (- (aref u 1)) (aref u 0) 0.0)
                            (kit.math::quaternion 0.0 0.0 (- (aref u 2)) (aref u 1))))
        (let ((w (cross-product u v)))
          (quat-normalize (kit.math::quaternion real-part (aref w 0) (aref w 1) (aref w 2)))))))

(defclass rotate-dragger (user-view-mixin)
  ((center :accessor center :documentation "The center of the arcball in camera coordinates")
   (m-center)
   (m-center-inv)
   (radius :initarg :radius)
   (initial-vector)))

(defun dist2d-squared (u v)
  (let ((deltax (- (aref u 0) (aref v 0)))
        (deltay (- (aref u 1) (aref v 1))))
    (+ (* deltax deltax) (* deltay deltay))))

(defun distance2d (u v)
  (sqrt (dist2d-squared u v)))

(defun compute-arcball-intersection (rotate-dragger mouse-point)
  (with-slots (viewport perspective-matrix center radius)
      rotate-dragger
    (let* ((camera-mouse-point (unproject (vec3 mouse-point 0.0)
                                         +identity-matrix+
                                         perspective-matrix
                                         viewport))
           (d2 (dist2d-squared camera-mouse-point center))
           (r2 (* radius radius))
           (z (if (<= d2 (/ r2 2.0))
                  (sqrt (- r2 d2))
                  (/ r2 (* 2.0 (sqrt d2))))))
      (vec3 (aref camera-mouse-point 0) (aref camera-mouse-point 1) z))))


(defmethod initialize-instance :after ((obj rotate-dragger)
                                       &key start-mouse-point arcball-center view-matrix)
  (with-slots (initial-vector)
      obj
    (let ((center (transform-point arcball-center view-matrix)))
      (setf (slot-value obj 'center) center)
      (setf (slot-value obj 'm-center) (translate center))
      (setf (slot-value obj 'm-center-inv) (translate (vec* center -1.0)))
      (setf initial-vector (compute-arcball-intersection obj start-mouse-point)))))

(defgeneric current-rotation-quat (dragger new-mouse)
  (:documentation "Returns the quaternion of the arcball rotation specified by
@cl:param{new-mouse}, in the coordinate system of the camera.")) 

(defmethod current-rotation-quat ((dragger rotate-dragger) new-mouse)
  (with-slots (initial-vector)
      dragger
    (let ((new-vec (compute-arcball-intersection dragger new-mouse)))
      (quat-between-vecs initial-vector new-vec))))

(defgeneric current-rotation (dragger new-mouse))
(defmethod current-rotation ((dragger rotate-dragger) new-mouse)
  (kit.math::quat-rotate-matrix (current-rotation-quat dragger new-mouse)))

;;; The transform of the arcball around the center (target) point in the camera coordinate system
;;; is McRMc'. The camera should be transformed by the inverse of that, McR'Mc'. We need this in
;;; the world coordinate system, because ultimately we will need to change properties of the camera
;;; (eye, up) that are expressed in world coordinates. V is the view matrix, and the needed
;;; transform is expressed as V'McR'Mc'V.

(defmethod current-world-transform ((dragger rotate-dragger) new-mouse)
  (with-slots (view-matrix m-center m-center-inv view-matrix-inverse)
      dragger
    (let* ((rotation-quat (current-rotation-quat dragger new-mouse))
           (rotation-quat-inv (kit.math::quat-inverse rotation-quat))
           (rotation-inv (kit.math::quat-rotate-matrix rotation-quat-inv)))
      (matrix* view-matrix-inverse m-center rotation-inv m-center-inv view-matrix))))
