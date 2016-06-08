;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;

(in-package #:lpsg-examples)

(defgeneric on-mouse-motion-event (window event last-event-p))

(lpsg:define-uset camera (("projectionMatrix" :float-mat4
                                              projection-matrix :accessor projection-matrix)
                          ("cameraMatrix" :float-mat4
                                          camera-matrix :accessor camera-matrix)))

(lpsg:define-uset model (("modelMatrix" :float-mat4
                                        model-matrix :accessor model-matrix)))
;;; We support both orthographic and perspective cameras, which are placed with the same
;;; eye-target-up parameters. Therefore we build the different parts from the camera mixin classes
;;; and incremental nodes, and then route their outputs to a camera-uset-node which produces a uset
;;; as its value.

(defclass partial-view-camera (lpsg-tinker:aimed-camera-mixin lpsg-tinker:view-node-mixin)
  ())

(defclass partial-ortho-camera (lpsg-tinker:ortho-camera-mixin lpsg-tinker:projection-node-mixin)
  ())

(defclass partial-fov-camera (lpsg-tinker:fov-camera-mixin lpsg-tinker::projection-node-mixin)
  ())

;;; This incremental node takes 'view-matrix and 'projection-matrix as input and produces a uset.
(defclass camera-uset-node (lpsg:computation-node lpsg:computation-node-mixin lpsg:source-sink-mixin)
  ((uset :accessor uset :initform (make-instance 'camera))))

(defmethod lpsg:compute ((node camera-uset-node))
  (let ((uset (uset node)))
    (setf (camera-matrix uset) (lpsg:input-value node 'view-matrix))
    (setf (projection-matrix uset) (lpsg:input-value node 'projection-matrix))
    uset))


(defclass viewer-window (standard-renderer glop:window)
  (
   ;; For testing if a glop resize event is really a resize
   (saved-width :accessor saved-width)
   (saved-height :accessor saved-height)
   ;; Work around for lack of coordinates in button events
   (last-x :accessor last-x :initform 0)
   (last-y :accessor last-y :initform 0)
   (projection-type :accessor projection-type :initarg :projection-type)
   (view-camera :accessor view-camera
                :initform (make-instance 'partial-view-camera
                                         :eye (sb-cga:vec 0.0 0.0 1.0)
                                         :target (sb-cga:vec 0.0 0.0 -5.0)
                                         :up (sb-cga:vec 0.0 1.0 0.0)))
   (ortho-camera :accessor ortho-camera :initform (make-instance 'partial-ortho-camera))
   (fov-camera :accessor fov-camera :initform (make-instance 'partial-fov-camera))
   ;; if-then node for choosing the orthographic or perspective camera's projection matrix
   (camera-choice :accessor camera-choice)
   ;; An input-value node for holding T or NIL to select the type of camera.
   (camera-selector :accessor camera-selector)
   (camera-uset-node :accessor camera-uset-node :initform (make-instance 'camera-uset-node))
   (exposed :accessor exposed :initarg :exposed)
   (current-dragger :initform nil)
   (last-draw-time :initform 0)
   (last-input-time :initform 0)
   (max-motion-time))
  (:default-initargs :projection-type 'orthographic :exposed nil))

(defmethod initialize-instance :after ((obj viewer-window) &key (max-motion-seconds .0167))
  (let ((choice (make-instance 'lpsg:if-then-node))
        (selector (make-instance 'lpsg:input-value-node
                                 :value (eq (projection-type obj) 'orthographic))))
    (setf (lpsg:input choice 'lpsg:then) (lpsg-tinker:projection-matrix-node (ortho-camera obj)))
    (setf (lpsg:input choice 'lpsg:else) (lpsg-tinker:projection-matrix-node (fov-camera obj)))
    (setf (lpsg:input choice 'if) selector)
    (setf (camera-choice obj) choice)
    (setf (camera-selector obj) selector)
    (setf (lpsg:input (camera-uset-node obj) 'projection-matrix) choice)
    (setf (lpsg:input (camera-uset-node obj) 'view-matrix)
          (lpsg-tinker:view-matrix-node (view-camera obj)))
    (setf (slot-value obj 'max-motion-time) (* max-motion-seconds internal-time-units-per-second))
    ;; default graphics state
    (let ((stage-state (make-instance 'graphics-state
                                      :cull-face (make-instance 'gl-cull-face :face :back)
                                      :depth-func (make-instance 'gl-depth-func :func :less)
                                      :depth-range (make-instance 'gl-depth-range
                                                                  :near 0.0 :far 1.0)
                                      :modes (make-modes '(:cull-face :depth-test)
                                                         '(:dither :multisample)))))
      (setf (graphics-state (render-stage obj)) stage-state))))

(defgeneric draw-window (window)
  (:documentation "Do one pass of the rendering loop."))

(defmethod draw-window ((window viewer-window))
  nil)

(defmethod draw-window :around ((window viewer-window))
  (setf (slot-value window 'last-draw-time) (get-internal-real-time))
  (%gl:clear-color .8 .8 .8 1.0)
  (gl:clear :color-buffer :depth-buffer)
  (call-next-method)
  (glop:swap-buffers window))

(defmethod glop:on-event :around ((window viewer-window) (event glop:expose-event))
  (call-next-method)
  (setf (exposed window) t))

(defmethod glop:on-event ((window viewer-window) (event glop:key-event))
  (when (eq (glop:keysym event) :escape)
      (glop:push-close-event window))
  (when (and (glop:pressed event) (eq (glop:keysym event) :f))
    (glop:toggle-fullscreen window))
  (when (and (glop:pressed event) (eq (glop:keysym event) :g))
    (glop:set-fullscreen window)))

(defmethod glop:on-event ((window viewer-window) (event glop:button-event))
  (declare (ignore event)))

(defmethod on-mouse-motion-event ((window viewer-window) (event glop:mouse-motion-event)
                                  last-event-p)
  (declare (ignore last-event-p)))

(defgeneric update-for-window-change (w event))

(defmethod update-for-window-change ((w viewer-window) event)
  (let ((width (glop:width event))
        (height (glop:height event)))
    (gl:viewport 0 0 width height)))

(defun compute-projection-matrix (window proj-type near far)
  (let ((width (glop:window-width window))
        (height (glop:window-height window)))
    (if (eq proj-type 'orthographic)
        (let* ((right (max (float (/ width height)) 1.0))
               (top (max (float (/ height width)) 1.0)))
          (lpsg-tinker:set-ortho-params (ortho-camera window) (- right) right (- top) top near far)
          (lpsg-tinker:set-perspective-params
           (fov-camera window) (/ (float pi 1.0) 4.0) (/ width height) near far)))))

(defmethod glop:on-event :around ((window viewer-window) (event glop:resize-event))
  (when (not (and (slot-boundp window 'saved-width)
                  (slot-boundp window 'saved-height)
                  (= (saved-width window) (glop:width event))
                  (= (saved-height window) (glop:height event))))
    (call-next-method)))

(defmethod glop:on-event ((window viewer-window) (event glop:resize-event))
  (update-for-window-change window event)
    (format t "Resize: ~Sx~S~%" (glop:width event) (glop:height event)))

(defmethod glop:on-event :after ((window viewer-window) (event glop:resize-event))
  (setf (saved-width window) (glop:width event)
        (saved-height window) (glop:height event))
  (compute-projection-matrix window (projection-type window) 1.0 10.0))

(defmethod glop:on-event ((window viewer-window) (event glop:expose-event))
  (update-for-window-change window event)
  (format t "Expose ~Sx~X~%" (glop:width event) (glop:height event)))

(defmethod glop:on-event ((window viewer-window) (event glop:close-event))
  (declare (ignore event))
  (format t "Close~%"))

(defgeneric open-viewer (window title width height &rest key-args
                         &key major minor fullscreen
                           x y
                           rgba
                           double-buffer
                           stereo
                           red-size
                           green-size
                           blue-size
                           alpha-size
                           depth-size
                           accum-buffer
                           accum-red-size
                           accum-green-size
                           accum-blue-size
                           stencil-buffer
                           stencil-size))

(defmethod open-viewer ((window viewer-window) title width height
                        &rest key-args &key major minor fullscreen
                        &allow-other-keys)
  (apply #'glop:open-window window title width height :allow-other-keys t key-args)
  (glop:create-gl-context window :major major :minor minor :make-current t)
  (glop::show-window window)
  (glop:set-fullscreen window fullscreen)
  window)

(defgeneric process-events (window &optional blocking)
  (:documentation "Process glop events for @cl:param(window).

This calls "))

(defparameter *max-motion-time* (* .0167 internal-time-units-per-second))

(defmethod process-events ((window viewer-window) &optional (blocking t))
  (let ((dropped 0)
        (max-motion-time (slot-value window 'max-motion-time)))
    (flet ((compress-motion (motion-event)
             (let ((saved-event motion-event))
               (loop
                  for read-ahead-event = (glop:next-event window :blocking nil)
                  while (typep read-ahead-event 'glop:mouse-motion-event)
                  do 
                    (let ((curr-time (get-internal-real-time))
                          (last-input-time (slot-value window 'last-input-time)))
                      (if (and (> last-input-time 0)
                               (< (- curr-time last-input-time) max-motion-time))
                          (progn
                            (when saved-event
                              (on-mouse-motion-event window saved-event nil))
                            (incf dropped)
                            (setf saved-event read-ahead-event))
                          (progn
                            (on-mouse-motion-event window read-ahead-event t)
                            (setf (slot-value window 'last-input-time) curr-time
                                  saved-event nil))))
                  finally
                    (when saved-event
                      (on-mouse-motion-event window saved-event t))
                    (return read-ahead-event)))))
      (loop
         for event = (glop:next-event window :blocking blocking)
         if (typep event 'glop:mouse-motion-event)
         do
           (setq event (compress-motion event))
         end
         if event
         do
           (glop:on-event window event)
           (when (typep event 'glop:close-event)
             (format t "Dropped ~D motion events~%" dropped)
             (return-from process-events nil))
         end))))

(defun mouse-to-viewport (window x y)
  (values (float x 1.0) (float (- (glop:window-height window) y) 1.0)))

(defclass viewer-dragger ()
  ((start-eye :initarg :start-eye)
   (start-look-at :initarg :start-look-at)))

(defclass trans-dragger (viewer-dragger lpsg-tinker::translate-dragger)
  ())

(defclass perspective-trans-dragger (trans-dragger)
  ((scale-factor)))

(defmethod initialize-instance :after ((obj perspective-trans-dragger) &key near)
  (with-slots (start-eye start-look-at scale-factor)
      obj
    (setf scale-factor (/ (sb-cga:vec-length (sb-cga:vec- start-look-at start-eye)) near))
    (format *terminal-io* "scale factor: ~S~%" scale-factor)))

(defclass rotate-dragger (viewer-dragger lpsg-tinker::rotate-dragger)
  ((start-up :initarg :start-up)))

(defun print-mouse-click (window x y)
  (let ((mouse-world (kit.math:unproject (sb-cga:vec x y 0.0)
                                         (lpsg-tinker:view-matrix (view-camera window))
                                         (lpsg:value (camera-choice window))
                                         (kit.math:vec4 0.0
                                                        0.0
                                                        (float (glop:window-width window) 1.0)
                                                        (float (glop:window-height window) 1.0)))))
    (format *terminal-io* "mouse: ~S~%" mouse-world)))

(defmethod glop:on-event :after ((window viewer-window) (event glop:button-press-event))
  (format t "Button ~S @ ~S, ~S~%" (glop:button event) (last-x window) (last-y window))
  (with-slots (current-dragger)
      window
    (multiple-value-bind (x y)
        (mouse-to-viewport window (last-x window) (last-y window))
      (let ((common-args (list :start-mouse-point (kit.math:vec2 x y)
                               :start-eye (lpsg-tinker:eye (view-camera window))
                               :start-look-at (lpsg-tinker:target (view-camera window))
                               :viewport (kit.math:vec4 0.0
                                                        0.0
                                                        (float (glop:window-width window) 1.0)
                                                        (float (glop:window-height window) 1.0))
                               :perspective-matrix (lpsg:value (camera-choice window))
                               :view-matrix (lpsg-tinker:view-matrix (view-camera window))))
            (button (glop:button event)))
        (cond ((and (eql button 2)
                    (eq (projection-type window) 'orthographic))
               (setq current-dragger (apply #'make-instance 'trans-dragger common-args)))
              ((eql button 2)
               (setq current-dragger (apply #'make-instance 'perspective-trans-dragger
                                            :near (lpsg-tinker:near (fov-camera window))
                                            common-args)))
              ((eql button 1)
               (setq current-dragger
                     (apply #'make-instance 'rotate-dragger
                            :arcball-center (lpsg-tinker:target (view-camera window))
                            :radius .8
                            :start-up (lpsg-tinker:up (view-camera window))
                            common-args)))
              (t nil)))
      (print-mouse-click window x y))))

(defgeneric transform-camera (window dragger x y))

;;; Helper function to scale the translation in perspective view

(defgeneric get-world-transform (window dragger event-x event-y))

(defmethod get-world-transform ((window viewer-window) (dragger trans-dragger) event-x event-y)
  (multiple-value-bind (x y)
      (mouse-to-viewport window event-x event-y)
    (lpsg-tinker::current-world-transform dragger (kit.math:vec2 x y))))

(defmethod get-world-transform :around ((window viewer-window) (dragger perspective-trans-dragger)
                                        event-x event-y)
  (let ((transform (call-next-method))
        (scale-factor (slot-value dragger 'scale-factor)))
    (sb-cga:matrix* (sb-cga:scale* scale-factor scale-factor scale-factor) transform)))

(defmethod transform-camera ((window viewer-window) (dragger trans-dragger) event-x event-y)
  (with-slots (start-eye start-look-at)
      dragger
    (let* ((transform (get-world-transform window dragger event-x event-y))
           (new-eye (sb-cga:transform-point start-eye transform))
           (new-look-at (sb-cga:transform-point start-look-at transform))
           (camera (view-camera window)))
      (lpsg-tinker:aim-camera camera new-eye new-look-at (lpsg-tinker:up camera)))))

(defmethod transform-camera ((window viewer-window) (dragger rotate-dragger) event-x event-y)
  (with-slots (start-eye start-look-at start-up)
      dragger
    (multiple-value-bind (x y)
        (mouse-to-viewport window event-x event-y)
      (let* ((transform (lpsg-tinker::current-world-transform dragger (kit.math:vec2 x y)))
             (new-eye (sb-cga:transform-point start-eye transform))
             (new-up (sb-cga:transform-direction start-up transform)))
        (lpsg-tinker:aim-camera (view-camera window) new-eye start-look-at new-up)))))

(defmethod on-mouse-motion-event :after ((window viewer-window) (event glop:mouse-motion-event)
                                         last-event-p)
  (declare (ignore last-event-p))
  (setf (last-x window) (glop:x event)
        (last-y window) (glop:y event))
  (with-slots (current-dragger)
      window
    (when current-dragger
      (transform-camera window current-dragger (last-x window) (last-y window)))))

(defmethod glop:on-event :after ((window viewer-window) (event glop:button-release-event))
  (with-slots (current-dragger)
      window
    (setf current-dragger nil)))


