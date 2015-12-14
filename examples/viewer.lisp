;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;

(in-package #:lpsg-examples)

(defclass viewer-window (glop:window)
  ((ortho-screen-matrix :accessor ortho-screen-matrix
                        :initarg :ortho-screen-matrix
                        :initform (lpsg:identity-matrix))))

(defmethod glop:on-event ((window viewer-window) (event glop:key-event))
  (when (eq (glop:keysym event) :escape)
      (glop:push-close-event window))
  (when (and (glop:pressed event) (eq (glop:keysym event) :f))
    (glop:toggle-fullscreen window))
  (when (and (glop:pressed event) (eq (glop:keysym event) :g))
    (glop:set-fullscreen window)))

(defmethod glop:on-event ((window viewer-window) (event glop:button-event))
  (declare (ignore event)))

(defmethod glop:on-event ((window viewer-window) (event glop:mouse-motion-event))
  (declare (ignore event)))

(defgeneric update-for-window-change (w event))

(defmethod update-for-window-change ((w viewer-window) event)
  (let ((width (glop:width event))
        (height (glop:height event)))
    (gl:viewport 0 0 (glop:width event) (glop:height event))
    ;; Ensure that projection matrix ratio always matches the window size ratio,
    ;; so the polygon will always look square.
    (let* ((right (max (float (/ width height)) 1.0))
           (top (max (float (/ height width)) 1.0))
           (ortho-mat (lpsg:ortho-matrix (- right) right (- top) top 1.0 10.0)))
      (setf (ortho-screen-matrix w) ortho-mat))))

(defmethod glop:on-event ((window viewer-window) (event glop:resize-event))
  (update-for-window-change window event)
  (format t "Resize: ~Sx~S~%" (glop:width event) (glop:height event)))

(defmethod glop:on-event ((window viewer-window) (event glop:expose-event))
  (update-for-window-change window event)
  (format t "Expose~%"))

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

