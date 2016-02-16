;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;

(in-package #:lpsg-examples)

(defgeneric on-mouse-motion-event (window event last-event-p))

(defclass viewer-window (glop:window)
  (
   ;; For testing if a glop resize event is really a resize
   (saved-width :accessor saved-width)
   (saved-height :accessor saved-height)
   ;; Work around for lack of coordinates in button events
   (last-x :accessor last-x :initform 0)
   (last-y :accessor last-y :initform 0)))

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
        (saved-height window) (glop:height event)))

(defmethod glop:on-event ((window viewer-window) (event glop:expose-event))
  (update-for-window-change window event)
  (format t "Expose ~Sx~X~%" (glop:width event) (glop:height event)))

(defmethod glop:on-event ((window viewer-window) (event glop:close-event))
  (declare (ignore event))
  (format t "Close~%"))

(defmethod on-mouse-motion-event :after ((window viewer-window) (event glop:mouse-motion-event)
                                         last-event-p)
  (declare (ignore last-event-p))
  (setf (last-x window) (glop:x event)
        (last-y window) (glop:y event)))


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
  (:documentation "Process glop events for @cl:param{window}.

This calls "))

(defmethod process-events ((window viewer-window) &optional (blocking t))
  (loop
     for event = (glop:next-event window :blocking blocking)
     if event
     do (let ((saved-event nil))
          (when (typep event 'glop:mouse-motion-event)
            (setq saved-event event)
            (loop
               for read-ahead-event = (glop:next-event window :blocking nil)
               while (typep read-ahead-event 'glop:mouse-motion-event)
               do (progn
                    (on-mouse-motion-event window saved-event nil)
                    (setq saved-event read-ahead-event))
               finally (progn
                         (on-mouse-motion-event window saved-event t)
                         (setq event read-ahead-event))))
          (when event
            (glop:on-event window event)
            (when (typep event 'glop:close-event)
              (return-from process-events nil))))))
