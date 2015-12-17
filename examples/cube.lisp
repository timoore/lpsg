;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;

(in-package #:lpsg-examples)

(defparameter *vertex-shader-source* "
#version 330

in vec3 in_Position;
in vec3 in_Normal;
// in vec3 in_Color;

smooth out vec3 theColor;

vec3 in_Color = vec3(1.0, 0.0, 1.0);
vec4 lightDir = vec4(-0.577350, -0.577350, -0.577350, 0);

uniform mat4 projectionMatrix;

void main()
{
  // add a temporary model and view transform
  vec3 modelPos = in_Position + vec3(0.0, 0.0, -5.0) - vec3(0.25, 0.75, 0.0);
  gl_Position = projectionMatrix * vec4(modelPos, 1.0);
  float intense = max(dot(-in_Normal, lightDir.xyz), 0.0);
  theColor = intense * in_Color;
}
")

(defparameter *fragment-shader-source* "
#version 330

smooth in vec3 theColor;

out vec4 out_Color;

void main()
{
  out_Color = vec4(theColor, 1.0);
}
")

(lpsg:define-uset projection (("projectionMatrix" :float-mat4
                               projection-matrix :accessor projection-matrix)))

(defclass cube-window (viewer-window lpsg:renderer)
  ((cube :accessor cube)
   (effect :accessor effect)
   (exposed :accessor exposed :initarg :exposed)
   (projection-type :accessor projection-type :initarg :projection-type))
  (:default-initargs :exposed nil :projection-type 'orthographic))

(defvar *proj-uset* (make-instance 'projection))

(defun draw-window (win)
  (when (exposed win)
    (%gl:clear-color .8 .8 .8 1.0)
    (gl:cull-face :back)
    (gl:depth-func :less)
    (gl:enable :cull-face :depth-test)
    (gl:disable :dither)
    (gl:clear :color-buffer :depth-buffer)
    (lpsg:draw win)
    (glop:swap-buffers win)))

(defun compute-projection-matrix (window proj-type near far)
  (let ((width (glop:window-width window))
        (height (glop:window-height window)))
    (if (eq proj-type 'orthographic)
        (let* ((right (max (float (/ width height)) 1.0))
               (top (max (float (/ height width)) 1.0)))
          (lpsg:ortho-matrix (- right) right (- top) top 1.0 10.0))
        (lpsg:perspective (/ (float pi 1.0) 4.0) (/ width height) 1.0 10.0))))

(defmethod glop:on-event :after ((window cube-window) (event glop:expose-event))
  (unless (exposed window)
    (let* ((cube (lpsg:make-cube-shape))
           (shader-program
            (make-instance 'lpsg:program
                           :shaders (list (make-instance 'lpsg:shader
                                                         :shader-type :vertex-shader
                                                         :source *vertex-shader-source*
                                                         :usets '(projection))
                                          (make-instance 'lpsg:shader
                                                         :shader-type :fragment-shader
                                                         :source *fragment-shader-source*
                                                         :usets ()))))
           (env (make-instance 'lpsg:environment :program shader-program
                               :attribute-map '((gl:vertex . "in_Position")
                                                (gl:normal . "in_Normal"))
                               :uniform-sets (list *proj-uset*))))
      (setf (cube window) cube)
      (setf (projection-matrix *proj-uset*)
            (compute-projection-matrix window (projection-type window) 1.0 10.0))
      (setf (effect window) (make-instance 'lpsg:simple-effect :environment env))
      (let ((buffer (make-instance 'lpsg:gl-buffer)) ;default size should be fine
            (alloc-size (lpsg:compute-buffer-allocation cube)))
        (loop
           for (name . vertex-attrib) in (lpsg:attributes cube)
           do (setf (lpsg:buffer vertex-attrib) buffer))
        (setf (lpsg:buffer (lpsg::element-array (lpsg::drawable cube))) buffer))
      (lpsg:submit-with-effect cube window (effect window))))
  (setf (exposed window) t)
  (draw-window window))

(defmethod glop:on-event :after ((window cube-window) (event glop:resize-event))
  (setf (projection-matrix *proj-uset*)
        (compute-projection-matrix window (projection-type window) 1.0 10.0))
  (draw-window window))

(defmethod glop:on-event ((window cube-window) (event glop:key-event))
  (if (and (glop:pressed event) (eq (glop:keysym event) :p))
      (progn
        (setf (projection-type window)
              (if (eq (projection-type window) 'orthographic)
                  'perspective
                  'orthographic))
        (setf (projection-matrix *proj-uset*)
              (compute-projection-matrix window (projection-type window) 1.0 10.0))
        (draw-window window))
      (call-next-method)))

(defun cube-example (&rest args)
  (let* ((win (apply #'make-instance 'cube-window args)))
    (open-viewer win "cube demo" 800 600)
    (unwind-protect
         (progn
           (unless win
             (return-from cube-example nil))
           (loop
              while (glop:dispatch-events win :blocking t :on-foo nil)))
      (and win (glop:destroy-window win)))))

