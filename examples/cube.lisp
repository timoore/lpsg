;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;

(in-package #:lpsg-examples)

(defparameter *vertex-shader-source* "
#version 330

in vec3 in_Position;
in vec3 in_Normal;
in vec3 in_color;

smooth out vec3 theColor;

vec4 lightDir(-0.577350, -0.577350, 0.577350, 0);

uniform mat4 projectionMatrix;

void main()
{
  gl_Position = projectionMatrix * vec4(in_Position, 1.0);
  vec3 intense = min(dot(-in_normal, lightDir), vec3(0.0, 0.0, 0.0));
  theColor = intense * in_color;
")

(defparameter *fragment-shader-source* "
#version 330

smooth in vec3 theColor;

out vec4 out_Color;

void main()
{
  out_Color = theColor;
}
")

(lpsg:define-uset projection (("projectionMatrix" :float-mat4
                               projection-matrix :accessor projection-matrix)))

(defparameter *shader-program*
  (make-instance 'lpsg:program
                 :shaders (list (make-instance 'lpsg:shader
                                               :shader-type :vertex-shader
                                               :source *vertex-shader-source*
                                               :usets (projection))
                                (make-instance 'lpsg:shader
                                               :shader-type :fragment-shader
                                               *source *fragment-shader-source
                                               :usets ()))))

(defclass cube-window (viewer-window renderer)
  ((cube :accessor cube)
   (effect :accessor effect)
   (exposed :accessor exposed :initarg :exposed))
  (:default-initargs :exposed nil))

(defmethod glop:on-event :after ((window cube-window) (event glop:expose-event))
  (setf (cube window) (make-cube-shape))
  (setf (effect window) (make-instance 'simple-effect :shader-program *shader-program*))
  (submit-with-effect (cube window) window (effect window))
  (setf (exposed window) t))

(defun cube-example ()
  (let* ((win (make-instance 'cube-window)))
    (open-viewer win "cube demo" 800 600)
    (unwind-protect
         (progn
           (unless win
             (return-from cube-example nil)
             (loop
                while (glop:dispatch-events win :blocking nil :on-foo nil)
                when (exposed win)
                do (progn
                     (gl:clear :color-buffer)
                     (lpsg:draw win)
                     (glop:swap-buffers win)))))
      (and (win (glop:destroy-window win))))))

