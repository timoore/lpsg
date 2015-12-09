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
                                               :usets '(projection))
                                (make-instance 'lpsg:shader
                                               :shader-type :fragment-shader
                                               :source *fragment-shader-source*
                                               :usets ()))))

(defclass cube-window (viewer-window lpsg:renderer)
  ((cube :accessor cube)
   (effect :accessor effect)
   (exposed :accessor exposed :initarg :exposed))
  (:default-initargs :exposed nil))

(defvar *proj-uset* (make-instance 'projection))

(defmethod glop:on-event :after ((window cube-window) (event glop:expose-event))
  (let ((cube (lpsg:make-cube-shape))
        (env (make-instance 'lpsg:environment :program *shader-program*
                            :attribute-map '((gl:vertex . "in_Position")
                                             (gl:normal . "in_Normal")))))
    (setf (cube window) cube)
    (setf (projection-matrix *proj-uset*) (ortho-screen-matrix window))
    (setf (effect window) (make-instance 'lpsg:simple-effect :environment env))
    (let ((buffer (make-instance 'lpsg:gl-buffer)) ;default size should be fine
          (alloc-size (lpsg:compute-buffer-allocation cube)))
      (loop
         for (name . vertex-attrib) in (lpsg:attributes cube)
         do (setf (lpsg:buffer vertex-attrib) buffer)))
    (lpsg:submit-with-effect cube window (effect window)))
  (setf (exposed window) t))

(defun cube-example ()
  (let* ((win (make-instance 'cube-window)))
    (open-viewer win "cube demo" 800 600)
    (unwind-protect
         (progn
           (unless win
             (return-from cube-example nil))
           (loop
              while (glop:dispatch-events win :blocking nil :on-foo nil)
              when (exposed win)
              do (progn
                   (gl:clear :color-buffer)
                   (lpsg:draw win)
                   (glop:swap-buffers win))))
      (and win (glop:destroy-window win)))))

