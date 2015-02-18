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
  ((assembly :accessor assembly :initarg :assembly))
  (:default-initargs :assembly (make-instance 'assembly)))

(defvar *window* (make-instance 'cube-window))

(defun cube-example ()
  (let ((win (make-instance 'viewer-window))
        (cube (make-cube-shape)))
    (open-viewer win "cube demo" 800 600)))
