;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Copyright (c) 2004, Oliver Markovic <entrox@entrox.org>
;;;   All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;;  o Redistributions of source code must retain the above copyright notice,
;;;    this list of conditions and the following disclaimer.
;;;  o Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;  o Neither the name of the author nor the names of the contributors may be
;;;    used to endorse or promote products derived from this software without
;;;    specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.
;;;
;;; Demo program copied from shader-vao.lisp in the cl-opengl sources.

(in-package #:lpsg-examples)

(defclass rotator (glut:window)
  ((renderer :accessor renderer)
   (va :accessor vertex-array)
   (program :accessor program)
   (angle :accessor angle :initform 0.0))
  (:default-initargs :width 500 :height 500 :pos-x 100 :pos-y 100
		     :mode '(:double :rgb :depth) :title "rotator.lisp"
		     :tick-interval (round 1000 60)))

(defvar *shader-vao-vertex-program*
  "#version 330

// The location is 0 because that's the vertex attribute we associate with vertex positions.
layout (location = 0) in vec3 in_Position;

uniform mat4 projectionMatrix;
uniform float angle;

// This is interpolated and used in the fragment shader.
smooth out vec2 pos;

void main()
{
  mat2 rotationMatrix = mat2(cos(angle), sin(angle), -sin(angle), cos(angle));
  float scaleFactor = 1.0 + 0.5 * sin(1.75 * angle);
  vec2 vertPos = scaleFactor * rotationMatrix * in_Position.xy;
  pos = vertPos * 5.0;

  gl_Position = projectionMatrix * vec4(vertPos, 0.0, 1.0); 
} 
")

(defvar *shader-vao-fragment-program*
  "#version 330

out vec4 out_Color;
smooth in vec2 pos;

uniform float angle;

void main() 
{
  mat2 rotationMatrix = mat2( cos(angle), sin(angle), -sin(angle), cos(angle) );
  vec2 rpos = mod(rotationMatrix * pos, 2.0 );
  
  if ((rpos.x > 1.0 && rpos.y > 1.0 ) || (rpos.x < 1.0 && rpos.y < 1.0))
    out_Color = vec4(0.1, 0.1, 0.1, 1.0); 
  else
    out_Color = vec4(0.5, 0.5, 0.7, 1.0);
} 
")

;;; Initialization 

(defun array-setup (p)
  (gl:enable-vertex-attrib-array 0)
  (gl:vertex-attrib-pointer 0 3 :float nil 0 p))

(defmethod glut:display-window :before ((w rotator))
  ;; Create a bundle and fill it with vertex attributes like position, etc.
  (unless (gl::features-present-p (>= :glsl-version 3.3))
    (glut:destroy-current-window)
    (return-from glut:display-window nil))
  (setf (renderer w) (make-instance 'lpsg:renderer))
  (let ((arr (gl:alloc-gl-array :float 12))
	(verts #(-0.5 -0.5 0.0 
		 -0.5 0.5 0.0 
		 0.5 -0.5 0.0 
		 0.5 0.5 0.0))
        (iarr (gl:alloc-gl-array :unsigned-short 6))
	(indexes #(0 2 1 1 2 3)))
    (dotimes (i (length verts))
      (setf (gl:glaref arr i) (aref verts i)))
    (dotimes (i (length indexes))
      (setf (gl:glaref iarr i) (aref indexes i)))
    (lpsg:add-bundle
     (renderer w)
     (make-instance 'lpsg:render-bundle
                    :geometry (make-instance 'lpsg:geometry
                                             :mode :triangles
                                             :number-vertices 6
                                             :indices iarr
                                             :vertex-attributes #'array-setup
                                             :vertex-data arr))))
  
  ;; A program object is a collection of shader objects to be used
  ;; together in a single pipeline for rendering objects. To create a
  ;; program, you first create the individual shaders. Then you attach
  ;; the shaders to the program and link the program together.
  (let* ((vs (make-instance 'lpsg::shader
                            :shader-type :vertex-shader
                            :source *shader-vao-vertex-program*))
         (fs (make-instance 'lpsg::shader
                            :shader-type :fragment-shader
                            :source *shader-vao-fragment-program*))
         (program (make-instance 'lpsg::program
                                 :shaders (list vs fs))))

    (setf (program w) program)
    (lpsg::gl-finalize program)
    ;; If we want to render using this program object, or add
    ;; uniforms, we need to use the program. This is similar to
    ;; binding a buffer.
    (gl:use-program (lpsg::id program))))

(defun get-location (program name)
  (cadr (assoc name (lpsg::uniforms program) :test #'equal)))

(defmethod glut:tick ((w rotator))
  (let ((seconds-per-revolution 6)
        (loc (get-location (program w) "angle"))) 
    (incf  (angle w)
	   (/ (* 2 pi) (* 60 seconds-per-revolution)))
    (gl:uniformf loc (angle w)))
  (glut:post-redisplay))

(defmethod glut:display ((w rotator))
  (gl:clear-color 0.0 0.0 0.2 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  
  ;; Since we never use any other program object, this is unnecessary
  ;; in this program. Typically, though, you'll have multiple program
  ;; objects, so you'll need to 'use' each one to activate it.
  (gl:use-program (lpsg::id (program w)))
  
  (lpsg:draw-render-groups (renderer w))
  (glut:swap-buffers))

(defmethod glut:reshape ((w rotator) width height)
  (gl:viewport 0 0 width height)
  ;; Ensure that projection matrix ratio always matches the window size ratio,
  ;; so the polygon will always look square.
  (let* ((right (max (float (/ width height)) 1.0))
         (top (max (float (/ height width)) 1.0))
         (ortho-mat (lpsg:ortho-matrix (- right) right (- top) top -1.0 1.0)))
    (when (program w)
      (let ((loc (get-location (program w) "projectionMatrix")))
        (gl:uniform-matrix loc 4 (vector ortho-mat) nil)))))
  
(defmethod glut:keyboard ((w rotator) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-current-window))))

;; Cleanup.
;; Most of the objects we created have analogous deletion function.
(defmethod glut:close ((w rotator))
  ;; Note: It doesn't matter whether we delete the program or the
  ;; linked shaders first. If a shader is linked to a program, the
  ;; shader isn't destroyed until after the program is
  ;; destroyed. Similarly, if the program is destroyed, the shaders
  ;; are detached.
  (when (slot-boundp w 'program)
   (gl:delete-program (lpsg::id (program w))))
  (lpsg:close-renderer (renderer w)))


(defun rotator ()
  (let ((w (make-instance 'rotator)))
    (unwind-protect
         (glut:display-window w)
      (when (not (glut::destroyed w))
         (setf (glut::destroyed w) t)
         (glut:destroy-window (glut:id w))))))
