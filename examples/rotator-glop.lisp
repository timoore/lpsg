;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;

(in-package #:lpsg-examples)

(defclass renderer-window (glop:window)
  ((renderer :accessor renderer)
   (va :accessor vertex-array)
   (program :accessor program)
   (angle :accessor angle :initform 0.0)
   (animation-uset :accessor animation-uset)
   (projection-uset :accessor projection-uset)))

(defmethod glop:on-event ((window renderer-window) (event glop:key-event))
  (when (eq (glop:keysym event) :escape)
      (glop:push-close-event window))
  (when (and (glop:pressed event) (eq (glop:keysym event) :f))
    (glop:toggle-fullscreen window))
  (when (and (glop:pressed event) (eq (glop:keysym event) :g))
    (glop:set-fullscreen window)))

(defmethod glop:on-event ((window renderer-window) (event glop:button-event))
  (declare (ignore event)))

(defmethod glop:on-event ((window renderer-window) (event glop:mouse-motion-event))
  (declare (ignore event)))

(defun update-for-window-change (w event)
  (let ((width (glop:width event))
        (height (glop:height event)))
    (gl:viewport 0 0 (glop:width event) (glop:height event))
    ;; Ensure that projection matrix ratio always matches the window size ratio,
    ;; so the polygon will always look square.
    (let* ((right (max (float (/ width height)) 1.0))
           (top (max (float (/ height width)) 1.0))
           (ortho-mat (lpsg:ortho-matrix (- right) right (- top) top -1.0 1.0)))
      (lpsg:with-mutable (projection-uset w)
        (setf (projection-matrix (projection-uset w)) ortho-mat)))))

(defmethod glop:on-event ((window renderer-window) (event glop:resize-event))
  (update-for-window-change window event)
  (format t "Resize: ~Sx~S~%" (glop:width event) (glop:height event)))

(defmethod glop:on-event ((window renderer-window) (event glop:expose-event))
  (update-for-window-change window event)
  (format t "Expose~%"))

(defmethod glop:on-event ((window renderer-window) (event glop:close-event))
  (declare (ignore event))
  (format t "Close~%"))

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

(lpsg:define-uset animation (("angle" :float angle :accessor angle)))
(lpsg:define-uset projection (("projectionMatrix" :float-mat4
                               projection-matrix :accessor projection-matrix)))

(defun array-setup (p)
  (gl:enable-vertex-attrib-array 0)
  (gl:vertex-attrib-pointer 0 3 :float nil 0 p))

(defun setup-lpsg (w)
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
    (setf (animation-uset w) (make-instance 'animation))
    (setf (projection-uset w) (make-instance 'projection))
    ;; A program object is a collection of shader objects to be used
    ;; together in a single pipeline for rendering objects. To create a
    ;; program, you first create the individual shaders. Then you attach
    ;; the shaders to the program and link the program together.
    (let* ((geom (make-instance 'lpsg:geometry
                                :mode :triangles
                                :number-vertices 6
                                :indices iarr
                                :vertex-attributes #'array-setup
                                :vertex-data arr))
           (vs (make-instance 'lpsg::shader
                              :shader-type :vertex-shader
                              :source *shader-vao-vertex-program*
                              :usets '(animation projection)))
           (fs (make-instance 'lpsg::shader
                              :shader-type :fragment-shader
                              :source *shader-vao-fragment-program*
                              :usets '(animation)))
           (program (make-instance 'lpsg::program
                                   :shaders (list vs fs))))
      (setf (program w) program)
      (lpsg::gl-finalize program)
      (let ((state (make-instance 'lpsg:graphics-state
                                  :program program
                                  :uniform-sets (list (animation-uset w)
                                                      (projection-uset w)))))
        (lpsg:add-bundle (renderer w) (make-instance 'lpsg:render-bundle
                                                     :geometry geom
                                                     :gl-state state))))))
  

(defun rotator-glop ()
  (let ((win (glop:create-window "rotator test" 800 600
                                 :win-class 'renderer-window))
        (time (get-internal-real-time)))
    (unless win
      (return-from rotator-glop nil))
    (format t "GL Context version: ~a~%Renderer: ~a~%Vendor: ~a~%"
            (gl:get-string :version)
            (gl:get-string :renderer)
            (gl:get-string :vendor))
    (let ((ticks-per-sec (float internal-time-units-per-second 1.0d0)))
      (setup-lpsg win)
      ;; 6 revolutions per second
      (loop
         with start-time = (get-internal-real-time)
         while (glop:dispatch-events win :blocking nil :on-foo nil)
         do (let* ((current-time (/ (float (- (get-internal-real-time) start-time)
                                           1.0d0)
                                    ticks-per-sec))
                   (angle (mod (* current-time 6.0d0 2.0d0 pi)
                               (* 2.0d0 pi)))
                   (anim (animation-uset win)))
              (lpsg:with-mutable anim
                (setf (angle anim) (float angle 1.0)))
              (gl:clear :color-buffer)
              (lpsg:draw-render-groups (renderer win))
              (glop:swap-buffers win)))
      (glop:destroy-window win))))
