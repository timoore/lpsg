;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;

(in-package #:lpsg-examples)

;;; Source for the vertex and fragment shaders. This is pretty standard OpenGL.
(defparameter *vertex-shader-source* "
#version 330

in vec4 in_Position;
in vec3 in_Normal;
// in vec3 in_Color;

smooth out vec3 theColor;

vec3 in_Color = vec3(1.0, 0.0, 1.0);

uniform vec4 lightDir;
uniform mat4 projectionMatrix;
uniform mat4 cameraMatrix;
uniform mat4 modelMatrix;

void main()
{
  vec4 modelPos = cameraMatrix * modelMatrix * in_Position;
  gl_Position = projectionMatrix * modelPos;
  float intense = max(dot(in_Normal, -lightDir.xyz), 0.0);
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

;;; Usets are sets of uniforms that can be set in shader programs. DEFINE-USET defines a CLOS class
;;; to hold values in lisp, as well as functions for uploading the values into a shader
;;; program. Note that the names in strings refer to uniforms in the above shaders.
(lpsg:define-uset camera (("projectionMatrix" :float-mat4
                                              projection-matrix :accessor projection-matrix)
                          ("cameraMatrix" :float-mat4
                                          camera-matrix :accessor camera-matrix)))

(lpsg:define-uset model (("modelMatrix" :float-mat4
                                        model-matrix :accessor model-matrix)))

(lpsg:define-uset light (("lightDir" :float-vec4 light-direction :accessor light-direction)))

(defclass cube-window (viewer-window lpsg:renderer)
  ((effect :accessor effect)
   (exposed :accessor exposed :initarg :exposed)
   (projection-type :accessor projection-type :initarg :projection-type)
   (cubes :accessor cubes :initform (make-array 2))
   (visible-inputs :accessor visible-inputs :initform (make-array 2)))
  (:default-initargs :exposed nil :projection-type 'orthographic))

;;; Instances of usets
(defvar *camera-uset* (make-instance 'camera))
(defvar *model-uset* (make-instance 'model))
(defvar *light-uset* (make-instance 'light))
(defvar *model-uset2* (make-instance 'model))

(defvar *camera-input* (make-instance 'lpsg:input-value-node :value *camera-uset*))
(defvar *model-input* (make-instance 'lpsg:input-value-node :value *model-uset*))
(defvar *light-input* (make-instance 'lpsg:input-value-node :value *light-uset*))
(defvar *model-input2* (make-instance 'lpsg:input-value-node :value *model-uset2*))

(defun draw-window (win)
  (when (exposed win)
    ;; All the OpenGL state set by theese calls will eventually be stored in a LPSG:GL-STATE
    ;; object.
    (%gl:clear-color .8 .8 .8 1.0)
    (gl:cull-face :back)
    (gl:depth-func :less)
    (gl:enable :cull-face :depth-test)
    (gl:disable :dither)
    (gl:clear :color-buffer :depth-buffer)
    ;; Initialize any OpenGL objects, upload any new data to OpenGL buffers, and draw all the
    ;; shapes.
    (lpsg:draw win)
    (glop:swap-buffers win)))

(defun compute-projection-matrix (window proj-type near far)
  (let ((width (glop:window-width window))
        (height (glop:window-height window)))
    (if (eq proj-type 'orthographic)
        (let* ((right (max (float (/ width height)) 1.0))
               (top (max (float (/ height width)) 1.0)))
          (kit.math:ortho-matrix (- right) right (- top) top near far))
        (kit.math:perspective-matrix (/ (float pi 1.0) 4.0) (/ width height) near far))))

(defun compute-view-matrix ()
  (kit.math:look-at (sb-cga:vec 1.0 1.0 0.0)
                    (sb-cga:vec 0.0 0.0 -5.0)
                    (sb-cga:vec 0.0 1.0 0.0)))

;;; Compute a high light, slightly to the side and front. This is the standard Lambert shading
;;; model, for diffuse shading only.

(defun compute-light-vector ()
  (let* ((angle (kit.math:deg-to-rad 15.0))
         (down (sb-cga:vec 0.0 -1.0 0.0))
         (mat (sb-cga:matrix* (sb-cga:rotate-around (sb-cga:vec 0.0 1.0 0.0) angle)
                              (sb-cga:rotate-around (sb-cga:vec 1.0 0.0 0.0) angle)))
         (light-vec3 (sb-cga:transform-direction down mat)))
    (kit.math:vec4 light-vec3)))

(defparameter *allocator* (make-instance 'lpsg:simple-allocator))

(defun make-cube (model-input allocator)
  (let ((cube (lpsg:make-cube-shape)))
    (setf (lpsg:input cube 'camera) *camera-input*)
    (setf (lpsg:input cube 'model) model-input)
    (setf (lpsg:input cube 'light) *light-input*)
    ;; Allocate storage  in OpenGL buffer objects for the cube's geometry.  Allocate an array
    ;; buffer and element buffer for each cube because we don't support gl:draw-elements-base-index
    ;; yet.
    (lpsg:compute-shape-allocation allocator cube)
    cube))

(defmethod glop:on-event :after ((window cube-window) (event glop:expose-event))
  (unless (exposed window)
    ;; Create a cube with correct face normals.
    (let* ((shader-program
            (make-instance 'lpsg:program
                           :shaders (list (make-instance 'lpsg:shader
                                                         :shader-type :vertex-shader
                                                         :source *vertex-shader-source*
                                                         :usets '(camera model light))
                                          (make-instance 'lpsg:shader
                                                         :shader-type :fragment-shader
                                                         :source *fragment-shader-source*
                                                         :usets ()))))
           ;; The shader program is the only OpenGL state we care about.
           (gl-state (make-instance 'lpsg:graphics-state :program shader-program))
           (effect (make-instance 'lpsg:simple-effect
                                  :gl-state gl-state
                                  :attribute-map '((gl:vertex . "in_Position")
                                                   (gl:normal . "in_Normal"))
                                  :uset-names '(camera model light))))
      ;; Initialize all the usets
      (setf (projection-matrix *camera-uset*)
            (compute-projection-matrix window (projection-type window) 1.0 10.0))
      (setf (camera-matrix *camera-uset*) (compute-view-matrix))
      (setf (model-matrix *model-uset*) (sb-cga:translate* 1.0 0.0 -5.0))
      (setf (model-matrix *model-uset2*) (sb-cga:translate* -1.0 0.0 -5.0))
      (setf (light-direction *light-uset*) (compute-light-vector))
      (setf (effect window) effect)
      (lpsg:with-allocator (allocator 'lpsg:interleaved-attribute-allocator)
        (loop
           for model-input in (list *model-input* *model-input2*)
           for i from 0
           for cube = (make-cube model-input allocator)
           for cube-visible = (make-instance 'lpsg:input-value-node :value t)
           do (progn
                (setf (lpsg:input cube 'lpsg::visiblep) cube-visible)
                (setf (aref (cubes window) i) cube)
                (setf (aref (visible-inputs window) i) cube-visible)
                (lpsg:submit-with-effect cube window (effect window)))))))
  (setf (exposed window) t)
  (draw-window window))

(defmethod glop:on-event :after ((window cube-window) (event glop:resize-event))
  (setf (projection-matrix *camera-uset*)
        (compute-projection-matrix window (projection-type window) 1.0 10.0))
  (setf (lpsg:value *camera-input*) *camera-uset*)
  (draw-window window))

(defmethod glop:on-event ((window cube-window) (event glop:key-event))
  (if (glop:pressed event)
      (case (glop:keysym event)
        (:p
         (setf (projection-type window)
               (if (eq (projection-type window) 'orthographic)
                   'perspective
                   'orthographic))
         (setf (projection-matrix *camera-uset*)
               (compute-projection-matrix window (projection-type window) 1.0 10.0))
         (setf (lpsg:value *camera-input*) *camera-uset*)
         (draw-window window))
        ((:1 :2)
         (let ((input-node (aref (visible-inputs window) (if (eq (glop:keysym event) :1)
                                                             0
                                                             1))))
           (setf (lpsg:value input-node) (not (lpsg:value input-node))))
         (draw-window window))
        (t
         (call-next-method)))
      (call-next-method)))

(defun cube-example (&rest args)
  "Draw a cube in a window.

The `p' key switches between orthographic and perspective views."
  (let* ((win (apply #'make-instance 'cube-window args)))
    (open-viewer win "cube demo" 800 600)
    (unwind-protect
         (progn
           (unless win
             (return-from cube-example nil))
           (loop
              while (glop:dispatch-events win :blocking t :on-foo nil)))
      (and win (glop:destroy-window win)))))

