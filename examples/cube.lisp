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

(defclass camera-uset-node (lpsg:computation-node lpsg:computation-node-mixin lpsg:source-sink-mixin)
  ((uset :accessor uset :initform (make-instance 'camera))))

(defmethod lpsg:compute ((node camera-uset-node))
  (let ((uset (uset node)))
    (setf (camera-matrix uset) (lpsg:input-value node 'view-matrix))
    (setf (projection-matrix uset) (lpsg:input-value node 'projection-matrix))
    uset))

;;; We support orthographic and perspective cameras, so instead of using a complete camera, we
;;; build the different parts from mixin classes, and then route their outputs to the
;;; camera-uset-node.

(defclass partial-view-camera (lpsg-tinker:aimed-camera-mixin lpsg-tinker:view-node-mixin)
  ())

(defclass partial-ortho-camera (lpsg-tinker:ortho-camera-mixin lpsg-tinker:projection-node-mixin)
  ())

(defclass partial-fov-camera (lpsg-tinker:fov-camera-mixin lpsg-tinker::projection-node-mixin)
  ())

(defclass cube-window (viewer-window lpsg:renderer)
  ((effect :accessor effect)
   (exposed :accessor exposed :initarg :exposed)
   (projection-type :accessor projection-type :initarg :projection-type)
   (cubes :accessor cubes :initform nil)
   (visible-inputs :accessor visible-inputs :initform nil)
   (view-camera :accessor view-camera
                :initform (make-instance 'partial-view-camera
                                         :eye (sb-cga:vec 1.0 1.0 0.0)
                                         :target (sb-cga:vec 0.0 0.0 -5.0)
                                         :up (sb-cga:vec 0.0 1.0 0.0)))
   (ortho-camera :accessor ortho-camera :initform (make-instance 'partial-ortho-camera))
   (fov-camera :accessor fov-camera :initform (make-instance 'partial-fov-camera))
   (camera-choice :accessor camera-choice)
   (camera-selector :accessor camera-selector)
   (camera-uset-node :accessor camera-uset-node :initform (make-instance 'camera-uset-node)))
  (:default-initargs :exposed nil :projection-type 'orthographic))

(defmethod initialize-instance :after ((obj cube-window) &key)
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
          (lpsg-tinker:view-matrix-node (view-camera obj)))))

;;; Instances of usets
(defvar *model-uset* (make-instance 'model))
(defvar *light-uset* (make-instance 'light))
(defvar *model-uset2* (make-instance 'model))

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
          (lpsg-tinker:set-ortho-params (ortho-camera window) (- right) right (- top) top near far)
          (lpsg-tinker:set-perspective-params
           (fov-camera window) (/ (float pi 1.0) 4.0) (/ width height) near far)))))

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

(defun make-cube (model-input allocator window)
  (let ((cube (lpsg:make-cube-shape)))
    (setf (lpsg:effect cube) (effect window))
    (setf (lpsg:input cube 'camera) (camera-uset-node window))
    (setf (lpsg:input cube 'model) model-input)
    (setf (lpsg:input cube 'light) *light-input*)
    ;; Allocate storage  in OpenGL buffer objects for the cube's geometry.  Allocate an array
    ;; buffer and element buffer for each cube because we don't support gl:draw-elements-base-index
    ;; yet.
    (lpsg:compute-shape-allocation allocator cube)
    cube))

(defun submit-cubes (window)
  (when (cubes window)
    (return-from submit-cubes nil))
  (setf (cubes window) (make-array 2)
        (visible-inputs window) (make-array 2))
  (lpsg:with-allocator (allocator 'lpsg:interleaved-attribute-allocator)
    (loop
       for model-input in (list *model-input* *model-input2*)
       for i from 0
       for cube = (make-cube model-input allocator window)
       for cube-visible = (make-instance 'lpsg:input-value-node :value t)
       do (progn
            (setf (lpsg:input cube 'lpsg:visiblep) cube-visible)
            (setf (aref (cubes window) i) cube)
            (setf (aref (visible-inputs window) i) cube-visible)
            (lpsg:submit cube window)))))

(defun retract-cubes (window)
  (unless (cubes window)
    (return-from retract-cubes nil))
  (loop
     for cube across (cubes window)
     do (lpsg:retract cube window))
  (setf (cubes window) nil
        (visible-inputs window) nil))

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
      (setf (model-matrix *model-uset*) (sb-cga:translate* 1.0 0.0 -5.0))
      (setf (model-matrix *model-uset2*) (sb-cga:translate* -1.0 0.0 -5.0))
      (setf (light-direction *light-uset*) (compute-light-vector))
      (setf (effect window) effect)
      (submit-cubes window)))
  (setf (exposed window) t)
  (draw-window window))

(defmethod glop:on-event :after ((window cube-window) (event glop:resize-event))
  (compute-projection-matrix window (projection-type window) 1.0 10.0)
  (draw-window window))

(defmethod glop:on-event ((window cube-window) (event glop:key-event))
  (if (glop:pressed event)
      (case (glop:keysym event)
        (:p
         (setf (projection-type window)
               (if (eq (projection-type window) 'orthographic)
                   'perspective
                   'orthographic))
         (setf (lpsg:value (camera-selector window)) (eq (projection-type window) 'orthographic))
         (draw-window window))
        ((:1 :2)
         (let ((input-node (aref (visible-inputs window) (if (eq (glop:keysym event) :1)
                                                             0
                                                             1))))
           (setf (lpsg:value input-node) (not (lpsg:value input-node))))
         (draw-window window))
        (:s
         (submit-cubes window)
         (draw-window window))
        (:r
         (retract-cubes window)
         (draw-window window))
        (:g
         (tg:gc :full t)
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

