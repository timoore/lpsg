;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;

(in-package #:lpsg-examples)

;;; The graphics-state associated with the effect is stored in a "loader" class that is a mixin to
;;; the renderer.
(defclass cube-effect (simple-effect)
  ;; uset slots
  ((camera :input-accessor camera)
   (model :input-accessor model)
   (light :input-accessor light))
  (:metaclass compute-class)
  (:default-initargs :attribute-map '((gl:vertex . "in_Position")
                                      (gl:normal . "in_Normal"))))

(defmethod simple-effect-usets nconc ((effect cube-effect))
  (list (camera effect)
        (model effect)
        (light effect)))

;;; The uset objects are shared with the environments, and the upstream nodes. Calling the input
;;; slot accessor forces their update.

(defmethod update-effect progn ((effect cube-effect))
  (camera effect)
  (model effect)
  (light effect))

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

(lpsg:define-uset light (("lightDir" :float-vec4 light-direction :accessor light-direction)))

(defparameter *default-camera-params* `(:eye ,(sb-cga:vec 1.0 1.0 0.0)
                                        :target ,(sb-cga:vec 0.0 0.0 -5.0)
                                        :up ,(sb-cga:vec 0.0 1.0 0.0)))

(defclass cube-effect-loader ()
  ((gl-state :accessor gl-state)
   (shader-program :accessor shader-program)))

(defmethod initialize-instance :after ((obj cube-effect-loader) &key)
  (unless (slot-boundp obj 'shader-program)
    (setf (shader-program obj)
          (make-instance 'lpsg:program
                         :shaders (list (make-instance 'lpsg:shader
                                                       :shader-type :vertex-shader
                                                       :source *vertex-shader-source*
                                                       :usets
                                                       '(camera model light))
                                        (make-instance 'lpsg:shader
                                                       :shader-type :fragment-shader
                                                       :source *fragment-shader-source*
                                                       :usets nil))))))

(defmethod submit-with-effect :before
    (shape (renderer cube-effect-loader) (effect cube-effect))
  (declare (ignore shape))
  (unless (slot-boundp renderer 'gl-state)
    (setf (gl-state renderer) (make-instance 'graphics-state :program (shader-program renderer))))
  (setf (gl-state effect) (gl-state renderer)))

(defclass cube-window (cube-effect-loader viewer-window lpsg:standard-renderer)
  ((view-camera :initform (apply #'make-instance 'partial-view-camera *default-camera-params*))
   (effect :accessor effect)
   (cubes :accessor cubes :initform nil)
   (visible-inputs :accessor visible-inputs :initform nil))
  (:default-initargs :exposed nil))

;;; Instances of usets
(defvar *model-uset* (make-instance 'model))
(defvar *light-uset* (make-instance 'light))
(defvar *model-uset2* (make-instance 'model))

(defvar *model-input* (make-instance 'lpsg:input-node :in *model-uset*))
(defvar *light-input* (make-instance 'lpsg:input-node :in *light-uset*))
(defvar *model-input2* (make-instance 'lpsg:input-node :in *model-uset2*))

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
  (let ((cube (lpsg-scene:make-cube-shape))
        (effect (make-instance 'cube-effect)))
    (setf (lpsg:effect cube) effect)
    (lpsg:connect effect 'camera (camera-uset-node window) 'uset)
    (lpsg:connect effect 'model model-input 'out)
    (lpsg:connect effect 'light *light-input* 'out)
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
       for cube-visible = (make-instance 'lpsg:input-node :in t)
       do (progn
            (lpsg:connect (lpsg:effect cube) 'lpsg:visiblep cube-visible 'lpsg:out)
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

(defmethod draw-window ((win cube-window))
  (lpsg:draw win))

(defmethod glop:on-event :after ((window cube-window) (event glop:expose-event))
  (unless (exposed window)
    (setf (model-matrix *model-uset*) (sb-cga:translate* 1.0 0.0 -5.0))
    (setf (model-matrix *model-uset2*) (sb-cga:translate* -1.0 0.0 -5.0))
    (setf (light-direction *light-uset*) (compute-light-vector))
    (submit-cubes window))
  (draw-window window))

(defmethod glop:on-event :after ((window cube-window) (event glop:resize-event))
  (draw-window window))

(defmethod glop:on-event ((window cube-window) (event glop:key-event))
  (if (glop:pressed event)
      (case (glop:keysym event)
        (:p
         (setf (projection-type window)
               (if (eq (projection-type window) 'orthographic)
                   'perspective
                   'orthographic))
         (setf (lpsg:in (camera-selector window)) (eq (projection-type window) 'orthographic))
         (draw-window window))
        ((:1 :2)
         (let ((input-node (aref (visible-inputs window) (if (eq (glop:keysym event) :1)
                                                             0
                                                             1))))
           (setf (lpsg:in input-node) (not (lpsg:out input-node))))
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

(defmethod on-mouse-motion-event :after ((window cube-window) (event glop:mouse-motion-event)
                                         last-event-p)
  (with-slots (current-dragger)
      window
    (when (and current-dragger last-event-p)
      (draw-window window))))

(defun cube-example (&rest args)
  "Draw a cube in a window.

The `p' key switches between orthographic and perspective views."
  (let* ((win (apply #'make-instance 'cube-window args)))
    (open-viewer win "cube demo" 800 600)
    (unwind-protect
         (progn
           (unless win
             (return-from cube-example nil))
           (process-events win))
      (and win (glop:destroy-window win)))))

