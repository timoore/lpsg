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

;;; We support both orthographic and perspective cameras, which are placed with the same
;;; eye-target-up parameters. Therefore we build the different parts from the camera mixin classes
;;; and incremental nodes, and then route their outputs to a camera-uset-node which produces a uset
;;; as its value.

(defclass partial-view-camera (lpsg-tinker:aimed-camera-mixin lpsg-tinker:view-node-mixin)
  ())

(defclass partial-ortho-camera (lpsg-tinker:ortho-camera-mixin lpsg-tinker:projection-node-mixin)
  ())

(defclass partial-fov-camera (lpsg-tinker:fov-camera-mixin lpsg-tinker::projection-node-mixin)
  ())

;;; This incremental node takes 'view-matrix and 'projection-matrix as input and produces a uset.
(defclass camera-uset-node (lpsg:computation-node lpsg:computation-node-mixin lpsg:source-sink-mixin)
  ((uset :accessor uset :initform (make-instance 'camera))))

(defmethod lpsg:compute ((node camera-uset-node))
  (let ((uset (uset node)))
    (setf (camera-matrix uset) (lpsg:input-value node 'view-matrix))
    (setf (projection-matrix uset) (lpsg:input-value node 'projection-matrix))
    uset))

(defparameter *default-camera-params* `(:eye ,(sb-cga:vec 1.0 1.0 0.0)
                                        :target ,(sb-cga:vec 0.0 0.0 -5.0)
                                        :up ,(sb-cga:vec 0.0 1.0 0.0)))

(defclass cube-window (viewer-window lpsg:renderer)
  ((effect :accessor effect)
   (exposed :accessor exposed :initarg :exposed)
   (projection-type :accessor projection-type :initarg :projection-type)
   (cubes :accessor cubes :initform nil)
   (visible-inputs :accessor visible-inputs :initform nil)
   (view-camera :accessor view-camera
                :initform (apply #'make-instance 'partial-view-camera *default-camera-params*))
   (ortho-camera :accessor ortho-camera :initform (make-instance 'partial-ortho-camera))
   (fov-camera :accessor fov-camera :initform (make-instance 'partial-fov-camera))
   ;; if-then node for choosing the orthographic or perspective camera's projection matrix
   (camera-choice :accessor camera-choice)
   ;; An input-value node for holding T or NIL to select the type of camera.
   (camera-selector :accessor camera-selector)
   (camera-uset-node :accessor camera-uset-node :initform (make-instance 'camera-uset-node))
   (current-dragger :initform nil))
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
    (gl:finish)
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

(defun mouse-to-viewport (window x y)
  (values (float x 1.0) (float (- (glop:window-height window) y) 1.0)))

(defclass viewer-dragger ()
  ((start-eye :initarg :start-eye)
   (start-look-at :initarg :start-look-at)))

(defclass trans-dragger (viewer-dragger lpsg-tinker::translate-dragger)
  ())

(defclass perspective-trans-dragger (trans-dragger)
  ((scale-factor)))

(defmethod initialize-instance :after ((obj perspective-trans-dragger) &key near)
  (with-slots (start-eye start-look-at scale-factor)
      obj
    (setf scale-factor (/ (sb-cga:vec-length (sb-cga:vec- start-look-at start-eye)) near))
    (format *terminal-io* "scale factor: ~S~%" scale-factor)))

(defgeneric translate-camera (dragger mouse-coords))

(defmethod translate-camera ((dragger trans-dragger) mouse-coords)
  (let ((displacement (lpsg-tinker::current-displacement dragger mouse-coords)))
    (with-slots (start-eye start-look-at)
        dragger
      (values (sb-cga:vec- start-eye displacement) (sb-cga:vec- start-look-at displacement)))))

(defmethod translate-camera :around ((dragger perspective-trans-dragger) mouse-coords)
  (with-slots (scale-factor)
      dragger
    (let ((raw-displacement (call-next-method)))
      (sb-cga:vec* raw-displacement scale-factor))))

(defclass rotate-dragger (viewer-dragger lpsg-tinker::rotate-dragger)
  ((start-up :initarg :start-up)))

(defgeneric rotate-camera (dragger mouse-coords))

;;; Multiplication around a point is MtMrMt'*v. In order to transform the camera, we want to
;;; multiply by the inverse i.e., MtMr'Mt'.

(defmethod rotate-camera ((dragger rotate-dragger) mouse-coords)
  (let* ((dragger-transform (lpsg-tinker::current-world-transform dragger mouse-coords)))
    (with-slots (start-eye start-up)
        dragger
      (values (sb-cga:transform-point start-eye dragger-transform)
              (sb-cga:transform-direction start-up dragger-transform)))))

(defun print-mouse-click (window x y)
  (let ((mouse-world (kit.math:unproject (sb-cga:vec x y 0.0)
                                         (lpsg-tinker:view-matrix (view-camera window))
                                         (lpsg:value (camera-choice window))
                                         (kit.math:vec4 0.0
                                                        0.0
                                                        (float (glop:window-width window) 1.0)
                                                        (float (glop:window-height window) 1.0)))))
    (format *terminal-io* "mouse: ~S~%" mouse-world)))

(defmethod glop:on-event :after ((window cube-window) (event glop:button-press-event))
  (format t "Button ~S @ ~S, ~S~%" (glop:button event) (last-x window) (last-y window))
  (with-slots (current-dragger)
      window
    (multiple-value-bind (x y)
        (mouse-to-viewport window (last-x window) (last-y window))
      (let ((common-args (list :start-mouse-point (kit.math:vec2 x y)
                               :start-eye (lpsg-tinker:eye (view-camera window))
                               :start-look-at (lpsg-tinker:target (view-camera window))
                               :viewport (kit.math:vec4 0.0
                                                        0.0
                                                        (float (glop:window-width window) 1.0)
                                                        (float (glop:window-height window) 1.0))
                               :perspective-matrix (lpsg:value (camera-choice window))
                               :view-matrix (lpsg-tinker:view-matrix (view-camera window))))
            (button (glop:button event)))
        (cond ((and (eql button 2)
                    (eq (projection-type window) 'orthographic))
               (setq current-dragger (apply #'make-instance 'trans-dragger common-args)))
              ((eql button 2)
               (setq current-dragger (apply #'make-instance 'perspective-trans-dragger
                                            :near (lpsg-tinker:near (fov-camera window))
                                            common-args)))
              ((eql button 1)
               (setq current-dragger
                     (apply #'make-instance 'rotate-dragger
                            :arcball-center (lpsg-tinker:target (view-camera window))
                            :radius .8
                            :start-up (lpsg-tinker:up (view-camera window))
                            common-args)))
              (t nil)))
      (print-mouse-click window x y))))

(defgeneric transform-camera (window dragger x y))

(defmethod transform-camera ((window cube-window) (dragger trans-dragger) event-x event-y)
  (with-slots (start-eye start-look-at)
      dragger
    (multiple-value-bind (x y)
        (mouse-to-viewport window event-x event-y)
      (let* ((displacement (lpsg-tinker::current-displacement dragger (kit.math:vec2 x y)))
             (camera (view-camera window)))
        (lpsg-tinker:aim-camera camera
                                (sb-cga:vec- start-eye displacement)
                                (sb-cga:vec- start-look-at displacement)
                                (lpsg-tinker:up camera))))))

(defmethod transform-camera ((window cube-window) (dragger rotate-dragger) event-x event-y)
  (with-slots (start-look-at)
      dragger
    (multiple-value-bind (x y)
        (mouse-to-viewport window event-x event-y)
      (multiple-value-bind (new-eye new-up)
          (rotate-camera dragger (kit.math:vec2 x y))
        (lpsg-tinker:aim-camera (view-camera window) new-eye start-look-at new-up)))))

(defmethod glop:on-event :after ((window cube-window) (event glop:mouse-motion-event))
  (with-slots (current-dragger)
      window
    (when current-dragger
      (transform-camera window current-dragger (last-x window) (last-y window))
      (draw-window window))))

(defmethod glop:on-event :after ((window cube-window) (event glop:button-release-event))
  (with-slots (current-dragger)
      window
    (setf current-dragger nil)))


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

