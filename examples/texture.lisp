;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg-examples.texture)

;;; The graphics-state associated with the effect is stored in a "loader" class that is a mixin to
;;; the renderer.
(defclass texture-effect (simple-effect)
  ;; uset slots
  ((camera :input-accessor camera)
   (model :input-accessor model)
   (light :input-accessor light)
   (tex-sampler :input-accessor tex-sampler))
  (:metaclass compute-class)
  (:default-initargs :attribute-map '((gl:vertex . "in_Position")
                                      (gl:normal . "in_Normal")
                                      (gl:tex-coord . "in_TexCoord")
                                      (color . "in_Color"))))

(defmethod simple-effect-usets nconc ((effect texture-effect))
  (list (camera effect)
        (model effect)
        (light effect)
        (tex-sampler effect)))

;;; The uset objects are shared with the environments, and the upstream nodes. Calling the input
;;; slot accessor forces their update.

(defmethod update-effect progn ((effect texture-effect))
  (camera effect)
  (model effect)
  (light effect)
  (tex-sampler effect))

;;; Texture effect that uses a single checkerboard texture

;;; Source for the vertex and fragment shaders. This is pretty standard OpenGL.
(defparameter *vertex-shader-source* "
#version 330

in vec4 in_Position;
in vec3 in_Normal;
in vec3 in_Color;
in vec2 in_TexCoord;

smooth out vec3 theColor;
smooth out vec2 theTexCoord;

uniform vec4 lightDir;
uniform mat4 projectionMatrix;
uniform mat4 cameraMatrix;
uniform mat4 cameraMatrixInverse;
uniform mat4 modelMatrix;
uniform mat4 modelMatrixInverse;

void main()
{
  vec4 modelPos = cameraMatrix * (modelMatrix * in_Position);
  gl_Position = projectionMatrix * modelPos;
  // Multiply normal vector by the transposed inverse model view matrix
  vec3 vNormal = (vec4(in_Normal, 0.0) * modelMatrixInverse * cameraMatrixInverse).xyz;
  float intense = max(dot(in_Normal, -lightDir.xyz), 0.0);
  theColor = intense * in_Color;
  theTexCoord = in_TexCoord;
}
")

(defparameter *fragment-shader-source* "
#version 330

smooth in vec3 theColor;
smooth in vec2 theTexCoord;

out vec4 out_Color;

uniform sampler2D texSampler;

void main()
{
  vec4 texColor = texture(texSampler, theTexCoord);
  out_Color = vec4(theColor * texColor.rgb, 1.0);
}
")

(lpsg:define-uset light (("lightDir" :float-vec4 light-direction :accessor light-direction)))

(lpsg:define-uset tex-sampler (("texSampler" :sampler-2d tex-sampler :accessor tex-sampler)))

(defclass checker-effect-loader ()
  ((texture-area :accessor texture-area)
   (gl-state :accessor gl-state)
   (shader-program :accessor shader-program)
   (sampler :accessor sampler)))

(defmethod initialize-instance :after ((obj checker-effect-loader) &key)
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
                                                       :usets '(tex-sampler)))))
    (setf (sampler obj) (make-instance 'sampler :min-filter :linear-mipmap-linear))))

(defun make-checker-texture-area ()
  (let* ((data-size (* 64 64 3))
         (mem (cffi:foreign-alloc :uint8 :count data-size :initial-element 255)))
    ;; Make a cyan checkerboard
    (loop
       for j from 0 below 64
       do (loop
             for i from 0 below 64
             for idx = (* (+ (* j 64) i) 3)
             for row = (floor j 8)
             for col = (floor i 8)
             do (if (or (and (evenp row) (evenp col))
                        (and (not (evenp row)) (not (evenp col))))
                    (progn
                      (setf (cffi:mem-aref mem :uint8 idx) 0)
                      (setf (cffi:mem-aref mem :uint8 (+ idx 1)) 255)
                      (setf (cffi:mem-aref mem :uint8 (+ idx 2)) 255)))))
    (let* ((texture (make-instance 'texture-2d
                                   :target :texture-2d
                                   :width 64
                                   :height 64
                                   :internal-format :rgb8
                                   :pixel-format :rgb
                                   :data-type :unsigned-byte))
           (tex-area (make-instance 'raw-mirrored-texture-resource
                                    :texture texture
                                    :data mem
                                    :data-count data-size
                                    :width 64
                                    :height 64)))
      tex-area)))

(defmethod submit-with-effect :before
    (shape (renderer checker-effect-loader) (effect texture-effect))
  (declare (ignore shape))
  (unless (slot-boundp renderer 'gl-state)
    (let ((area (make-checker-texture-area)))
      (setf (texture-area renderer) area)
      (setf (gl-state renderer)
            (make-instance
             'graphics-state
             :program (shader-program renderer)
             :texunits (make-instance
                        'gl-texunits
                        :units (vector (make-instance 'gltexture-unit
                                                      :tex-object (texture area)
                                                      :sampler-object (sampler renderer))))))
      (schedule-upload renderer area)))
  (setf (gl-state effect) (gl-state renderer)))

(defparameter *default-camera-params* `(:eye ,(sb-cga:vec 1.0 1.0 0.0)
                                        :target ,(sb-cga:vec 0.0 0.0 -5.0)
                                        :up ,(sb-cga:vec 0.0 1.0 0.0)))

(defclass texture-window (checker-effect-loader viewer-window)
  ((view-camera :initform (apply #'make-instance 'partial-view-camera *default-camera-params*))
   (shapes :accessor shapes :initform nil)
   (visible-inputs :accessor visible-inputs :initform nil)
   (texture-source :accessor texture-source :initarg texture-source
                   :documentation "??? A pathname, perhaps?")
   (shape-type :accessor shape-type :initarg :shape-type))
  (:default-initargs :exposed nil :shape-type 'cube))

;;; Instances of usets
(defvar *model-uset* (make-instance 'model))
(defvar *light-uset* (make-instance 'light))
(defvar *sampler-uset* (make-instance 'tex-sampler))

(defvar *model-input* (make-instance 'lpsg:input-node :in *model-uset*))
(defvar *light-input* (make-instance 'lpsg:input-node :in *light-uset*))
(defvar *sampler-input* (make-instance 'lpsg:input-node :in *sampler-uset*))

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

;;; Assign colors and texture coordinates to the vertices of the cube.

(defun make-cube-with-attributes ()
  (let ((cube (lpsg-scene:make-cube-shape))
        (tex-coord-array (make-array '(24 2) :element-type 'single-float))
        (color-array (make-array '(24 3) :element-type 'single-float)))
    ;; Map each face to a square texture
    (loop
       for i from 0 below 6
       for face-base = (* i 4)
       do (loop
             for in-face from 0 below 4
             for u = (if (or (= in-face 0) (= in-face 3)) 0.0 1.0)
             for v = (if (>= in-face 2) 1.0 0.0)
             for tex-index = (+ face-base in-face)
             do (progn
                  (setf (aref tex-coord-array tex-index 0) u)
                  (setf (aref tex-coord-array tex-index 1) v))))
    ;; map each vertex to a color using the coordinates of the vertex
    (let ((coord-array (data (attribute cube 'gl:vertex))))
      (loop
         for i from 0 below 24
         do (loop
               for j from 0 below 3
               do (setf (aref color-array i j) (+ (aref coord-array i j) 0.5)))))
    (setf (attribute cube 'tex-coord)
          (make-instance 'vertex-attribute
                         :data tex-coord-array
                         :data-count 24
                         :components 2
                         :buffer-type :float))
    (setf (attribute cube 'color)
          (make-instance 'vertex-attribute
                         :data color-array
                         :data-count 24
                         :components 3
                         :buffer-type :float))
    cube))

(defun make-sphere-with-attributes ()
  (let* ((sphere-shape (lpsg-scene:make-sphere-shape))
         (vertex-count (lpsg::data-count (attribute sphere-shape 'gl:vertex)))
         (color-array (make-array (list vertex-count 3)
                                  :element-type 'single-float
                                  :initial-element 1.0)))
    (setf (attribute sphere-shape 'color)
          (make-instance 'vertex-attribute
                         :data color-array
                         :data-count vertex-count
                         :components 3
                         :buffer-type :float))
    sphere-shape))

(defun make-textured-shape (model-input allocator window)
  (let ((shape (if (eq (shape-type window) 'cube)
                   (make-cube-with-attributes)
                   (make-sphere-with-attributes)))
        (effect (make-instance 'texture-effect)))
    (setf (lpsg:effect shape) effect)
    (connect effect 'camera (camera-uset-node window) 'uset)
    (connect effect 'model model-input 'out)
    (connect effect 'light *light-input* 'out)
    (connect effect 'tex-sampler *sampler-input* 'out)
    ;; Allocate storage  in OpenGL buffer objects for the shape's geometry.
    (lpsg:compute-shape-allocation allocator shape)
    shape))

(defun submit-shape (window)
  (when (shapes window)
    (return-from submit-shape nil))
  (setf (shapes window) (make-array 1)
        (visible-inputs window) (make-array 1))
  (lpsg:with-allocator (allocator 'lpsg:interleaved-attribute-allocator)
    (loop
       for model-input in (list *model-input*)
       for i from 0
       for shape = (make-textured-shape model-input allocator window)
       for shape-visible = (make-instance 'lpsg:input-node :in t)
       do (progn
            (connect (lpsg:effect shape) 'lpsg:visiblep shape-visible 'lpsg:out)
            (setf (aref (shapes window) i) shape)
            (setf (aref (visible-inputs window) i) shape-visible)
            (lpsg:submit shape window)))))

(defun retract-shapes (window)
  (unless (shapes window)
    (return-from retract-shapes nil))
  (loop
     for shape across (shapes window)
     do (lpsg:retract shape window))
  (setf (shapes window) nil
        (visible-inputs window) nil))

(defmethod draw-window ((window texture-window))
  (draw window))

(defmethod glop:on-event :after ((window texture-window) (event glop:expose-event))
  (unless (exposed window)
    ;; Create a cube with correct face normals.
    (setf (model-matrix *model-uset*) (sb-cga:translate* 1.0 0.0 -5.0))
    (setf (model-matrix-inverse *model-uset*)
          (sb-cga:inverse-matrix (model-matrix *model-uset*)))
    (setf (light-direction *light-uset*) (compute-light-vector))
    (setf (tex-sampler *sampler-uset*) 0)
    (submit-shape window))
  (draw-window window))

(defmethod glop:on-event :after ((window texture-window) (event glop:resize-event))
  (when (exposed window)
    (draw-window window)))

(defmethod glop:on-event ((window texture-window) (event glop:key-event))
  (if (glop:pressed event)
      (case (glop:keysym event)
        (:p
         (setf (projection-type window)
               (if (eq (projection-type window) 'orthographic)
                   'perspective
                   'orthographic))
         (setf (in (camera-selector window)) (eq (projection-type window) 'orthographic))
         (draw-window window))
        (:1
         (let ((input-node (aref (visible-inputs window) 0)))
           (setf (lpsg:in input-node) (not (lpsg:out input-node))))
         (draw-window window))
        (:s
         (submit-shape window)
         (draw-window window))
        (:r
         (retract-shapes window)
         (draw-window window))
        (:g
         (tg:gc :full t)
         (draw-window window))
        (t
         (call-next-method)))
      (call-next-method)))

(defmethod on-mouse-motion-event :after ((window texture-window) (event glop:mouse-motion-event)
                                         last-event-p)
  (with-slots (current-dragger)
      window
    (when (and current-dragger last-event-p)
      (draw-window window))))

(defgeneric cleanup-window (win))

(defmethod cleanup-window ((win checker-effect-loader))
  (when (slot-boundp win 'texture-area)
    (let ((mem (data (texture-area win))))
      (cffi:foreign-free mem))))

(defun texture-example (&rest args)
  "Draw a textured object in a window.

The `p' key switches between orthographic and perspective views."
  (let* ((win (apply #'make-instance 'texture-window args)))
    (open-viewer win "texture demo" 800 600)
    (unwind-protect
         (progn
           (unless win
             (return-from texture-example nil))
           (process-events win))
      (when win
        (cleanup-window win)
        (glop:destroy-window win)))))
