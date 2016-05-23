;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg-examples.texture)

(defclass texture-effect (simple-effect)
  ((texture-area :accessor texture-area :initarg :texture-area)
   (sampler :accessor sampler :initarg :sampler)
   (shader-program :accessor shader-program :initform nil :allocation :class)
   (tex-loaded :accessor tex-loaded :initform nil))
  (:default-initargs :attribute-map '((gl:vertex . "in_Position")
                                      (gl:normal . "in_Normal")
                                      (texcoord . "in_TexCoord"))
    :uset-names '(lpsg-examples::camera lpsg-examples::model light tex-sampler)))

;;; Source for the vertex and fragment shaders. This is pretty standard OpenGL.
(defparameter *vertex-shader-source* "
#version 330

in vec4 in_Position;
in vec3 in_Normal;
// in vec3 in_Color;
in vec2 in_TexCoord;

smooth out vec3 theColor;
smooth out vec2 theTexCoord;

vec3 in_Color = vec3(1.0, 1.0, 1.0);

uniform vec4 lightDir;
uniform mat4 projectionMatrix;
uniform mat4 cameraMatrix;
uniform mat4 modelMatrix;

void main()
{
  vec4 modelPos = cameraMatrix * modelMatrix * in_Position;
  gl_Position = projectionMatrix * modelPos;
  // XXX Do the right thing with the normal!
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

(defmethod initialize-instance :after ((obj texture-effect) &key)
  (unless (shader-program obj)
    (setf (shader-program obj)
          (make-instance 'lpsg:program
                         :shaders (list (make-instance 'lpsg:shader
                                                       :shader-type :vertex-shader
                                                       :source *vertex-shader-source*
                                                       :usets
                                                       '(lpsg-examples::camera lpsg-examples::model light))
                                        (make-instance 'lpsg:shader
                                                       :shader-type :fragment-shader
                                                       :source *fragment-shader-source*
                                                       :usets '(tex-sampler)))))))


(defmethod submit-with-effect :before (shape renderer (effect texture-effect))
  (unless (slot-boundp effect 'gl-state)
    (setf (gl-state effect)
          (make-instance
           'graphics-state
           :program (shader-program effect)
           :texunits (make-instance
                      'lpsg::gl-texunits
                      :renderer renderer
                      :units (vector (make-instance 'lpsg::gltexture-unit
                                                    :tex-object (lpsg::texture (texture-area effect))
                                                    :sampler-object (sampler effect)))))))
  (unless (tex-loaded effect)
    (lpsg::schedule-upload renderer (texture-area effect))
    (setf (tex-loaded effect) t)))

(defparameter *default-camera-params* `(:eye ,(sb-cga:vec 1.0 1.0 0.0)
                                        :target ,(sb-cga:vec 0.0 0.0 -5.0)
                                        :up ,(sb-cga:vec 0.0 1.0 0.0)))

(defclass texture-window (viewer-window lpsg:standard-renderer)
  ((view-camera :initform (apply #'make-instance 'partial-view-camera *default-camera-params*))
   (effect :accessor effect)
   (shapes :accessor shapes :initform nil)
   (visible-inputs :accessor visible-inputs :initform nil)
   (texture-source :accessor texture-source :initarg texture-source
                   :documentation "??? A pathname, perhaps?"))
  (:default-initargs :exposed nil))

;;; Instances of usets
(defvar *model-uset* (make-instance 'lpsg-examples::model))
(defvar *light-uset* (make-instance 'light))
(defvar *sampler-uset* (make-instance 'tex-sampler))

(defvar *model-input* (make-instance 'lpsg:input-value-node :value *model-uset*))
(defvar *light-input* (make-instance 'lpsg:input-value-node :value *light-uset*))
(defvar *sampler-input* (make-instance 'lpsg:input-value-node :value *sampler-uset*))

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
  (let ((cube (make-cube-shape))
        (texcoord-array (make-array '(24 2) :element-type 'single-float))
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
                  (setf (aref texcoord-array tex-index 0) u)
                  (setf (aref texcoord-array tex-index 1) v))))
    ;; map each vertex to a color using the coordinates of the vertex
    (let ((coord-array (data (attribute cube 'gl:vertex))))
      (loop
         for i from 0 below 24
         do (loop
               for j from 0 below 3
               do (setf (aref color-array i j) (+ (aref coord-array i j) 0.5)))))
    (setf (attribute cube 'texcoord)
          (make-instance 'vertex-attribute
                         :data texcoord-array
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
    
(defun make-textured-shape (model-input allocator window)
  (let ((shape (make-cube-with-attributes)))
        (setf (lpsg:effect shape) (effect window))
    (setf (lpsg:input shape 'lpsg-examples::camera) (lpsg-examples::camera-uset-node window))
    (setf (lpsg:input shape 'lpsg-examples::model) model-input)
    (setf (lpsg:input shape 'light) *light-input*)
    (setf (input shape 'tex-sampler) *sampler-input*)
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
       for shape-visible = (make-instance 'lpsg:input-value-node :value t)
       do (progn
            (setf (lpsg:input shape 'lpsg:visiblep) shape-visible)
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

(defun make-texture-effect (window)
  ;;; create a texture test pattern
  (let* ((data-size (* 64 64 3))
         (mem (cffi:foreign-alloc :uint8 :count data-size :initial-element 255)))
    ;;; Make a cyan checkerboard
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
    (let* ((texture (make-instance 'lpsg::texture-2d
                                   :target :texture-2d
                                   :width 64
                                   :height 64
                                   :internal-format :rgb8
                                   :pixel-format :rgb
                                   :data-type :unsigned-byte))
           (tex-area (make-instance 'lpsg::raw-mirrored-texture-resource
                                    :texture texture
                                    :data mem
                                    :data-count data-size
                                    :width 64
                                    :height 64)))
      (make-instance 'texture-effect
                     :texture-area tex-area
                     :sampler (make-instance 'sampler :min-filter :linear-mipmap-linear)))))


(defmethod glop:on-event :after ((window texture-window) (event glop:expose-event))
  (unless (exposed window)
    ;; Create a cube with correct face normals.
    (let* ((effect (make-texture-effect window)))
      (setf (lpsg-examples::model-matrix *model-uset*) (sb-cga:translate* 1.0 0.0 -5.0))
      (setf (light-direction *light-uset*) (compute-light-vector))
      (setf (tex-sampler *sampler-uset*) 0)
      (setf (effect window) effect)
      (submit-shape window)))
  (draw-window window))

(defmethod glop:on-event :after ((window texture-window) (event glop:resize-event))
  (draw-window window))

(defmethod glop:on-event ((window texture-window) (event glop:key-event))
  (if (glop:pressed event)
      (case (glop:keysym event)
        (:p
         (setf (projection-type window)
               (if (eq (projection-type window) 'orthographic)
                   'perspective
                   'orthographic))
         (setf (lpsg:value (camera-selector window)) (eq (projection-type window) 'orthographic))
         (draw-window window))
        (:1
         (let ((input-node (aref (visible-inputs window) 0)))
           (setf (lpsg:value input-node) (not (lpsg:value input-node))))
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

(defmethod cleanup-window ((win texture-window))
  (let* ((effect (effect win))
         (mem (data (texture-area effect))))
    (cffi:foreign-free mem)
    (setf (shader-program effect) nil)
    t))

(defun texture-example (&rest args)
  "Draw a textured cube in a window.

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
