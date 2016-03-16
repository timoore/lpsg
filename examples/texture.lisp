;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg-examples.texture)

(defclass texture-effect (simple-effect)
  ((texture :accessor texture)
   (sampler :accessor sampler)
   (shader-program :accessor shader-program :initform nil :allocation :class))
  (:default-initargs :attribute-map '((vertex . "in_Position")
                                      (normal . "in_Normal")
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

vec3 in_Color = vec3(1.0, 0.0, 1.0);

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
  (unless (gl-state effect)
    (setf (gl-state effect) (make-instance 'graphics-state
                                           :renderer renderer
                                           :program (shader-program effect)))
    (setf (svref (units (gl-state effect)) 0)
          (make-instance 'lpsg::gltexture-unit
                         :tex-object (texture effect)
                         :sampler-object (sampler effect)))))

(defparameter *default-camera-params* `(:eye ,(sb-cga:vec 1.0 1.0 0.0)
                                        :target ,(sb-cga:vec 0.0 0.0 -5.0)
                                        :up ,(sb-cga:vec 0.0 1.0 0.0)))

(defclass texture-window (viewer-window lpsg:renderer)
  ((view-camera :initform (apply #'make-instance 'partial-view-camera *default-camera-params*))
   (effect :accessor effect)
   (exposed :accessor exposed :initarg :exposed)
   (shapes :accessor shapes :initform nil)
   (current-dragger :initform nil)
   (texture-source :accessor texture-source :initarg texture-source
                   :documentation "??? A pathname, perhaps?"))
  (:default-initargs :exposed nil))

;;; Instances of usets
(defvar *model-uset* (make-instance 'lpsg-examples::model))
(defvar *light-uset* (make-instance 'light))

(defvar *model-input* (make-instance 'lpsg:input-value-node :value *model-uset*))
(defvar *light-input* (make-instance 'lpsg:input-value-node :value *light-uset*))

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
             for v from 0 to 1
             for vf = (float u 1.0)
             do (loop
                   for u from 0 to 1
                   for uf = (float v 1.0)
                   for tex-index = (+ face-base (* 2 v) u)
                   do (progn
                        (setf (aref texcoord-array tex-index 0) uf)
                        (setf (aref texcoord-array tex-index 1) vf)))))
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
    (setf (lpsg:input shape 'lpsg-examples::camera) (camera-uset-node window))
    (setf (lpsg:input shape 'lpsg-examples::model) model-input)
    (setf (lpsg:input shape 'light) *light-input*)
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
            (setf (lpsg:input shape 'lpsg:visiblep) cube-visible)
            (setf (aref (shapes window) i) cube)
            (setf (aref (visible-inputs window) i) cube-visible)
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
  (draw win))

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
             do (if (oddp (* i j))
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
           (tex-area (make-instance 'lpsg::raw-texture-resource
                                    :texture texture
                                    :data mem
                                    :data-count data-size
                                    :width 64
                                    :height 64))))))


(defmethod glop:on-event :after ((window texture-window) (event glop:expose-event))
  (unless (exposed window)
    ;; Create a cube with correct face normals.
    (let* ((effect (make-instance texture-effect
                                  )))
      (setf (model-matrix *model-uset*) (sb-cga:translate* 1.0 0.0 -5.0))
      (setf (light-direction *light-uset*) (compute-light-vector))
      (setf (effect window) effect)
      (submit-shape window)))
  (draw-window window))
