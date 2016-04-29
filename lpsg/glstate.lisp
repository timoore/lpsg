;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

(defclass texture-area ()
  ((level :accessor level :initarg :level :documentation "Mipmap level of this data in the texture.")
   (texture :accessor texture :initarg :texture :documentation "OpenGL texture object")
   (x-offset :accessor x-offset :initarg :x-offset
             :documentation "X offset of data in texture image.")
   (y-offset :accessor y-offset :initarg :y-offset
             :documentation "Y offset of data in texture image.")
   (width :accessor width :initarg :width :documentation "Width of image in pixels")
   (height :accessor height :initarg :height :documentation "Height of image in pixels")
   (generate-mipmap-p :accessor generate-mipmap-p :initarg :generate-mipmap-p
                      :documentation "If true (default), a mipmap will be generated in the texture."))
  (:default-initargs :level 0 :x-offset 0 :y-offset 0 :generate-mipmap-p t)
  (:documentation "Superclass for image data that will be stored in a texture."))

(define-gl-object texture ()
  ((target :accessor target :initarg :target)
   (internal-format :accessor internal-format :initarg :internal-format)
   (pixel-format :accessor pixel-format :initarg :pixel-format)
   (data-type :accessor data-type :initarg :data-type)
   (base-level :accessor base-level :initarg :base-level))
  (:default-initargs :base-level 0))

(defmethod gl-finalized-p ((obj texture))
  (gl-valid-p obj))

(defclass texture-2d (texture)
  ((width :accessor width :initarg :width :documentation "width of texture")
   (height :accessor height :initarg :height :documentation "height of texture"))
  (:documentation "class representing OpenGL Texture 2D"))

(defmethod gl-finalize ((obj texture-2d) &optional errorp)
  (declare (ignorable errorp))
  (setf (id obj) (gl:gen-texture))
  (gl:active-texture :texture0)
  (let ((target (target obj)))
    (gl:bind-texture (target obj) (id obj))
    (gl:tex-image-2d (target obj)
                     0                  ;XXX level?
                     (internal-format obj)
                     (width obj)
                     (height obj)
                     0                  ; border must be 0
                     (pixel-format obj)
                     (data-type obj)
                     (cffi:null-pointer))
    (gl:tex-parameter target :texture-base-level (base-level obj))
    (gl:bind-texture (target obj) 0)))

(defclass raw-mirrored-texture-resource (texture-area)
  ((data :accessor data :initarg :data :documentation "foreign memory containing the image data")
   (data-offset :accessor data-offset :initarg :data-offset
                :documentation "offset of image data in foreign memory")
   (data-count :accessor data-count :initarg :data-count :initform 0
               :documentation "number of elements")
   (row-alignment :accessor row-alignment :initarg :row-alignment
                  :documentation "alignment, in bytes, of each row of the image data"))
  (:default-initargs :data-offset 0 :row-alignment 1)
  (:documentation "Class for texture data stored in foreign memory."))

;;; Texture upload queue stuff

;;; TODO: Arrange queue by texture object. Push areas onto the end of the queue.
(defmethod add-to-upload-queue ((queue texture-upload-queue) (obj texture-area))
  (push obj (tex-queue queue)))

(defgeneric upload-texture (renderer texture-area texture))

(defmethod process-upload-queue :after (renderer (queue texture-upload-queue))
  (loop
     for area in (tex-queue queue)
     for texture = (texture area)
     do (upload-texture renderer area texture)
     finally (setf (tex-queue queue) nil)))

(defmethod upload-texture (renderer (area raw-mirrored-texture-resource) (texture texture-2d))
  (declare (ignore renderer))
  (gl:active-texture :texture0)
  (let ((target (target texture)))
    (gl:bind-texture target (id texture))
    (gl:pixel-store :unpack-alignment (row-alignment area))
    (gl:tex-sub-image-2d target
                         (level area)
                         (x-offset area)
                         (y-offset area)
                         (width area)
                         (height area)
                         (pixel-format texture)
                         (data-type texture)
                         (cffi:inc-pointer (data area) (data-offset area)))
    (when (generate-mipmap-p area)
      (gl:generate-mipmap target))
    (gl:bind-texture target 0)))

(define-gl-object sampler ()
  ((wrap-s :accessor wrap-s :initarg :wrap-s :documentation "wrap mode in the S dimension, a
  @c(cl-opengl) keyword")
   (wrap-t :accessor wrap-t :initarg :wrap-t :documentation "wrap mode in the T dimension, a
  @c(cl-opengl) keyword")
   (wrap-r :accessor wrap-r :initarg :wrap-r :documentation "wrap mode in the R dimension, a
  @c(cl-opengl) keyword")
   (border-color :accessor border-color :initarg :border-color
                 :documentation "the texture border color, an array of 4 floats.")
   (min-filter :accessor min-filter :initarg :min-filter
               :documentation "minification filter - a @c(cl-opengl) keyword")
   (mag-filter :accessor mag-filter :initarg :mag-filter
               :documentation "magniffication filter - a @c(cl-opengl) keyword")
   (min-lod :accessor min-lod :initarg :min-lod
            :documentation "minimum texture LOD level")
   (max-lod :accessor max-lod :initarg :max-lod
            :documentation "minimum texture LOD level")
   (lod-bias :accessor lod-bias :initarg :lod-bias :documentation "texture LOD bias")
   (compare-mode :accessor compare-mode :initarg :compare-mode
                 :documentation "comparison mode, for depth textures")
   (compare-func :accessor compare-func :initarg :compare-func
                 :documentation "comparison function, for depth textures"))
  (:default-initargs :wrap-s :repeat :wrap-t :repeat :wrap-r :repeat
                     :border-color #(0.0 0.0 0.0 0.0) :min-filter :nearest-mipmap-linear
                     :mag-filter :linear :min-lod -1000.0 :max-lod 1000.0 :lod-bias 0.0
                     :compare-mode :none :compare-func :lequal)
  (:documentation "class representing OpenGL sampler object, which controls texture filtering"))

(defmethod gl-finalize ((obj sampler) &optional errorp)
  (let ((id (gl:gen-sampler)))
    (setf (id obj) id)
    (gl:sampler-parameter id :texture-wrap-s (wrap-s obj))
    (gl:sampler-parameter id :texture-wrap-t (wrap-t obj))
    (gl:sampler-parameter id :texture-wrap-r (wrap-r obj))
    (gl:sampler-parameter id :texture-border-color (border-color obj))
    (gl:sampler-parameter id :texture-min-filter (min-filter obj))
    (gl:sampler-parameter id :texture-mag-filter (mag-filter obj))
    (gl:sampler-parameter id :texture-min-lod (min-lod obj))
    (gl:sampler-parameter id :texture-max-lod (max-lod obj))
    (gl:sampler-parameter id :texture-lod-bias (lod-bias obj))
    (gl:sampler-parameter id :texture-compare-mode (compare-mode obj))
    (gl:sampler-parameter id :texture-compare-func (compare-func obj))))

(define-protocol-class glstate-member ()
  ((:generic glstate-compare (m1 m2)
             (:documentation "Compares state members. Returns -1, 0, 1."))
   (:generic glstate-bind (renderer m previous))
   (:generic glstate-finalize (m))))

(defgeneric glstate-finalized-p (m)
  (:method-combination and))

;;; This is called only after all the other primary methods have returned 0, so the states must be
;;; equal.
(defmethod glstate-compare ((m1 glstate-member) m2)
  (declare (ignore m2))
  0)

(defclass glstate-program (glstate-member)
  ((program :accessor program :initarg :program :initform nil)))

(defun compare-num (x y)
  (cond ((= x y)
         0)
        ((< x y)
         -1)
        (t 1)))

(defun compare-gl-objects (obj1 obj2)
  (cond ((eq obj1 obj2)
         0)
        ((null obj1)
         -1)
        ((null obj2)
         1)
        (t
         (compare-num (id obj1) (id obj2)))))

(defmethod glstate-compare ((m1 glstate-program) (m2 glstate-program))
  (let ((result (compare-gl-objects (program m1) (program m2))))
    (if (zerop result)
        (call-next-method)
        result)))

(defmethod glstate-bind :after (renderer (m glstate-program) previous)
  (declare (ignorable renderer))
  (unless (and previous (eq (program m) (program previous)))
    (gl:use-program (id (program m)))))

(defmethod glstate-finalized-p and ((m glstate-program))
  (gl-finalized-p (program m)))

(defmethod glstate-finalize :after ((m glstate-program))
  (gl-finalize (program m)))

(defclass gltexture-unit ()
  ((tex-object :accessor tex-object :initarg :tex-object)
   (sampler-object :accessor sampler-object :initarg :sampler-object)))

(defun compare-texture-unit (unit1 unit2)
  (cond ((and unit1 unit2)
         (let ((tex-comp (compare-gl-objects (tex-object unit1) (tex-object unit2))))
           (if (zerop tex-comp)
               (compare-gl-objects (sampler-object unit1) (sampler-object unit2))
               tex-comp)))
        (unit1
         -1)
        (unit2
         1)
        (t 0)))

(defclass glstate-texunits (glstate-member)
  ((units :accessor units
          :documentation "An array of bindings for OpenGL's texture units. The members of the array
  can be NIL, representing no binding")))

(defmethod initialize-instance :after ((obj glstate-texunits) &key units renderer)
  (let ((max-tex-units (if renderer
                           (max-combined-texture-image-units (context-parameters renderer))
                           16)))
    (if (and units (= (length units) max-tex-units))
        (setf (units obj) units)
        (let ((new-units (make-array max-tex-units :initial-element nil)))
          (when units
            (setf (subseq new-units 0) units))
          (setf (units obj) new-units)))))

(defmethod glstate-compare ((m1 glstate-texunits) m2)
  (let* ((units1 (units m1))
         (units2 (units m2)))
    (loop
       for unit1 across units1
       for unit2 across units2
       for comp = (compare-texture-unit unit1 unit2)
       unless (zerop comp)
       do (return-from glstate-compare comp))
    (call-next-method)))

(defparameter *tex-unit-enums* (apply #'vector
                                      (loop
                                         for i from 0 below 32
                                         collect (intern (format nil "TEXTURE~d" i) :keyword))))

#|
(defun active-texture (num &optional (unit :texture0))
  (let ((texture0 (cffi:foreign-enum-value '%gl:enum unit)))
    (%gl:active-texture (+ texture0 num))))
|#

(defmethod glstate-bind :after (renderer (m glstate-texunits) previous)
  (declare (ignore renderer))
  (flet ((unbind-texture (unit)
           (when unit
             (gl:bind-texture (target (tex-object unit)) 0))))
    (let ((units (units m))
          (prev-units (and previous (units previous))))
      (loop
         with len = (length units)
         for i from 0 below len
         do (progn
              (gl:active-texture (svref *tex-unit-enums* i))
              (cond ((svref units i)
                     (let* ((unit (svref units i))
                            (tex-obj (tex-object unit))
                            (sampler-obj (sampler-object unit)))
                       (gl:bind-texture (target tex-obj) (id tex-obj))
                       (gl:bind-sampler i (id sampler-obj))))
                    ((and prev-units (svref prev-units i))
                     (unbind-texture (svref prev-units i)))
                    (t (gl:bind-texture :texture-2d 0))))))))

(defmethod glstate-finalized-p and ((m glstate-texunits))
  (let ((units (units m)))
    (unless units
      (return-from glstate-finalized-p t))
    (loop
       for unit across units
       when unit
       do (unless (and (gl-finalized-p (tex-object unit)) (gl-finalized-p (sampler-object unit)))
            (return-from glstate-finalized-p nil)))
    t))

(defmethod glstate-finalize :after ((m glstate-texunits))
  (loop
     for unit across (units m)
     do (when unit
          (gl-finalize (tex-object unit))
          (gl-finalize (sampler-object unit)))))

(defclass graphics-state (glstate-program glstate-texunits)
  ()
  (:documentation "Class that stores most OpenGL state."))

;;; Maybe use progn method combination as an alternative to these dummy primary methods? 
(defmethod glstate-finalize ((obj graphics-state))
  t)

(defmethod glstate-bind (renderer (obj graphics-state) previous)
  (declare (ignorable renderer previous))
  t)

(defmethod gl-finalized-p ((obj graphics-state))
  (glstate-finalized-p obj))

(defmethod gl-finalize ((obj graphics-state) &optional errorp)
  (declare (ignore errorp))
  (glstate-finalize obj))

(defgeneric bind-state (renderer state))

(defmethod bind-state ((renderer renderer) (state graphics-state))
  (with-slots (current-state)
      renderer
    (when (eq current-state state)
      (return-from bind-state nil))
    (glstate-bind renderer state current-state)
    (setf (current-state renderer) state)))

