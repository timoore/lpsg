;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

(defclass %gl-object ()
  ((%id :accessor %id :initform 0)))

(defclass gl-object-mixin ()
  ((gl-proxy :accessor gl-proxy :initarg :gl-proxy)))

(defmethod initialize-instance :after ((obj gl-object-mixin) &key (id 0 idp))
  (when idp
    (setf (id obj) id)))

(defmethod id ((obj gl-object-mixin))
  (%id (gl-proxy obj)))

(defmethod (setf id) (val (obj gl-object-mixin))
  (setf (%id (gl-proxy obj)) val))

(defmacro define-gl-object (name super-classes slots &rest options)
  (let* ((sym-name (symbol-name name))
         (proxy-name (intern (concatenate 'string "%" sym-name)))
         (init-mixin-name (intern (concatenate 'string
                                               "%"
                                               sym-name
                                               (symbol-name '#:-proxy-init-mixin)))))
    `(progn
       (defclass ,proxy-name (%gl-object) ())
       (defclass ,init-mixin-name () () (:default-initargs :gl-proxy (make-instance ',proxy-name)))
       (defclass ,name (,@super-classes gl-object ,init-mixin-name gl-object-mixin)
         ,slots
         ,@options))))

(defgeneric make-default-glstate-member (member-name)
  (:documentation "Create an instance of the default GL state for @cl:param(member-name)"))

;;; The members of the graphics state are stored in an a vector, for easy iteration over all the
;;; elements of the state. However, it is convenient to compose the actual graphics-state class
;;; from classes that each reference one part of the state; in this way we automatically get
;;; e.g. keyword arguments for the creation functions.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *glstate-element-keywords*
    '(:program
      :texunits
      :blend-func
      :blend-equation
      :blend-color
      :cull-face
      :depth-func
      :depth-range
      :front-face
      :framebuffers
      :viewport))
  (defparameter *glstate-elements*
    (let ((prefix (symbol-name '#:glstate-)))
      (mapcar (lambda (element)
                (intern (concatenate 'string
                                     prefix
                                     (symbol-name element))))
              *glstate-element-keywords*)))
  (defparameter *glstate-elements-number* (length *glstate-elements*))
  
  (defun glstate-element-priority (element-name)
    (position element-name *glstate-elements*)))

(defclass state-members-container ()
  ((state-members :accessor state-members
                  :initform (make-array *glstate-elements-number* :initial-element nil))))


(defmacro define-glstate-element (name glstate-name)
  (let ((priority (glstate-element-priority glstate-name)))
    `(progn
       (defclass ,glstate-name (state-members-container)
         ())
       (defgeneric ,glstate-name (state)
         (:method ((state ,glstate-name))
           (svref (state-members state) ,priority)))
       (defgeneric (setf ,glstate-name) (new-val state)
         (:method (new-val (state ,glstate-name))
           (setf (svref (state-members state) ,priority) new-val)))
       (defmethod initialize-instance :after ((obj ,glstate-name) &key ((,name val) nil supplied-p))
         (when supplied-p
           (setf (,glstate-name obj) val))))))

(macrolet ((define-all-elements ()
             `(progn ,@(mapcar (lambda (name glstate-name)
                                 `(define-glstate-element ,name ,glstate-name))
                               *glstate-element-keywords*
                               *glstate-elements*)))
           (define-glstate ()
             `(defclass graphics-state ,*glstate-elements*
                ((modes :accessor modes :initarg :modes)
                 (global-usets :accessor global-usets :initarg :global-usets :initform nil))
                (:documentation "Class that stores most OpenGL state."))))
  (define-all-elements)
  (define-glstate))

;;; Shader object and programs

(defclass gl-shader-source ()
  ((shader-type :accessor shader-type :initarg :shader-type )
   (source :accessor source :initarg :source :initform nil)))

(define-gl-object gl-shader (gl-shader-source)
  ((status :accessor status :initarg :status :documentation "status of shader compilation")
   (compiler-log :accessor compiler-log :initarg :compiler-log :initform nil
                 :documentation "log of shader compilation errors and warnings")))

(defmethod gl-finalized-p ((obj gl-shader))
  (slot-boundp obj 'status))

(defmethod gl-finalize ((obj gl-shader) &optional (errorp t))
  (let* ((src (source obj))
         (id (gl:create-shader (shader-type obj))))
    (setf (id obj) id)
    ;; XXX #defines for usets; comes from program
    (gl:shader-source id src)
    (gl:compile-shader id)
    (let ((status (gl:get-shader id :compile-status)))
      (setf (status obj) status)
      (unless status
        (setf (compiler-log obj) (gl:get-shader-info-log id))
        (when errorp
          (error 'render-error :gl-object obj :error-log (compiler-log obj)
                 :format-control "The shader ~S has compile errors.")))))
  t)

(defmethod gl-destroy ((obj %gl-shader))
  (gl:delete-shader (%id obj))
  (setf (%id obj) 0))

(define-gl-object gl-program ()
  ((shaders :accessor shaders :initarg :shaders :initform nil
            :documentation "shader objects that compose this program")
   ;; (name location type size)
   (uniforms :accessor uniforms :initform nil
             :documentation "Information on uniforms declared within
  the program shader source.") 
   (status :accessor status :initarg :status
           :documentation "status of shader program link")
   (link-log :accessor link-log :initarg :link-log :initform nil
             :documentation "log of errors and warnings from linking shader program")
   (vertex-attribs :accessor vertex-attribs :initform nil :documentation "private"))
  (:documentation "The representation of an OpenGL shader program."))

(defmethod gl-finalized-p ((obj gl-program))
  (slot-boundp obj 'status))

(defmethod gl-finalize ((obj gl-program) &optional (errorp t))
  (flet ((err (&rest args)
           (if errorp
               (apply #'error args)
               (return-from gl-finalize nil))))
    (let ((id (gl:create-program)))
      (setf (id obj) id)
      (with-slots (shaders)
          obj
        (loop
           for shader in shaders
           do (progn
                (gl-finalize shader errorp)
                (gl:attach-shader id (id shader)))))
      (gl:link-program id)
      (unless (setf (status obj) (gl:get-program id :link-status))
        (setf (link-log obj) (gl:get-program-info-log id))
        (err 'render-error
             :gl-object obj :error-log (link-log obj)
             :format-control "The program ~S has link errors."))
      ;; Info on the uniforms
      (loop
         with num-actives = (gl:get-program id :active-uniforms)
         for index from 0 below num-actives
         collecting (multiple-value-bind (size type name)
                        (gl:get-active-uniform id index)
                      (let ((location (gl:get-uniform-location id name)))
                        (unless (eql -1 location)
                          (list name location type size))))
         into uniforms
         finally (setf (uniforms obj) uniforms))
      (loop
         for (nil strategy) in (uset-alist obj)
         do (initialize-uset-strategy strategy obj))
      ;; Info on vertex attributes
      (loop
         with num-active-attribs = (gl:get-program id :active-attributes)
         for index from 0 below num-active-attribs
         collecting (multiple-value-bind (size type name)
                        (gl:get-active-attrib id index)
                      (let ((location (gl:get-attrib-location id name)))
                        (list name location type size)))
         into attributes
         finally (setf (vertex-attribs obj) attributes))
      t)))

(defmethod gl-destroy ((obj %gl-program))
  (gl:delete-program (%id obj))
  (setf (%id obj) 0))

(defmethod make-default-glstate-member ((name (eql 'glstate-program)))
  (make-instance 'gl-program :status t))

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
;;;; actual queue operations are in render.lisp.

(defgeneric upload-texture (renderer texture-area texture))

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


(defgeneric glstate-bind (tracker element previous-element))

(defgeneric glstate-compare (element-1 element-2)
  (:documentation "Compares state members. Returns -1, 0, 1."))

(defmethod gl-finalized-p ((obj graphics-state))
  (loop
     for element across (state-members obj)
     when element
     do (unless (gl-finalized-p element)
          (return-from gl-finalized-p nil)))
  t)

(defmethod gl-finalize ((obj graphics-state) &optional errorp)
  (loop
     for element across (state-members obj)
     when element
     do (gl-finalize element errorp)))

 ;;;

(defun compare-num (x y)
  (cond ((= x y)
         0)
        ((< x y)
         -1)
        (t 1)))

(defmacro compare-pairs (&rest pairs)
  "Call compare-num on each pair of arguments, but short-circuit and return the value if the result
  of the comparison is not 0."
  (if (null pairs)
      0
      (destructuring-bind (a b &rest rest-args)
          pairs
        (let ((result (gensym)))
          `(let ((,result (compare-num ,a ,b)))
             (if (zerop ,result)
                 (compare-pairs ,@rest-args)
                 ,result))))))

(defun compare-gl-objects (obj1 obj2)
  (cond ((eq obj1 obj2)
         0)
        ((null obj1)
         -1)
        ((null obj2)
         1)
        (t
         (compare-num (id obj1) (id obj2)))))

(defmacro compare-object-pairs (&rest pairs)
  "Call compare-gl-objects on each pair of arguments, but short-circuit and return the value if the
result of the comparison is not 0."
  (if (null pairs)
      0
      (destructuring-bind (a b &rest rest-args)
          pairs
        (let ((result (gensym)))
          `(let ((,result (compare-gl-objects ,a ,b)))
             (if (zerop ,result)
                 (compare-gl-objects ,@rest-args)
                 ,result))))))

(defmethod glstate-compare ((m1 gl-program) (m2 gl-program))
  (compare-gl-objects m1 m2))

(defmethod glstate-bind (tracker (m gl-program) previous)
  (declare (ignorable tracker))
  (unless (and previous (eq m  previous))
    (gl:use-program (id m))))

(defclass gltexture-unit ()
  ((tex-object :accessor tex-object :initarg :tex-object
               :documentation "The texture bound to a unit.")
   (sampler-object :accessor sampler-object :initarg :sampler-object
                   :documentation "The sampler object bound to a unit."))
  (:documentation "The state of a single OpenGL texture (actually, image) unit."))

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

(defclass gl-texunits ()
  ((units :accessor units :initarg :units :initform #()
          :documentation "An array of bindings for OpenGL's texture units. The members of the array
  can be NIL, representing no binding")))

(defmethod glstate-compare ((m1 gl-texunits) m2)
  (let* ((units1 (units m1))
         (units2 (units m2))
         (result (compare-num (length units1) (length units2))))
    (unless (zerop result)
      (return-from glstate-compare result))
    (loop
       for unit1 across units1
       for unit2 across units2
       for comp = (compare-texture-unit unit1 unit2)
       unless (zerop comp)
       do (return-from glstate-compare comp))))

(defparameter *tex-unit-enums* (apply #'vector
                                      (loop
                                         for i from 0 below 32
                                         collect (intern (format nil "TEXTURE~d" i) :keyword))))

(defmethod glstate-bind (tracker (m gl-texunits) previous-units)
  (declare (ignore tracker))
  (flet ((unbind-texture (unit)
           (when unit
             (gl:bind-texture (target (tex-object unit)) 0)))
         (safe-unit (units i)
           (if (< i (length units))
               (svref units i)
               nil)))
    (loop
       with units = (units m)
       with prev-units = (and previous-units (units previous-units))
       with len = (max (min (length units) (length prev-units))
                       (max-combined-texture-image-units (context-parameters tracker)))
       for i from 0 below len
       for unit = (safe-unit units i)
       for prev-unit = (safe-unit prev-units i)
       if (or unit prev-unit)
       do 
         (gl:active-texture (svref *tex-unit-enums* i))
         (cond (unit
                (let* ((tex-obj (tex-object unit))
                       (sampler-obj (sampler-object unit)))
                  (gl:bind-texture (target tex-obj) (id tex-obj))
                  (gl:bind-sampler i (id sampler-obj))))
               (prev-unit
                (unbind-texture prev-unit))
               (t (gl:bind-texture :texture-2d 0)))
       end)))

(defmethod gl-finalized-p ((m gl-texunits))
  (loop
     for unit across (units m)
     when unit
     do (unless (and (gl-finalized-p (tex-object unit)) (gl-finalized-p (sampler-object unit)))
          (return-from gl-finalized-p nil)))
  t)

(defmethod gl-finalize ((m gl-texunits) &optional errorp)
  (declare (ignore errorp))
  (loop
     for unit across (units m)
     do (when unit
          (gl-finalize (tex-object unit))
          (gl-finalize (sampler-object unit)))))

(defmethod make-default-glstate-member ((name (eql 'glstate-texunits)))
  (make-instance 'gl-texunits))

(defun get-gl-enum (val error-msg)
  (cond ((keywordp val)
         (cffi:foreign-enum-value 'cl-opengl-bindings:enum val))
        ((integerp val)
         val)
        (t (error error-msg val))))

(defclass gl-depth-func ()
  ((func :accessor func)))

(defmethod initialize-instance :after ((obj gl-depth-func) &key (func :less))
  (setf (func obj) (get-gl-enum func "~S is not a valid value for depth-func.")))

(defmethod glstate-bind (tracker (element gl-depth-func) previous)
  (%gl:depth-func (func element)))

(defmethod glstate-compare ((e1 gl-depth-func) (e2 gl-depth-func))
  (compare-num (func e1) (func e2)))

(defmethod make-default-glstate-member ((name (eql 'glstate-depth-func)))
  (make-instance 'gl-depth-func))

(defclass gl-depth-range ()
  ((near :accessor near :initarg :near :initform 0.0)
   (far :accessor far :initarg :far :initform 1.0)))

(defmethod glstate-bind (tracker (element gl-depth-range) previous)
  (declare (ignore tracker previous))
  (gl:depth-range (near element) (far element)))

(defmethod glstate-compare ((e1 gl-depth-range) (e2 gl-depth-range))
  (compare-pairs
   (near e1) (near e2)
   (far e1) (far e2)))

(defmethod make-default-glstate-member ((name (eql 'glstate-depth-range)))
  (make-instance 'gl-depth-range))

(defclass gl-blend-func ()
  ((src-rgb :accessor src-rgb)
   (dst-rgb :accessor dst-rgb)
   (src-alpha :accessor src-alpha)
   (dst-alpha :accessor dst-alpha)))

(defmethod initialize-instance :after ((obj gl-blend-func)
                                       &key
                                         (src-rgb :one) (dst-rgb :zero)
                                         (src-alpha src-rgb) (dst-alpha dst-rgb))
  (flet ((get-enum (enum)
           (get-gl-enum enum "~S is not a valid value for gl-blend-func.")))
    (setf (src-rgb obj) (get-enum src-rgb))
    (setf (dst-rgb obj) (get-enum dst-rgb))
    (setf (src-alpha obj) (get-enum src-alpha))
    (setf (dst-alpha obj) (get-enum dst-alpha))))

(defmethod glstate-bind (tracker (element gl-blend-func) previous)
  (declare (ignore tracker previous))
  (%gl:blend-func-separate (src-rgb element)
                           (dst-rgb element)
                           (src-alpha element)
                           (dst-alpha element)))

(defmethod glstate-compare ((e1 gl-blend-func) (e2 gl-blend-func))
  (compare-pairs
   (src-rgb e1) (src-rgb e2)
   (dst-rgb e1) (dst-rgb e2)
   (src-alpha e1) (src-alpha e2)
   (dst-alpha e1) (dst-alpha e2)))

(defmethod make-default-glstate-member ((name (eql 'glstate-blend-func)))
  (make-instance 'gl-blend-func))

(defclass gl-blend-equation ()
  ((equation :accessor equation)))

(defmethod initialize-instance :after ((obj gl-blend-equation) &key (equation :func-add))
  (unless (slot-boundp obj 'equation)
    (setf (equation obj) (get-gl-enum equation "~S is not a valid value for gl-blend-equation."))))

(defmethod glstate-bind (tracker (element gl-blend-equation) previous)
  (declare (ignore tracker previous))
  (%gl:blend-equation (equation element)))

(defmethod glstate-compare ((e1 gl-blend-equation) (e2 gl-blend-equation))
  (compare-num (equation e1) (equation e2)))

(defmethod make-default-glstate-member ((name (eql 'glstate-blend-equation)))
  (make-instance 'gl-blend-equation))

(defclass gl-blend-color ()
  ((rgba :accessor rgba :initarg :rgba :initform #(0.0 0.0 0.0 0.0))))

(defmethod glstate-bind (tracker (element gl-blend-color) previous)
  (declare (ignore tracker previous))
  (let ((color (rgba element)))
    (%gl:blend-color (svref color 0) (svref color 1) (svref color 2) (svref color 3))))

(defmethod glstate-compare ((e1 gl-blend-color) (e2 gl-blend-color))
  (loop
     for c1 across (rgba e1)
     for c2 across (rgba e2)
     for comp = (compare-num c1 c2)
     unless (zerop comp)
     return comp
     finally (return 0)))

(defmethod make-default-glstate-member ((name (eql 'glstate-blend-color)))
  (make-instance 'gl-blend-color))

(defclass gl-cull-face ()
  ((face :accessor face)))

(defmethod initialize-instance :after ((obj gl-cull-face) &key (face :back))
  (setf (face obj) (get-gl-enum face "~S is not a valid value for gl-cull-face.")))

(defmethod glstate-bind (tracker (element gl-cull-face) previous)
  (declare (ignore tracker previous))
  (%gl:cull-face (face element)))

(defmethod glstate-compare ((e1 gl-cull-face) (e2 gl-cull-face))
  (compare-num (face e1) (face e2)))

(defmethod make-default-glstate-member ((name (eql 'glstate-cull-face)))
  (make-instance 'gl-cull-face))

(defclass gl-front-face ()
  ((mode :accessor mode)))

(defmethod initialize-instance :after ((obj gl-front-face) &key (mode :ccw))
  (setf (mode obj) (get-gl-enum mode "~S is not a valid value for gl-front-face.")))

(defmethod glstate-bind (tracker (element gl-front-face) previous)
  (declare (ignore tracker previous))
  (%gl:front-face (mode element)))

(defmethod glstate-compare ((e1 gl-front-face) (e2 gl-front-face))
  (compare-num (mode e1) (mode e2)))

(defmethod make-default-glstate-member ((name (eql 'glstate-front-face)))
  (make-instance 'gl-front-face))

(define-gl-object renderbuffer ()
  ((internal-format :accessor internal-format :initarg :internal-format)
   (width :accessor width :initarg :width :documentation "width of renderbuffer")
   (height :accessor height :initarg :height :documentation "height of renderbuffer")
   (samples :accessor samples :initarg :samples :initform 0)))

(defmethod gl-finalized-p ((obj renderbuffer))
  (gl-valid-p obj))

(defmethod gl-finalize ((obj renderbuffer) &optional errorp)
  (declare (ignorable errorp))
  (setf (id obj) (gl:gen-renderbuffer))
  (gl:bind-renderbuffer :renderbuffer (id obj))
  (if (zerop (samples obj))
      (gl:renderbuffer-storage :renderbuffer (internal-format obj) (width obj) (height obj))
      (%gl:renderbuffer-storage-multisample :renderbuffer (samples obj)
                                            (internal-format obj) (width obj) (height obj))))


(defclass gl-framebuffers ()
  ((read-fbo :accessor read-fbo :initarg :read-fbo :initform nil)
   (draw-fbo :accessor draw-fbo :initarg :draw-fbo :initform nil)))

(defgeneric is-default-p (gl-obj))

(defmethod is-default-p ((gl-obj gl-framebuffers))
  (and (null (read-fbo gl-obj))
       (null (draw-fbo gl-obj))))

(defmethod gl-finalize ((obj gl-framebuffers) &optional errorp)
  (let ((read-fbo (read-fbo obj))
        (draw-fbo (draw-fbo obj)))
    (when read-fbo
      (gl-finalize read-fbo errorp))
    (when draw-fbo
      (gl-finalize draw-fbo errorp))))

(defmethod gl-finalized-p ((obj gl-framebuffers))
  (let ((read-fbo (read-fbo obj))
        (draw-fbo (draw-fbo obj)))
    (and (or (null read-fbo)
             (gl-finalized-p read-fbo))
         (or (null draw-fbo)
             (gl-finalized-p draw-fbo)))))

(defmethod glstate-bind (tracker (element gl-framebuffers) previous)
  (declare (ignore tracker previous))
  (let ((read-fbo (read-fbo element))
        (draw-fbo (draw-fbo element)))
    (%gl:bind-framebuffer :read-framebuffer (if read-fbo
                                                (id read-fbo)
                                                0))
    (%gl:bind-framebuffer :draw-framebuffer (if draw-fbo
                                                (id draw-fbo)
                                                0))))

(defmethod glstate-compare ((e1 gl-framebuffers) (e2 gl-framebuffers))
  (compare-object-pairs
   (read-fbo e1) (read-fbo e2)
   (draw-fbo e1) (draw-fbo e2)))

(defmethod make-default-glstate-member ((name (eql 'glstate-framebuffers)))
  (make-instance 'gl-framebuffers))

(define-gl-object framebuffer-object ()
  ((color-attachments :accessor color-attachments :initarg :color-attachments :initform nil
                      :documentation "plist of attachment names and attachment specs. An attachment
spec is a texture or renderbuffer object, or a list of arguments for
@c(attach-framebuffer-texture)")
   (depth-attachment :accessor depth-attachment :initarg :depth-attachment :initform nil)
   (depth-stencil-attachment :accessor depth-stencil-attachment :initarg
                             :depth-stencil-attachment :initform nil)
   (gl-draw-buffers :accessor gl-draw-buffers :initarg :gl-draw-buffers
                    :initform '(:color-attachment0))))

(defgeneric attach-framebuffer-texture (fb-target attachment-point texture &key))

(defmethod attach-framebuffer-texture (fb-target attachment-point (texture texture-2d)
                                       &key (level 0))
  (gl:framebuffer-texture-2d fb-target attachment-point (target texture) (id texture) level))

(defmethod attach-framebuffer-texture (fb-target attachment-point (buffer renderbuffer) &key)
  (gl:framebuffer-renderbuffer fb-target attachment-point :renderbuffer (id buffer)))

(defmethod gl-finalize ((obj framebuffer-object) &optional errorp)
  (flet ((get-attachment (tex)
           (if (listp tex)
               (car tex)
               tex))
         (attach (fb-target attachment attachment-spec)
           (if (listp attachment-spec)
               (apply #'attach-framebuffer-texture fb-target attachment attachment-spec)
               (attach-framebuffer-texture fb-target attachment attachment-spec))))
    (loop
       for tail on (color-attachments obj) by #'cddr
       for (nil color-attachment) = tail
       do (gl-finalize (get-attachment color-attachment) errorp))
    (when (depth-attachment obj)
      (gl-finalize (get-attachment (depth-attachment obj)) errorp))
    (when (depth-stencil-attachment obj)
      (gl-finalize (get-attachment (depth-stencil-attachment obj)) errorp))
    (setf (id obj) (gl:gen-framebuffer))
    (gl:bind-framebuffer :framebuffer (id obj))
    (loop
       for tail on (color-attachments obj) by #'cddr
       for (attachment-point attachment) = tail
       do (attach :framebuffer attachment-point attachment))
    (when (depth-attachment obj)
      (attach-framebuffer-texture :framebuffer :depth-attachment  (depth-attachment obj)))
    (when (depth-stencil-attachment obj)
      (attach-framebuffer-texture
       :framebuffer :depth-stencil-attachment (depth-stencil-attachment obj)))
    (gl:draw-buffers (gl-draw-buffers obj))
    (gl:bind-framebuffer :framebuffer 0)))

(defclass gl-viewport ()
  ((viewport-x :accessor viewport-x :initarg :viewport-x :initform 0)
   (viewport-y :accessor viewport-y :initarg :viewport-y :initform 0)
   (viewport-width :accessor viewport-width :initarg :viewport-width :initform 0)
   (viewport-height :accessor viewport-height :initarg :viewport-height :initform 0)))

(defmethod gl-finalized-p ((obj gl-viewport))
  t)

(defmethod gl-finalize ((obj gl-viewport) &optional errorp)
  t)

(defmethod glstate-compare ((e1 gl-viewport) (e2 gl-viewport))
  (compare-pairs
   (viewport-x e1) (viewport-x e2)
   (viewport-y e1) (viewport-y e2)
   (viewport-width e1) (viewport-width e2)
   (viewport-height e1) (viewport-height e2)))

(defmethod glstate-bind (tracker (element gl-viewport) previous-element)
  (declare (ignore tracker previous-element))
  (gl:viewport (viewport-x element) (viewport-y element) (viewport-width element) (viewport-height element)))

(defmethod make-default-glstate-member ((name (eql 'glstate-viewport)))
  (make-instance 'gl-viewport))

;;; Graphics modes are the state controlled by gl:enable and gl:disable. They are represented as
;;; bits in an integer. The modes fit comfortably in a fixnum in 64 bit Lisps, but things are
;;; pretty tight on 32 bit Lisps, so we put the less common modes in the top bits.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *modes*
    '(:blend
      :color-logic-op
      :cull-face
      :depth-clamp
      :depth-test
      :dither
      :framebuffer-srgb
      :multisample
      :polygon-offset-fill
      :polygon-offset-line
      :polygon-offset-point
      :primitive-restart
      :scissor-test
      :stencil-test
      :texture-cube-map-seamless
      :program-point-size
      :sample-alpha-to-coverage
      :sample-alpha-to-one
      :sample-coverage
      :clip-distance0
      :clip-distance1
      :clip-distance2
      :clip-distance3
      :clip-distance4
      :clip-distance5
      :clip-distance6
      :clip-distance7
      :line-smooth
      :polygon-smooth
      ))
  
  (defun modelist-to-mask (modes)
    (let ((val 0))
      (loop
         for mode in modes
         for pos = (position mode *modes*)
         do (if pos
                (setf (ldb (byte 1 pos) val) 1)
                (error "~S is not a known OpenGL mode." mode)))
      val))

  (defparameter *all-modes* (1- (ash 1 (length *modes*)))))

(defparameter *mode-enums* (mapcar (lambda (name)
                                     (cffi:foreign-enum-value 'cl-opengl-bindings:enum name))
                                   *modes*))
(defclass gl-modes ()
  ((mode-value :accessor mode-value :initform 0 :initarg :mode-value)
   (is-set :accessor is-set :initform 0 :initarg :is-set)))

(defun make-modes (enabled disabled)
  (let* ((enabled-mask (modelist-to-mask enabled))
         (disabled-mask (modelist-to-mask disabled))
         (is-set-mask (logior enabled-mask disabled-mask)))
    (make-instance 'gl-modes :mode-value enabled-mask :is-set is-set-mask)))

(defmethod glstate-compare ((m1 gl-modes) (m2 gl-modes))
  (let ((mode-comp (compare-num (mode-value m1) (mode-value m2))))
    (if (zerop mode-comp)
        (compare-num (is-set m1) (is-set m2))
        mode-comp)))

(defun bind-mode-mask (modes modes-to-set)
  (loop
     for set-shift = modes-to-set then (ash set-shift -1)
     while (not (zerop set-shift))
     for mode-shift = modes then (ash mode-shift -1)
     for enum in *mode-enums*
     when (logbitp 0 set-shift)
     do
       (if (logbitp 0 mode-shift)
           (%gl:enable enum)
           (%gl:disable enum))
     end))

(defmethod initialize-instance :after ((obj graphics-state) &key)
  (unless (slot-boundp obj 'modes)
    (setf (modes obj) (make-modes nil nil))))

(defgeneric bind-state (renderer state))

(defclass glstate-tracker ()
  ((state-stack :accessor state-stack :initform (make-array 2 :adjustable t :fill-pointer 0))
   (current-state :accessor current-state :initform (make-instance 'graphics-state))
   (last-applied :accessor last-applied :initform nil)
   (frame-count :accessor frame-count :initform 0)))

(defgeneric increment-frame-count (tracker))

(defmethod increment-frame-count ((tracker glstate-tracker))
  (incf (frame-count tracker)))

(defgeneric push-state (tracker state))

(defmethod push-state ((tracker glstate-tracker) (state graphics-state))
  (vector-push-extend state (state-stack tracker))
  (setf (last-applied tracker) nil))

(defgeneric pop-state (tracker))

(defmethod pop-state ((tracker glstate-tracker))
  (vector-pop (state-stack tracker))
  (setf (last-applied tracker) nil))

(defun bind-and-push-state (tracker state)
  "Convenience function to bind a graphics-state and push it on the state stack."
  (bind-state tracker state)
  (push-state tracker state))

(defgeneric bind-state-with-tracker (tracker state))

(defmethod bind-state ((renderer glstate-tracker) (state graphics-state))
  (bind-state-with-tracker renderer state))

(defclass program (gl-program)
  (
   ;; elements are (desc strategy (most-recent-uset counter))
   (uset-alist :accessor uset-alist :initform nil
               :documentation "private")))

;;; Compute all the usets used in a program's shaders, then choose strategies
;;; for them.
(defun compute-usets (program)
  (let ((uset-alist nil))
    (loop
       for shader in (shaders program)
       for usets = (usets shader)
       do (loop
             for uset in usets
             for uset-pair = (assoc uset uset-alist)
             do (if uset-pair
                    (push shader (cdr uset-pair))
                    (push (cons uset shader) uset-alist))))
    ;; Only one kind of uset for now.
    (setf (uset-alist program)
          (mapcar #'(lambda (entry)
                      (list (car entry)
                            (make-uset-strategy (car entry)
                                                program
                                                'explicit-uniforms)
                            (list nil 0)))
                  uset-alist))))

;;; Set the uniform values in a program, assuming  that it is currently bound.
(defun upload-uset-to-program (uset program tracker)
  (let* ((descriptor (descriptor uset))
         (uset-data (getassoc descriptor (uset-alist program)))
         (strategy (car uset-data)))
    (when strategy
      (funcall (uploader strategy) uset))
    uset))

(defmethod gl-finalize ((obj program) &optional (errorp t))
  (compute-usets obj)
  (call-next-method))

(defgeneric bind-modes (tracker state))

(defmethod bind-state-with-tracker ((tracker glstate-tracker) (state graphics-state))
  (let ((stack (state-stack tracker))
        (current-elements (state-members (current-state tracker))))
    (flet ((bind1 (i)
             (let ((new-element (svref (state-members state) i)))
               (unless new-element
                 (loop
                    for j from (1- (length stack)) downto 0
                    for state-on-stack = (aref stack j)
                    for element-from-stack = (svref (state-members state-on-stack) i)
                    if element-from-stack
                    do
                      (setq new-element element-from-stack)
                      (loop-finish)
                    end))
               (unless (eq new-element (svref current-elements i))
                 (glstate-bind tracker new-element (svref current-elements i))
                 (setf (svref current-elements i) new-element)))))
      (unless (eq state (last-applied tracker))
        (let ((old-program (glstate-program (current-state tracker))))
          (loop
             for i from 0 below (length current-elements)
             do
               (bind1 i))
          (bind-modes tracker state)
          ;; Global uset values. Will change with UBOs. Gross hack or clever hack?
          (let ((current-program (glstate-program (current-state tracker))))
            (unless (eq old-program current-program)
              (loop
                 for state-on-stack across stack
                 for usets = (global-usets state-on-stack)
                 do (loop
                       for uset in usets
                       do (upload-uset-to-program uset current-program))))))
        (setf (last-applied tracker) state)))))

(defmethod bind-modes ((tracker glstate-tracker) (new-state graphics-state))
  ;; First accumulate the mode state represented by the state stack
  (let ((all-modes 0)
        (all-is-set 0)
        (stack (state-stack tracker)))
    (flet ((update-modes (mode-value is-set)
             (setq all-modes (logior (logand all-is-set
                                             (logandc2 all-modes is-set))
                                     (logand mode-value is-set)))
             (setq all-is-set (logior all-is-set is-set))))
      (loop
         for state across stack
         for modes = (modes state)
         for mode-value = (mode-value modes)
         for is-set = (is-set modes)
         do (update-modes (mode-value modes) (is-set modes)))
      ;; modes from new state
      (let ((modes (modes new-state)))
        (update-modes (mode-value modes) (is-set modes))))
    ;; Set modes in OpenGL according to the validity of the current value (was it ever set)  and
    ;; whether or not the new value is different.
    (let* ((current-mode-obj (modes (current-state tracker)))
           (current-mode-value (mode-value current-mode-obj))
           (current-is-set (is-set current-mode-obj))
           (difference (logxor all-modes current-mode-value))
           (to-set (logand all-is-set
                           (logorc1 current-is-set difference))))
      (bind-mode-mask all-modes to-set)
      (setf (mode-value current-mode-obj)
            (logior (logand all-modes to-set)
                    (logand current-is-set current-mode-value (lognot to-set))))
      (setf (is-set current-mode-obj) (logior to-set current-is-set)))))

(defun make-default-graphics-state ()
  (let* ((state (make-instance 'graphics-state))
         (members (state-members state)))
    (loop
       for i from 0 below (length members)
       for m in *glstate-elements*
       do (setf (svref members i) (make-default-glstate-member m)))
    (let* ((enabled-modes '(:dither :multisample))
           (disabled-modes (set-difference *modes* enabled-modes)))
      (setf (modes state) (make-modes enabled-modes disabled-modes)))
    state))

(defparameter *default-graphics-state* (make-default-graphics-state))
