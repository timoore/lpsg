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
    '(#+nil :render-target
      :program
      :texunits
      :cull-face
      :depth-func
      :depth-range
      :front-face
      #+nil :modes))
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
                ()
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

(defun compare-gl-objects (obj1 obj2)
  (cond ((eq obj1 obj2)
         0)
        ((null obj1)
         -1)
        ((null obj2)
         1)
        (t
         (compare-num (id obj1) (id obj2)))))

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
  ((units :accessor units
          :documentation "An array of bindings for OpenGL's texture units. The members of the array
  can be NIL, representing no binding")))

(defmethod initialize-instance :after ((obj gl-texunits) &key units renderer)
  (let ((max-tex-units (if renderer
                           (max-combined-texture-image-units (context-parameters renderer))
                           16)))
    (if (and units (= (length units) max-tex-units))
        (setf (units obj) units)
        (let ((new-units (make-array max-tex-units :initial-element nil)))
          (when units
            (setf (subseq new-units 0) units))
          (setf (units obj) new-units)))))

(defmethod glstate-compare ((m1 gl-texunits) m2)
  (let* ((units1 (units m1))
         (units2 (units m2)))
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

#|
(defun active-texture (num &optional (unit :texture0))
  (let ((texture0 (cffi:foreign-enum-value '%gl:enum unit)))
    (%gl:active-texture (+ texture0 num))))
|#

(defmethod glstate-bind (tracker (m gl-texunits) previous-units)
  (declare (ignore tracker))
  (flet ((unbind-texture (unit)
           (when unit
             (gl:bind-texture (target (tex-object unit)) 0))))
    (loop
       with units = (units m)
       with prev-units = (and previous-units (units previous-units))
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
                  (t (gl:bind-texture :texture-2d 0)))))))

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
  (let ((comp1 (compare-num (near e1) (near e2))))
    (if (zerop comp1)
        (compare-num (far e1) (far e2))
        comp1)))

(defmethod make-default-glstate-member ((name (eql 'glstate-depth-range)))
  (make-instance 'gl-depth-range))

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

(defgeneric bind-state (renderer state))

(defclass glstate-tracker ()
  ((state-stack :accessor state-stack :initform (make-array 2 :adjustable t :fill-pointer 0))
   (current-state :accessor current-state :initform (make-instance 'graphics-state))
   (last-applied :accessor last-applied :initform nil)
   (current-state-valid :accessor current-state-valid :initform nil)))

(defgeneric push-state (tracker state))

(defmethod push-state ((tracker glstate-tracker) (state graphics-state))
  (vector-push-extend state (state-stack tracker))
  (setf (current-state-valid tracker) nil))

(defgeneric pop-state (tracker))

(defmethod pop-state ((tracker glstate-tracker))
  (vector-pop (state-stack tracker))
  (setf (current-state-valid tracker) nil))

(defgeneric bind-state-with-tracker (tracker state))

(defmethod bind-state ((renderer glstate-tracker) (state graphics-state))
  (bind-state-with-tracker renderer state))

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
      (unless (and (current-state-valid tracker)
                   (eq state (last-applied tracker)))
        (loop
           for i from 0 below (length current-elements)
           do
             (bind1 i))
        (setf (last-applied tracker) state)
        (setf (current-state-valid tracker) t)))))

(defun make-default-graphics-state ()
  (let* ((state (make-instance 'graphics-state))
         (members (state-members state)))
    (loop
       for i from 0 below (length members)
       for m in *glstate-elements*
       do (setf (svref members i) (make-default-glstate-member m)))
    state))

(defparameter *default-graphics-state* (make-default-graphics-state))
