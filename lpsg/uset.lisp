;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

;;; Uniforms variables, as defined in OpenGL, are slowly-changing values
;;; in a shader program. They are contained in the program object. Uniform
;;; blocks, first defined in OpenGL 3.0, are aggregates of uniform variables
;;; that are stored in OpenGL buffer objects.
;;;
;;; A uniform set, or "uset", is an LPSG abstraction representing a collection
;;; of uniform variables. They might be represented concretely in OpenGL as
;;; uniform variables, a uniform block, several vertex attributes, values
;;; stored in a texture, or values in shader storage objects. The choice will
;;; depend on the frequency of updating, data size, and OpenGL version. A
;;; "uniform set descriptor" defines the names, types, layout, etc. of
;;; variables in the set. A particular instantiation of a uniform set's values
;;; can be assigned to a render bundle [Individually? In a graphics state
;;; object?]
;;;
;;; Uniform sets are declared using LPSG functions, not declarations within the
;;; shader program source. The shader program will refer to the variable names,
;;; and LPSG will insert appropriate preprocessor defines at the beginning of
;;; the program source.
;;;
;;; Any uniform variables [and uniform blocks?] declared explicitly in a
;;; program will be treated automatically as a individual shader sets, and will
;;; of course be stored within a program.

;;; Uniforms within a uset descriptor
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass uniform-declaration ()
    ((name :accessor name :initarg :name)
     (full-name :accessor full-name)
     (gl-type :accessor gl-type :initarg :gl-type)
     (accessor :accessor accessor :initarg :accessor)
     (local-offset :accessor local-offset :initarg :local-offset
                   :documentation "offset of uniform in local storage")
     (local-storage-setter :accessor local-storage-setter)))

;;; The uniform set descriptor functions as a "metaclass" for the uset. It
;;; stores all information about the uniforms in a uset.
;;;
;;; LOCAL-STORAGE-SIZE refers to CPU-side storage into which uniforms may be
;;; copied. 

  (defclass uset-descriptor ()
    ((name :accessor name :initarg :name)
     (uniforms :accessor uniforms)
     (local-storage-size :accessor local-storage-size :initform 0)))
)

;;; Uset-descriptor chooses the specific class of uniform
;;; set used for its strategy.

;;; Uset variable values are stored locally, so they can be set by the
;;; application and then later uploaded to shader programs during
;;; rendering. 

(defclass uset ()
  ;; descriptor slot is class allocated, but it must be shared by each specific
  ;; uset class.
  ())

(defgeneric descriptor (uset))

;;; Different memory layouts are needed, depending on the uniform set
;;; strategy. For uniforms in the default block, we need a C-like layout. If we
;;; are going to use uniform buffers, then we need to use a layout like
;;; std140 in the memory backing the uniform buffer object.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *uniform-type-info* (make-hash-table :test 'eq))

;;; Local-writer and uploader are for the default strategy.
;;; XXX Should other strategies' functions be added to this object, or
;;; something else?

  (defclass uniform-type-info ()
    ((name :accessor name :initarg :name)
     (glsl-name :accessor glsl-name :initarg :glsl-name)
     (size :accessor size :initarg :size)
     (c-alignment :accessor c-alignment :initarg :c-alignment)
     (std140-alignment :accessor std140-alignment :initarg :std140-alignment)
     (stride :accessor stride :initarg :stride)
     ;; names of writer functions
     (local-writer :accessor local-writer :initarg :local-writer)
     (uploader :accessor uploader :initarg :uploader)))
)

(defmacro define-uniform-type (sym glsl-name size c-alignment gl-alignment
                               stride)
  (let ((writer-sym (intern (concatenate 'simple-string
                                         (symbol-name '#:write-uniform-local-)
                                         (symbol-name sym))))
        (uploader-sym (intern (concatenate 'simple-string
                                           (symbol-name '#:upload-uniform-)
                                           (symbol-name sym)))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',sym *uniform-type-info*)
             (make-instance 'uniform-type-info
                            :name ,sym :glsl-name ,glsl-name :size ,size
                            :c-alignment ,c-alignment
                            :std140-alignment ,gl-alignment
                            :stride ,stride
                            :local-writer ',writer-sym
                            :uploader ',uploader-sym)))))

#|
(define-uniform-type :int)
(define-uniform-type :unsigned-int)
|#

(defun write-uniform-local-float (ptr val)
  (setf (cffi:mem-ref ptr :float) val))
(defun upload-uniform-float (location ptr)
  (%gl:uniform-1fv location 1 ptr))
(define-uniform-type :float "float" 4 4 4 4)

#|
(define-uniform-type :double)

(define-uniform-type :float-vec2 "vec2" 8 4 8)
(define-uniform-type :float-vec3 "vec3" 12 4 16)
|#

(defun write-uniform-local-float-vec4 (ptr val)
  (loop
     for i from 0 below 4
     do (setf (cffi:mem-aref ptr '%gl:float i) (aref val i))))
(defun upload-uniform-float-vec4 (location ptr)
  (%gl:uniform-4fv location 1 ptr))
(define-uniform-type :float-vec4 "vec4" 16 4 16 16)

#|
(define-uniform-type :int-vec2)
(define-uniform-type :int-vec3)
(define-uniform-type :int-vec4)

(define-uniform-type :unsigned-int-vec2)
(define-uniform-type :unsigned-int-vec3)
(define-uniform-type :unsigned-int-vec4)

(define-uniform-type :float-mat2 "mat2" 16 4 8 16)
(define-uniform-type :float-mat2x3)
(define-uniform-type :float-mat2x4)
(define-uniform-type :float-mat3)
(define-uniform-type :float-mat3x2)
(define-uniform-type :float-mat3x4)
|#

(defun write-uniform-local-float-mat4 (ptr val)
  (loop
     for i from 0 below 16
     do (setf (cffi:mem-aref ptr '%gl:float i) (row-major-aref val i))))

(defun upload-uniform-float-mat4 (location ptr)
  (%gl:uniform-matrix-4fv location 1 nil ptr))
(define-uniform-type :float-mat4 "mat4" 64 4 16 64)

#|
(define-uniform-type :float-mat4x2)
(define-uniform-type :float-mat4x3)


(define-uniform-type :int-sampler-1d)
(define-uniform-type :int-sampler-1d-array)
(define-uniform-type :int-sampler-1d-array-ext)
(define-uniform-type :int-sampler-1d-ext)
(define-uniform-type :int-sampler-2d)
(define-uniform-type :int-sampler-2d-array)
(define-uniform-type :int-sampler-2d-array-ext)
(define-uniform-type :int-sampler-2d-ext)
(define-uniform-type :int-sampler-2d-multisample)
(define-uniform-type :int-sampler-2d-multisample-array)
(define-uniform-type :int-sampler-2d-rect)
(define-uniform-type :int-sampler-2d-rect-ext)
(define-uniform-type :int-sampler-3d)
(define-uniform-type :int-sampler-3d-ext)
(define-uniform-type :int-sampler-buffer)
(define-uniform-type :int-sampler-buffer-amd)
(define-uniform-type :int-sampler-buffer-ext)
(define-uniform-type :int-sampler-cube)
(define-uniform-type :int-sampler-cube-ext)
(define-uniform-type :int-sampler-cube-map-array)
(define-uniform-type :int-sampler-cube-map-array-arb)

(define-uniform-type :sampler)
(define-uniform-type :sampler-1d)
(define-uniform-type :sampler-1d-arb)
(define-uniform-type :sampler-1d-array)
(define-uniform-type :sampler-1d-array-ext)
(define-uniform-type :sampler-1d-array-shadow)
(define-uniform-type :sampler-1d-array-shadow-ext)
(define-uniform-type :sampler-1d-shadow)
(define-uniform-type :sampler-1d-shadow-arb)
(define-uniform-type :sampler-2d)
(define-uniform-type :sampler-2d-arb)
(define-uniform-type :sampler-2d-array)
(define-uniform-type :sampler-2d-array-ext)
(define-uniform-type :sampler-2d-array-shadow)
(define-uniform-type :sampler-2d-array-shadow-ext)
(define-uniform-type :sampler-2d-multisample)
(define-uniform-type :sampler-2d-multisample-array)
(define-uniform-type :sampler-2d-rect)
(define-uniform-type :sampler-2d-rect-arb)
(define-uniform-type :sampler-2d-rect-shadow)
(define-uniform-type :sampler-2d-rect-shadow-arb)
(define-uniform-type :sampler-2d-shadow)
(define-uniform-type :sampler-2d-shadow-arb)
(define-uniform-type :sampler-2d-shadow-ext)
(define-uniform-type :sampler-3d)
(define-uniform-type :sampler-3d-arb)
(define-uniform-type :sampler-3d-oes)

(define-uniform-type :unsigned-int-sampler-1d)
(define-uniform-type :unsigned-int-sampler-1d-array)
(define-uniform-type :unsigned-int-sampler-1d-array-ext)
(define-uniform-type :unsigned-int-sampler-1d-ext)
(define-uniform-type :unsigned-int-sampler-2d)
(define-uniform-type :unsigned-int-sampler-2d-array)
(define-uniform-type :unsigned-int-sampler-2d-array-ext)
(define-uniform-type :unsigned-int-sampler-2d-ext)
(define-uniform-type :unsigned-int-sampler-2d-multisample)
(define-uniform-type :unsigned-int-sampler-2d-multisample-array)
(define-uniform-type :unsigned-int-sampler-2d-rect)
(define-uniform-type :unsigned-int-sampler-2d-rect-ext)
(define-uniform-type :unsigned-int-sampler-3d)
(define-uniform-type :unsigned-int-sampler-3d-ext)
(define-uniform-type :unsigned-int-sampler-buffer)
(define-uniform-type :unsigned-int-sampler-buffer-amd)
(define-uniform-type :unsigned-int-sampler-buffer-ext)
(define-uniform-type :unsigned-int-sampler-cube)
(define-uniform-type :unsigned-int-sampler-cube-ext)
(define-uniform-type :unsigned-int-sampler-cube-map-array)
(define-uniform-type :unsigned-int-sampler-cube-map-array-arb)
|#


(defun get-std140-base-alignment (type)
  (case type
    ((:int :unsigned-int :float :bool)
     4)
    (:double
     8)
    ((:float-vec2 :int-vec2)
     8)
    ((:float-vec3 :int-vec3 :float-vec4 :int-vec4)
     16)
    ))
(defun std140-layout (uniforms)
  (let ((offset 0))
    (loop ))
  )

;;; Implementation of strategies
(defclass strategy ()
  ((uset-descriptor :accessor uset-descriptor :initarg :uset-descriptor)))

;;; The default uniform set strategy, which uploads uniform values into uniform
;;; locations in a shader program.

(defclass explicit-uniforms (strategy)
  ((per-program-locations :accessor per-program-locations
                          :initarg :per-program-locations
                          :initform nil)
   (uploader :accessor uploader)))

(defclass uniform-definition ()
  ((decl :accessor decl)
   (location :accessor location)
   (offset :accessor offset)))

(defvar *uset-descriptors* (make-hash-table :test 'eq))

(defun get-uset-descriptor (name)
  (gethash name *uset-descriptors*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-uniform-slots (variables)
    (loop
       for var in variables
       for (gl-name gl-type . slot-args) = var
       collecting slot-args)))

;;; Create an object that is stored with a program that contains data for
;;; uploading the uset for that program. 
(defgeneric make-uset-strategy (descriptor program strategy-name))

(defmethod make-uset-strategy ((descriptor uset-descriptor) program name)
  (make-instance name :uset-descriptor descriptor))
  
;;; PROGRAM has already been finalized
;;; i.e., OpenGL has been queried for uniform locations, etc.
(defgeneric initialize-uset-strategy (strategy program))

(defun initialize-uset-descriptor (obj variables)
  (let ((decls (mapcar #'(lambda (clause)
                           (destructuring-bind (name gl-type accessor &key
                                                     &allow-other-keys)
                               clause
                             (make-instance 'uniform-declaration
                                            :name name
                                            :gl-type gl-type
                                            :accessor accessor)))
                       variables)))
    (loop
       with offset = 0
       for var in decls
       for typeinfo = (gethash (gl-type var) *uniform-type-info*)
       do (if typeinfo
              (with-slots (size c-alignment)
                  typeinfo
                ;; XXX Do something with size when arrays are supported
                (setq offset (round-up offset c-alignment))
                (setf (local-offset var) offset)
                (incf offset size)))
       finally (setf (local-storage-size obj) (round-up offset 8))) ;double alignment
    (setf (uniforms obj) decls))
  obj)

(defmethod initialize-instance :after ((obj uset-descriptor)
                                       &key variables)
  (initialize-uset-descriptor obj variables))

(defun ensure-uset (name variables &key (strategy :default) descriptor-class)
  ;; class choice will be more interesting in the future
  (let* ((actual-class (or descriptor-class
                           'uset-descriptor))
         (existing-desc (get-uset-descriptor name)))

    (if existing-desc
        (initialize-uset-descriptor existing-desc variables)
        (setf (gethash name *uset-descriptors*)
              (make-instance actual-class :name name :variables variables)))))


;;; Upload uniform values from a uset to a program for the default strategy.

(defgeneric get-uniform-writer (uniform-decl))

(defmethod get-uniform-writer ((uniform-decl uniform-declaration))
  (let* ((gl-type (gl-type uniform-decl))
         (type-decl (gethash gl-type *uniform-type-info*)))
    (if type-decl
        (local-writer type-decl)
        (error "~S is not a known GL type." gl-type))))
  
 (defgeneric get-uniform-uploader (uniform-decl))

(defmethod get-uniform-uploader ((uniform-decl uniform-declaration))
  (let* ((gl-type (gl-type uniform-decl))
         (type-decl (gethash gl-type *uniform-type-info*)))
    (if type-decl
        (uploader type-decl)
        (error "~ is not a known GL type." gl-type))))
         
(defun make-uniform-upload-lambda (program strategy)
  (let ((descriptor (uset-descriptor strategy)))
    (loop
       for var in (uniforms descriptor)
       for offset = (local-offset var)
       for writer = (get-uniform-writer var)
       for uploader = (get-uniform-uploader var)
       for prog-uniform = (assoc (name var) (uniforms program) :test #'string=)
       for (name location) = prog-uniform
       if prog-uniform
       collect `(,writer (cffi:inc-pointer raw ,offset) (,(accessor var) uset))
         into local-clauses
       and collect `(,uploader ,location (cffi:inc-pointer raw ,offset))
         into upload-clauses
       finally (return `(lambda (uset)
                          (cffi:with-foreign-object
                              ;; XXX compute size correctly
                              (raw :double ,(local-storage-size descriptor))
                            ,@local-clauses
                            ,@upload-clauses))))))

(defmethod initialize-uset-strategy ((strategy explicit-uniforms) program)
  (let* ((upload-lambda (make-uniform-upload-lambda program strategy))
         (upload-fn (compile nil upload-lambda)))
    (setf (uploader strategy) upload-fn)))

(defmacro define-uset (name variables &rest desc-args
                       &key (strategy :default strategyp) descriptor-class)
  "Define a uset.

  A uset is a set of variables that parameterize a shader program. In OpenGL, these variables are
  called `uniforms', and change infrequently. DEFINE-USET defines a class that stores the values,
  and for each value specifies how to load the value into the program.

  VARIABLES is a list of uniform definitions, much like slot definitions in DEFCLASS. A uniform
  definition looks like:
  (gl-namestring uniform-type slot-name &rest slot-args)

  GL-NAMESTRING is the name of the uniform in the OpenGL shader program, as a string. UNIFORM-TYPE
  is an OpenGL uniform type, specified as a keyword from cl-opengl. SLOT-NAME and SLOT-ARGS are the
  same as in DEFCLASS."
  (let* ((uset-slots (parse-uniform-slots variables)))
    `(progn
       (ensure-uset ',name ',variables
                    ,@(and strategyp `(:strategy ',strategy))
                    ,@(and descriptor-class
                           `(:descriptor-class ',descriptor-class)))
       (defclass ,name (uset)
         (,@uset-slots
          (descriptor :accessor descriptor :allocation :class
                      :initform (get-uset-descriptor ',name)))))))

(defun ensure-descriptors (descriptors)
  (mapcar #'ensure-descriptor descriptors))
