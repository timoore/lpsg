;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

(defun report-render-error (condition stream)
  (let ((args (format-arguments condition))
        (obj (render-error-gl-object condition)))
    (when obj
      (push obj args))
    (cond ((format-control condition)
           (apply #'format stream (format-control condition) args))
          (obj
           (format stream "A render error occured using ~S." obj))
          (t (format stream "A render error occured.")))))

(define-condition render-error (error)
  ((gl-object :reader render-error-gl-object :initarg :gl-object
              :initform nil)
   (format-control :reader format-control :initarg :format-control
                   :initarg :message :initform nil)
   (format-arguments :reader format-arguments :initarg :format-arguments
                     :initform nil)
   (log :reader render-error-log :initarg :error-log :initform nil))
  (:report report-render-error))

(defvar *renderer*)

(define-protocol-class gl-object ()
  ((:accessor id :documentation "The OpenGL ID of an object.")
   (:accessor gl-proxy :documentation "Object for accessing the associated
OpenGL object"))
  (:documentation "Class representing any OpenGL object."))

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

(defgeneric gl-finalize (obj &optional errorp)
  (:documentation "Allocate any OpenGL resources needed for @cl:param(obj) and perform any
tasks needed to use it (e.g. link a shader program).

Returns @c(t) if finalize actions were performed, @c(nil) otherwise.

This is called when the renderer's OpenGL context is current. The renderer is accessible in
@c(*renderer*)."))

(defgeneric gl-finalized-p (obj)
  (:documentation "Returns @c(t) if object has already been finalized."))

(defgeneric gl-destroy (obj)
  (:documentation "Deallocate an OpenGL object."))

(defmethod gl-finalize :around ((obj t) &optional errorp)
  (if (gl-finalized-p obj)
      nil
      (call-next-method)))

(defmethod gl-finalize :around ((object gl-object) &optional errorp)
  (let ((result (call-next-method)))
    (when result
      (push (cons (tg:make-weak-pointer object) (gl-proxy object)) (gl-objects *renderer*)))
    result))

(defmethod gl-finalized-p (obj)
  (declare (ignore obj))
  t)

;;; This should be some kind of ordered data structure (map, skip list, ...)
(defclass unordered-render-queue (render-queue)
  ((bundles :accessor bundles :initarg :bundles :initform nil
            :documentation "private"))
  (:documentation "A queue that contains bundles to be rendered."))

(defmethod add-rendered-object ((render-queue unordered-render-queue) object)
  (push object (bundles render-queue)))

(defmethod remove-rendered-object ((render-queue unordered-render-queue) object)
  (setf (bundles render-queue) (delete object (bundles render-queue))))

(defmethod map-render-queue ((render-queue unordered-render-queue) function)
  (mapc function (bundles render-queue)))

(defmethod find-if-queue (predicate (render-queue unordered-render-queue))
  (find-if predicate (bundles render-queue)))

(defclass ordered-render-queue (render-queue)
  ((queue-object :documentation "private"))
  (:documentation "Class for a queue of objects that are rendered in order.

@c(add-rendered-object) will add an object to the end of the queue."))

(defmethod initialize-instance :after ((obj ordered-render-queue) &key)
  (setf (slot-value obj 'queue-object) (serapeum:queue)))

(defmethod add-rendered-object ((render-queue ordered-render-queue) object)
  (with-slots (queue-object)
      render-queue
    (serapeum:enq object queue-object)
    nil))

(defmethod remove-rendered-object ((render-queue ordered-render-queue) object)
  (with-slots (queue-object)
      render-queue
    (let ((contents (serapeum:clear-queue queue-object)))
      (setq contents (delete object contents))
      (serapeum:qconc queue-object contents))
    nil))

(defmethod map-render-queue ((render-queue ordered-render-queue) function)
  (mapc function (serapeum:qlist (slot-value render-queue 'queue-object))))

(defmethod find-if-queue (predicate (queue ordered-render-queue))
  (find predicate (serapeum:qlist (slot-value queue 'queue-object))))

;;; holds multiple render queues. These will be rendered in order.
(defclass render-stage (ordered-render-queue)
  ()
  (:documentation "A render queue with designated read and draw buffers @i([default for now])"))

(defclass glcontext-parameters ()
  ((max-combined-texture-image-units :accessor max-combined-texture-image-units
                                     :initarg :max-combined-texture-image-units))
  (:default-initargs :max-combined-texture-image-units 8)) ;XXX way to small, for testing

(defclass standard-renderer (renderer)
  ((buffers :accessor buffers :initform nil :documentation "private")
   (bundles :accessor bundles :initform nil :documentation "private")
   (current-state :accessor current-state :initform nil :documentation "private")
   (predraw-queue :accessor predraw-queue :initform nil :documentation "private")
   (finalize-queue :accessor finalize-queue :initform nil :documentation "private")
   ;; alist of (buffer . buffer-areas)
   (upload-queue :accessor upload-queue :initform nil :documentation "private")
   (render-stage :accessor render-stage
                  :documentation "The top-level (default) render stage.")
   ;; XXX Should be weak
   (vao-cache :accessor vao-cache :initform (make-hash-table :test 'equal) :documentation "private")
   (gl-objects :accessor gl-objects :initform nil :documentation "List of all OpenGL objects
allocated by calls in LPSG." )
   (context-parameters :accessor context-parameters
                       :documentation "Parameters of the OpenGL context."))
  (:documentation "The standard instantiable class of @c(renderer).")
  )

;;; XXX Temporary until we figure out how how / when to intialize the renderer from a graphics
;;; context.

(defmethod initialize-instance :after ((obj standard-renderer) &key)
  (setf (context-parameters obj) (make-instance 'glcontext-parameters))
  (setf (render-stage obj) (make-instance 'render-stage)))

(defun process-finalize-queue (renderer)
  (loop
     for obj in (finalize-queue renderer)
     if (not (gl-finalized-p obj))
     do (gl-finalize obj)))

(defgeneric gl-valid-p (obj))

(defmethod gl-valid-p ((obj gl-object))
  (not (zerop (id obj))))

(defconstant +default-buffer-size+ 104856
  "The default size of a buffer object allocated with GL-BUFFER.")

(define-gl-object gl-buffer ()
  ((size :accessor size :initarg :size
         :documentation "The size of the buffer object in OpenGL. Note: this value is mutable until
  @c(gl-finalize) is called on the @c(gl-buffer) object.")
   (usage :accessor usage :initarg :usage
          :documentation "Usage hint for the buffer object. Value is a @c(cl-opengl) keyword e.g.,
  :STATIC-DRAW.")
   (target :accessor target :initarg :target
           :documentation "OpenGL targert for the buffer object. Value is a @c(cl-opengl) keyword e.g.,
  :ARRAY-BUFFER."))
  (:default-initargs :target :array-buffer :usage :static-draw :size +default-buffer-size+)
  (:documentation "A buffer object allocated in OpenGL.

In OpenGL, the @c(usage) and @c(target) parameters are hints and it is legal to use a buffer differently,
but that can impact performance."))

(defmethod gl-finalized-p ((obj gl-buffer))
  (gl-valid-p obj))

(defmethod gl-finalize ((buffer gl-buffer) &optional errorp)
  (declare (ignorable errorp))          ; TODO: handle errorp
  (let ((target (target buffer))
        (id (car (gl:gen-buffers 1))))
    (setf (id buffer) id)
    (gl:bind-buffer target id)
    (%gl:buffer-data target (size buffer) (cffi:null-pointer) (usage buffer)))
  t)

(defmethod gl-destroy ((obj %gl-buffer))
  (gl:delete-buffers (list (%id obj)))
  (setf (%id obj) 0))

(defclass drawable ()
  ((mode :accessor mode :initarg :mode
         :documentation "A mode for an OpenGL draw-elements or draw-array call,
  e.g. :triangles")
   (vertex-count :accessor vertex-count :initarg :vertex-count
                 :documentation "The total number of vertices in this geometry."))
  (:documentation "Superclass for classes that describe the format, number, and layout of vertex
data.")) 

(defclass array-drawable (drawable)
  ((first-vertex :accessor first-vertex :initarg :first-vertex
                 :documentation "The starting index in the enabled arrays."))
  (:documentation "Drawable class for a shape that stores its attribute values in linear arrays."))

(defclass indexed-drawable (drawable)
  ((index-type :accessor index-type :initarg :index-type
               :documentation "OpenGL type of the index values")
   (base-vertex :accessor base-vertex :initarg :base-vertex
                :documentation "index offset to the shape's data in the attribute arrays, as
used in the gl:draw-elements-base-vertex function")
   (element-array :accessor element-array :initarg :element-array
                  :documentation "Lisp array of element indices"))
  (:default-initargs :index-type :unsigned-short :base-vertex 0)
  (:documentation "Drawable class for a shape that "))

(defclass buffer-area ()
  ((buffer :accessor buffer :initarg :buffer
           :documentation "The OpenGL buffer object containing attribute data.")
   (resource-size :accessor resource-size :initarg :resource-size
                  :documentation "total size, in bytes, of attribute's data in buffer")
   (components :accessor components :initarg :components
               :documentation "number of components in each element of an attribute")
   (buffer-type :accessor buffer-type :initarg :buffer-type
                :documentation "GL format of data in buffer")
   (normalizedp :accessor normalizedp :initarg :normalizedp :initform nil
                :documentation "If true, OpenGL will normalize integer format values to [-1,1] for
signed data and [0,-1] for unsigned.")
   (stride :accessor stride :initarg :stride :initform 0
           :documentation "Byte offset between the first bytes of consecutive attributes. 0
  indicates that the attributes are tightly packed.")
   (offset :accessor offset :initarg :offset :initform 0
           :documentation "Offset used when binding a buffer with e.g., %gl:vertex-attrib-pointer."))
  (:documentation "Class for formatted attribute data stored somewhere in a buffer.

This class describes data in a vertex buffer object that will be bound using
%gl:vertex-attrib-pointer."))

(defgeneric upload-fn (buffer-area)
  (:documentation "function taking (BUFFER-AREA POINTER) ???"))

(defmethod upload-fn ((obj buffer-area))
  (error "No upload function defined."))

(defclass mirrored-resource-mixin ()
  ((data :accessor data :initarg :data)
   (data-offset :accessor data-offset :initarg :data-offset :initform 0)
   (data-count :accessor data-count :initarg :data-count :initform 0
               :documentation "number of elements")
   (data-stride :accessor data-stride :initarg :data-stride :initform 0
                :documentation "offset between start of each element")
   (num-components :accessor num-components :initarg :num-components
                   :documentation "number of components per element. Redundant
  with buffer-area components?")
   (upload-fn :accessor upload-fn :initarg :upload-fn
              :documentation "Function to upload Lisp data to a mapped buffer.
Will be created automatically, but must be specified for now.")))

(defgeneric schedule-upload (renderer object)
  (:documentation "Register an object to be uploaded to OpenGL."))

(defclass buffer-object-upload-queue ()
  ((bo-queue :accessor bo-queue :initform nil)))

(defclass texture-upload-queue ()
  ((tex-queue :accessor tex-queue :initform nil)))

(defclass upload-queue (buffer-object-upload-queue texture-upload-queue)
  ())

(defgeneric add-to-upload-queue (queue object))

(defmethod schedule-upload :before ((renderer standard-renderer) object)
  (unless (upload-queue renderer)
    (setf (upload-queue renderer) (make-instance 'upload-queue))))

(defmethod schedule-upload ((renderer standard-renderer) object)
  (add-to-upload-queue (upload-queue renderer) object))

(defmethod add-to-upload-queue ((queue buffer-object-upload-queue) (obj buffer-area))
  (let* ((buffer (buffer obj))
         (entry (assoc buffer (bo-queue queue))))
    (if entry
        (push obj (cdr entry))
        (setf (getassoc buffer (bo-queue queue)) (list obj)))))

(defgeneric process-upload-queue (renderer queue))

(defmethod process-upload-queue (renderer queue)
  )

(defmethod process-upload-queue :after (renderer (queue buffer-object-upload-queue))
  (loop
     for (buffer . uploads) in (bo-queue queue)
     for target = (target buffer)
     do (progn
          (gl:bind-buffer target (id buffer))
          (let ((ptr (gl:map-buffer target :write-only)))
            (mapc (lambda (area)
                    (funcall (upload-fn area) area ptr))
                  uploads))
          (gl:unmap-buffer target))
     finally
       (setf (bo-queue queue) nil))
  ;; Is this necessary? Should all the targets be set to 0?
  (gl:bind-buffer :array-buffer 0))

(defun do-upload-queue (renderer)
  (process-upload-queue renderer (upload-queue renderer)))

(defmethod gl-finalized-p ((obj buffer-area))
  (gl-finalized-p (buffer obj)))

(defmethod gl-finalize ((obj buffer-area) &optional errorp)
  (gl-finalize (buffer obj) errorp))

(defclass buffer-map () ())

(defclass attribute-set ()
  ((array-bindings :accessor array-bindings :initarg :array-bindings :initform nil
                   :documentation "list of @c((name vertex-attribute index)). @c(name) is the string name
  in the shader program. @c(index) is -1 if not valid.")
   (element-binding :accessor element-binding :initarg element-binding :initform nil)
   (vao :accessor vao))
  (:documentation "A collection of buffer mappings (buffer + offset) bound to specific attributes,
  along with an associated VAO."))

(define-gl-object vertex-array-object ()
  ())

(defmethod gl-finalized-p ((obj vertex-array-object))
  (gl-valid-p obj))

(defmethod gl-finalize ((obj vertex-array-object) &optional errorp)
  (declare (ignore errorp))
  (setf (id obj) (gl:gen-vertex-array))
  t)

(defmethod gl-destroy ((obj %vertex-array-object))
  (gl:delete-vertex-arrays (list (%id obj)))
  (setf (%id obj) 0))

(defun make-attribute-set-key (attr-set)
  (let ((vertex-keys (mapcar (lambda (binding)
                               (destructuring-bind (name area index)
                                   binding
                                 (declare (ignore name))
                                 (list (id (buffer area))
                                       index
                                       (components area)
                                       (normalizedp area)
                                       (stride area)
                                       (offset area))))
                             (array-bindings attr-set))))
    (if (element-binding attr-set)
        (cons (id (buffer (element-binding attr-set))) vertex-keys)
        vertex-keys)))

(defmethod gl-finalized-p ((obj attribute-set))
  (slot-boundp obj 'vao))

(defmethod gl-finalize ((attribute-set attribute-set) &optional errorp)
  (declare (ignorable errorp))
  (loop
     for (nil area) in (array-bindings attribute-set)
     unless (gl-finalized-p area)
     do (gl-finalize area errorp))
  (let ((element-binding (element-binding attribute-set)))
    (when (and element-binding (not (gl-finalized-p (element-binding attribute-set))))
      (gl-finalize (element-binding attribute-set) errorp))
    (let* ((attr-set-key (make-attribute-set-key attribute-set)))
      (multiple-value-bind (vao presentp)
          (gethash attr-set-key (vao-cache *renderer*))
        (unless presentp
          (setf vao (make-instance 'vertex-array-object))
          (gl-finalize vao)
          (let* ((vao-id (id vao))
                 (nullptr (cffi:null-pointer)))
            (gl:bind-vertex-array vao-id)
            (loop
               for (nil area index) in (array-bindings attribute-set)
               when (>= index 0)
               do (progn
                    (gl:bind-buffer :array-buffer (id (buffer area)))
                    (gl:enable-vertex-attrib-array index)
                    (gl:vertex-attrib-pointer index
                                              (components area)
                                              (buffer-type area)
                                              (normalizedp area)
                                              (stride area)
                                              (cffi:inc-pointer nullptr (offset area)))))
            (when element-binding
              (gl:bind-buffer :element-array-buffer (id (buffer element-binding))))
            (gl:bind-vertex-array 0)
            (setf (gethash attr-set-key (vao-cache *renderer*)) vao)))
        (setf (vao attribute-set) vao))))
  attribute-set)

(defclass shader-source ()
  ((shader-type :accessor shader-type :initarg :shader-type )
   (source :accessor source :initarg :source :initform nil)
   (usets :accessor usets)))
   
(defmethod initialize-instance :after ((obj shader-source) &key usets)
  (setf (usets obj) (mapcar #'(lambda (uset-name)
                                (or (get-uset-descriptor uset-name)
                                    (error "Uset ~S is not defined." uset-name)))
                            usets)))
 
;;; A shader object could be different for different programs because the uset
;;; strategies might be different. Should we keep a cache of objects for a set
;;; of uset strategies?

(define-gl-object shader (shader-source)
  ((status :accessor status :initarg :status :documentation "status of shader compilation")
   (compiler-log :accessor compiler-log :initarg :compiler-log :initform nil
                 :documentation "log of shader compilation errors and warnings"))
  (:documentation "The LPSG object that holds the source code for an OpenGL shader, information
  about its usets, ID in OpenGL, and any errors that result from its compilation."))

(defmethod gl-finalized-p ((obj shader))
  (slot-boundp obj 'status))

(defmethod gl-finalize ((obj shader) &optional (errorp t))
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

(defmethod gl-destroy ((obj %shader))
  (gl:delete-shader (%id obj))
  (setf (%id obj) 0))

(define-gl-object program ()
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
   ;; elements are (desc strategy (most-recent-uset counter))
   (uset-alist :accessor uset-alist :initform nil
               :documentation "private")
   (vertex-attribs :accessor vertex-attribs :initform nil :documentation "private"))
  (:documentation "The representation of an OpenGL shader program."))

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
(defun upload-uset-to-program (uset program)
  (let* ((descriptor (descriptor uset))
         (uset-data (getassoc descriptor (uset-alist program)))
         (strategy (car uset-data)))
    (when strategy
      (funcall (uploader strategy) uset))
    uset))

(defmethod gl-finalized-p ((obj program))
  (slot-boundp obj 'status))

(defmethod gl-finalize ((obj program) &optional (errorp t))
  (flet ((err (&rest args)
           (if errorp
               (apply #'error args)
               (return-from gl-finalize nil))))
    (compute-usets obj)
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

(defmethod gl-destroy ((obj %program))
  (gl:delete-program (%id obj))
  (setf (%id obj) 0))

(defgeneric upload-buffers (renderer obj))

(defgeneric draw (renderer)
  (:documentation "Draw all graphic objects that have been registered with @cl:param(renderer).

Perform all outstanding operations in @cl:param(renderer): finalize all objects on the
finalize queue(s), do any upload operations in the upload queue, then traverse the render stages
and their render queues to render all bundles. The renderer does not swap OpenGL front and back
buffers; that is done by the application outside of LPSG."))

(defgeneric draw-bundles (renderer))

(defgeneric process-gl-objects (renderer))

(defmethod draw ((renderer standard-renderer))
  (let ((*renderer* renderer))
    (process-gl-objects renderer)
    (loop
       for fn in (predraw-queue renderer)
       do (funcall fn renderer))
    (setf (predraw-queue renderer) nil)
    (process-finalize-queue renderer)
    (setf (finalize-queue renderer) nil)
    (do-upload-queue renderer)
    (draw-bundles renderer)))

(defgeneric draw-bundle (renderer bundle))

(defmethod draw-bundles ((renderer standard-renderer))
  ;; XXX Should we set the state to something known here?
  (setf (current-state renderer) nil)
  (do-render-queue
      (queue (render-stage renderer))
    (do-render-queue
        (bundle queue)
      (draw-bundle renderer bundle))))


(defmethod process-gl-objects ((renderer standard-renderer))
  (unless (gl-objects renderer)
    (return-from process-gl-objects nil))
  (let ((prev nil))
    (loop
       for current = (gl-objects renderer) then (cdr current)
       while current
       for pair = (car current)
       do (if (tg:weak-pointer-value (car pair))
              (setq prev current)
              (progn
                (gl-destroy (cdr pair))
                (if prev
                    (setf (cdr prev) (cdr current))
                    (setf (gl-objects renderer) (cdr current))))))))

(defmethod close-renderer ((renderer standard-renderer) &key deallocate-objects)
  (declare (ignorable deallocate-objects))
  ;;; XXX Do what, exactly?
  )

;;; size of data elements in VBOs.

(defparameter *buffer-size-alist*
  '((:half-float . 2)
    (:float . 4)
    (:double . 8)
    (:fixed . 4)
    (:byte . 1)
    (:unsigned-byte . 1)
    (:short . 2)
    (:unsigned-short . 2)
    (:int . 4)
    (:unsigned-int . 4)
    (:int-2-10-10-10-rev . 4)
    (:unsigned-int-2-10-10-10-rev . 4)
    (:unsigned-int-10f-11f-11f-rev . 4)))

(defun get-component-size (type)
  (let ((size (getassoc type *buffer-size-alist*)))
    (or size
        (error 'render-error :format-control "Unknown OpenGL type %s."
               :format-arguments type))))
