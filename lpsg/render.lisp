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
  (:documentation "Allocate any OpenGL resources needed for OBJ and perform any
tasks needed to use it (e.g. link a shader program). Returns T if finalize actions were
performed, NIL otherwise."))

(defgeneric gl-finalized-p (obj)
  (:documentation "Return T if object has already been finalized."))

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
(defclass render-queue ()
  ((bundles :accessor bundles :initarg :bundles :initform nil)))

(defgeneric add-bundle (render-queue bundle)
  (:documentation "Add BUNDLE to RENDER-QUEUE.

This function is used in the implementation of SUBMIT-WITH-EFFECT."))

(defmethod add-bundle ((render-queue render-queue) bundle)
  (push bundle (bundles render-queue)))

(defgeneric remove-bundle (render-queue bundle))

(defmethod remove-bundle ((render-queue render-queue) bundle)
  (setf (bundles render-queue) (delete bundle (bundles render-queue))))

;;; holds multiple render queues. These will be rendered in order.
(defclass render-stage (render-queue)
  ((render-queues :accessor render-queues :initarg :render-queues :initform nil)))

(defclass renderer ()
  ((buffers :accessor buffers :initform nil)
   (bundles :accessor bundles :initform nil)
   (current-state :accessor current-state :initform nil)
   (predraw-queue :accessor predraw-queue :initform nil)
   (finalize-queue :accessor finalize-queue :initform nil)
   ;; alist of (buffer . buffer-areas)
   (upload-queue :accessor upload-queue :initform nil)
   (render-stages :accessor render-stages :initform nil)
   ;; XXX Should be weak
   (vao-cache :accessor vao-cache :initform (make-hash-table :test 'equal))
   (gl-objects :accessor gl-objects :initform nil :documentation "List of all OpenGL objects
allocated by calls in LPSG." ))
  (:documentation "The class responsible for all rendering."))

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
  GL-FINALIZE is called on the GL-BUFFER object.")
   (usage :accessor usage :initarg :usage
          :documentation "Usage hint for the buffer object. Value is a CL-OPENGL keyword e.g.,
  :STATIC-DRAW.")
   (target :accessor target :initarg :target
           :documentation "OpenGL targert for the buffer object. Value is a CL-OPENGL keyword e.g.,
  :ARRAY-BUFFER."))
  (:default-initargs :target :array-buffer :usage :static-draw :size +default-buffer-size+)
  (:documentation "A buffer object allocated in OpenGL.

In OpenGL, the USAGE and TARGET parameters are hints and it is legal to use a buffer differently,
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
                    :documentation "The total number of vertices in this geometry.")))

(defclass array-drawable (drawable)
  ((first-vertex :accessor first-vertex :initarg :first-vertex
                 :documentation "The starting index in the enabled arrays.")))

(defclass indexed-drawable (drawable)
  ((index-type :accessor index-type :initarg :index-type)
   (base-vertex :accessor base-vertex :initarg :base-vertex)
   (element-array :accessor element-array :initarg :element-array))
  (:default-initargs :index-type :unsigned-short :base-vertex 0))

(defclass buffer-area ()
  ((buffer :accessor buffer :initarg :buffer)
   (resource-size :accessor resource-size :initarg :resource-size
                  :documentation "size in bytes of resource's data in buffer")
   (components :accessor components :initarg :components
               :documentation "number of components in each element of an attribute")
   (buffer-type :accessor buffer-type :initarg :buffer-type
                :documentation "GL format of data in buffer")
   (normalizedp :accessor normalizedp :initarg :normalizedp :initform nil)
   (stride :accessor stride :initarg :stride :initform 0)
   (offset :accessor offset :initarg :offset :initform 0
           :documentation "Offset used when binding a buffer with e.g., %gl:vertex-attrib-pointer."))
  (:documentation "class for formatted data stored somewhere in a buffer"))

(defgeneric upload-fn (buffer-area)
  (:documentation "function taking (BUFFER-AREA POINTER) ???"))

(defmethod upload-fn ((obj buffer-area))
  (error "No upload function defined."))

(defgeneric add-to-upload-queue (renderer buffer-area))

(defmethod add-to-upload-queue ((renderer renderer) (obj buffer-area))
  (let* ((buffer (buffer obj))
         (entry (assoc buffer (upload-queue renderer))))
    (if entry
        (push obj (cdr entry))
        (setf (upload-queue renderer) (acons buffer (list obj) (upload-queue renderer))))))

(defun do-upload-queue (renderer)
  (loop
     for (buffer . uploads) in (upload-queue renderer)
     for target = (target buffer)
     do (progn
          (gl:bind-buffer target (id buffer))
          (let ((ptr (gl:map-buffer target :write-only)))
            (mapc (lambda (area)
                    (funcall (upload-fn area) area ptr))
                  uploads))
          (gl:unmap-buffer target)))
  ;; Is this necessary? Should all the targets be set to 0?
  (gl:bind-buffer :array-buffer 0))

(defmethod gl-finalized-p ((obj buffer-area))
  (gl-finalized-p (buffer obj)))

(defmethod gl-finalize ((obj buffer-area) &optional errorp)
  (gl-finalize (buffer obj) errorp))

(defclass buffer-map () ())

(defclass attribute-set ()
  ((array-bindings :accessor array-bindings :initarg :array-bindings :initform nil
                   :documentation "list of (name vertex-attribute index). NAME is the string name
  in the shader program. INDEX is -1 if not valid.")
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
               when index
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


(defclass graphics-state ()
  ((bindings)
   (program :accessor program :initarg :program :initform nil))
  (:documentation "Class that stores all OpenGL state."))

(defmethod gl-finalized-p ((obj graphics-state))
  (gl-finalized-p (program obj)))

(defmethod gl-finalize ((obj graphics-state) &optional errorp)
  (gl-finalize (program obj) errorp))

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
  ((status :accessor status :initarg :status)
   (compiler-log :accessor compiler-log :initarg :compiler-log :initform nil))
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
  ((shaders :accessor shaders :initarg :shaders :initform nil)
   (compiled-shaders :accessor compiled-shaders :initform nil)
   ;; (name location type size)
   (uniforms :accessor uniforms :initform nil
             :documentation "Information on uniforms declared within
  the program shader source.") 
   (status :accessor status :initarg :status)
   (link-log :accessor link-log :initarg :link-log :initform nil)
   ;; elements are (desc strategy (most-recent-uset counter))
   (uset-alist :accessor uset-alist :initform nil)
   (vertex-attribs :accessor vertex-attribs :initform nil)))

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
         (uset-entry (assoc descriptor (uset-alist program)))
         (strategy (cadr uset-entry)))
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
  (:documentation "Perform all outstanding operations in RENDERER.

Finalize all objects on the finalize queue(s), do any upload operations in the upload queue, then
traverse the render stages and their render queues to render all bundles."))

(defgeneric draw-bundles (renderer))

(defgeneric bind-state (renderer state))

(defmethod bind-state ((renderer renderer) (state graphics-state))
  (with-slots (current-state)
      renderer
    (when (eq current-state state)
      (return-from bind-state nil))
    (let* ((old-program (and current-state (program current-state)))
           (new-program (program state)))
      (unless (eq new-program old-program)
        (gl:use-program (id new-program)))
        ;; XXX bindings
      (setf (current-state renderer) state))))

(defgeneric process-gl-objects (renderer))

(defmethod draw ((renderer renderer))
  (let ((*renderer* renderer))
    (process-gl-objects renderer)
    (loop
       for fn in (predraw-queue renderer)
       do (funcall fn renderer))
    (setf (predraw-queue renderer) nil)
    (process-finalize-queue renderer)
    (setf (finalize-queue renderer) nil)
    (do-upload-queue renderer)
    (setf (upload-queue renderer) nil)
    (draw-bundles renderer)))

(defgeneric draw-bundle (renderer bundle))

(defmethod draw-bundles ((renderer renderer))
  ;; XXX Should we set the state to something known here?
  (setf (current-state renderer) nil)
  (loop
     for stage in (render-stages renderer)
     do (loop
           for rq in (render-queues stage)
           do (loop
                 for bundle in (bundles rq)
                 do (draw-bundle renderer bundle)))))

(defmethod process-gl-objects ((renderer renderer))
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

(defgeneric close-renderer (renderer))

(defmethod close-renderer ((renderer renderer))
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
  (let ((size (cdr (assoc type *buffer-size-alist*))))
    (or size
        (error 'render-error :format-control "Unknown OpenGL type %s."
               :format-arguments type))))
