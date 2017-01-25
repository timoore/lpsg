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

(defmethod gl-finalize :around ((obj t) &optional errorp)
  (if (gl-finalized-p obj)
      nil
      (call-next-method)))

(defmethod gl-finalize (obj &optional errorp)
  (declare (ignore errorp))
  t)

(defmethod gl-finalize :around ((object gl-object) &optional errorp)
  (let ((result (call-next-method)))
    (when result
      (push (cons (tg:make-weak-pointer object) (gl-proxy object)) (gl-objects *renderer*)))
    result))

(defmethod gl-finalized-p (obj)
  (declare (ignore obj))
  t)

(defclass queue-state-mixin ()
  ((graphics-state :accessor graphics-state :initarg :graphics-state)
   (bind-state-p :accessor bind-state-p :initarg :bind-state-p :initform nil))
  (:documentation "A queue that contains a graphics state to be pushed and popped in an around method on
DRAW-QUEUE. @c(bind-state) indicates if the graphics state should also be bound."))

(defmethod draw-queue :around (renderer (render-queue queue-state-mixin))
  (declare (ignorable func))
  (if (slot-boundp render-queue 'graphics-state)
      (progn
        (if (bind-state-p render-queue)
            (bind-and-push-state renderer (graphics-state render-queue))
            (push-state renderer (graphics-state render-queue)))
        (call-next-method)
        (pop-state renderer))
      (call-next-method)))

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

(defclass ordered-queue ()
  ((queue-object :initform (serapeum:queue)))
  (:documentation "CLOS wrapper around serapeum:queue."))

(defgeneric add-to-queue (queue object))

(defgeneric remove-from-queue (queue object))

(defgeneric map-queue (queue function))

(defgeneric find-if-queue (predicate queue))

(defmethod add-to-queue ((queue ordered-queue) object)
  (with-slots (queue-object)
      queue
    (serapeum:enq object queue-object)
    nil))

(defmethod remove-from-queue ((queue ordered-queue) object)
  (with-slots (queue-object)
      queue
    (let ((contents (serapeum:clear-queue queue-object)))
      (serapeum:qconc queue-object (delete object contents)))
    nil))

(defmethod map-queue ((queue ordered-queue) function)
  (mapc function (serapeum:qlist (slot-value queue 'queue-object))))

(defmethod find-if-queue (predicate (queue ordered-queue))
  (find predicate (serapeum:qlist (slot-value queue 'queue-object))))

(defclass ordered-render-queue (ordered-queue render-queue)
  ()
  (:documentation "Class for a queue of objects that are rendered in order.

@c(add-rendered-object) will add an object to the end of the queue."))

(defmethod add-rendered-object ((render-queue ordered-render-queue) object)
  (add-to-queue render-queue object))

(defmethod remove-rendered-object ((render-queue ordered-render-queue) object)
  (remove-from-queue render-queue object))

(defmethod map-render-queue ((render-queue ordered-render-queue) function)
  (map-queue render-queue function))


(defmethod draw-queue (renderer (render-queue render-queue))
  (flet ((map-func (item)
           (draw-queue renderer item)))
    (declare (dynamic-extent map-func))
    (map-render-queue render-queue #'map-func)))

;;; holds multiple render queues. These will be rendered in order.
(defclass render-stage (queue-state-mixin ordered-render-queue)
  ())


(defclass render-target-stage (render-stage)
  ((clear-colors :accessor clear-colors :initform '(#(0.0 0.0 0.0 0.0)) :initarg :clear-colors
                 :documentation "list of color vectors")
   (depth-clear :accessor depth-clear :initform 1.0 :initarg :depth-clear)
   (framebuffer-object :accessor framebuffer-object :initarg :framebuffer-object
                       :initform (make-instance 'gl-framebuffers)))
  (:default-initargs :bind-state-p t)
  (:documentation "A render queue with designated read and draw buffers @i([default for now])"))

(defmethod initialize-instance :after ((obj render-target-stage) &key)
  (unless (slot-boundp obj 'graphics-state)
    (setf (graphics-state obj)
          (make-instance 'graphics-state :framebuffers (framebuffer-object obj)))))

(defmethod draw-queue :before (renderer (render-queue render-target-stage))
  (cffi:with-foreign-object (gl-col '%gl:float 4)
    (flet ((copy-color (color)
             (loop
                for i from 0 below 4
                do (setf (cffi:mem-aref gl-col '%gl:float i) (aref color i)))))
      (if (is-default-p (framebuffer-object render-queue))
          (progn
            (copy-color (car (clear-colors render-queue)))
            (%gl:clear-buffer-fv :color 0 gl-col))
          (loop
             for color in (clear-colors render-queue)
             for i from 0
             do
               (copy-color color)
               (%gl:clear-buffer-fv :color i gl-col)))
      (%gl:clear-buffer-fi :depth-stencil 0 (depth-clear render-queue) 0))))

(defclass glcontext-parameters ()
  ((max-combined-texture-image-units :accessor max-combined-texture-image-units
                                     :initarg :max-combined-texture-image-units))
  (:default-initargs :max-combined-texture-image-units 8)) ;XXX way to small, for testing

(defclass draw-update-queue ()
  ((update-queue :accessor update-queue :initform (make-instance 'ordered-queue))))

(defclass buffer-object-upload-queue ()
  ((bo-queue :accessor bo-queue :initform nil)))

(defclass texture-upload-queue ()
  ((tex-queue :accessor tex-queue :initform nil)))

(defclass upload-queue (draw-update-queue buffer-object-upload-queue texture-upload-queue)
  ())

(defgeneric update-object-for-draw (renderer object)
  (:documentation "Generic function calledd when draw update queue is processed."))

(defclass standard-renderer (glstate-tracker renderer)
  ((buffers :accessor buffers :initform nil :documentation "private")
   (bundles :accessor bundles :initform nil :documentation "private")
   (predraw-queue :accessor predraw-queue :initform nil :documentation "private")
   (finalize-queue :accessor finalize-queue :initform nil :documentation "private")
   ;; alist of (buffer . buffer-areas)
   (upload-queue :accessor upload-queue :initform (make-instance 'upload-queue) :documentation "private")
   (render-stage :accessor render-stage :initarg :render-stage
                 :initform (make-instance 'render-target-stage)
                 :documentation "The top-level (default) render stage.")
   ;; XXX Should be weak
   (vao-cache :accessor vao-cache :initform (make-hash-table :test 'equal) :documentation "private")
   (gl-objects :accessor gl-objects :initform nil :documentation "List of all OpenGL objects
allocated by calls in LPSG." )
   (context-parameters :accessor context-parameters
                       :documentation "Parameters of the OpenGL context.")
   (default-graphics-state :accessor default-graphics-state :initarg :default-graphics-state
                           :initform *default-graphics-state*)
   (default-render-queue :accessor default-render-queue :initarg :default-render-queue))
  (:documentation "The standard instantiable class of @c(renderer)."))


;;; XXX Temporary until we figure out how how / when to intialize the renderer from a graphics
;;; context.

(defmethod initialize-instance :after ((obj standard-renderer) &key)
  (setf (context-parameters obj) (make-instance 'glcontext-parameters))
  (unless (slot-boundp obj 'default-render-queue)
    (setf (default-render-queue obj) (render-stage obj))))

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

(defgeneric remove-upload (renderer object)
  (:documentation "Cancel the upload of a previously scheduled object."))

(defgeneric add-to-upload-queue (queue object))

(defgeneric remove-from-upload-queue (queue object))

(defmethod schedule-upload ((renderer standard-renderer) object)
  (add-to-upload-queue (upload-queue renderer) object))

(defmethod remove-upload ((renderer standard-renderer) object)
  (remove-from-upload-queue (upload-queue renderer) object))

(defmethod add-to-upload-queue ((queue buffer-object-upload-queue) (obj buffer-area))
  (let* ((buffer (buffer obj))
         (entry (assoc buffer (bo-queue queue))))
    (if entry
        (push obj (cdr entry))
        (setf (getassoc buffer (bo-queue queue)) (list obj)))))

(defmethod remove-from-upload-queue ((queue buffer-object-upload-queue) (obj buffer-area))
  (let* ((buffer (buffer obj))
         (entry (assoc buffer (bo-queue queue))))
    (when entry
      (let ((tail (setf (cdr entry) (delete obj (cdr entry)))))
        (unless tail
          (setf (bo-queue queue) (delete entry (bo-queue queue) :test #'eq)))))
    nil))

(defgeneric schedule-update (renderer object)
  (:documentation "Register an object to be updated at every draw call."))

(defgeneric remove-update (renderer object)
  (:documentation "Remove object that has been registered for update."))

(defmethod schedule-update ((renderer standard-renderer) object)
  (add-to-queue (update-queue (upload-queue renderer)) object))

(defmethod remove-update ((renderer standard-renderer) object)
  (remove-from-queue (update-queue (upload-queue renderer)) object))

;;; TODO: Arrange queue by texture object. Push areas onto the end of the queue.
(defmethod add-to-upload-queue ((queue texture-upload-queue) (obj texture-area))
  (push obj (tex-queue queue)))

(defgeneric process-upload-queue (renderer queue))

(defmethod process-upload-queue (renderer queue)
  )

(defmethod process-upload-queue :before (renderer (queue draw-update-queue))
  (map-queue (update-queue (upload-queue renderer))
             (lambda (object)
               (update-object-for-draw renderer object))))

(defmethod process-upload-queue :before (renderer (queue buffer-object-upload-queue))
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

(defmethod process-upload-queue :before (renderer (queue texture-upload-queue))
  (loop
     for area in (tex-queue queue)
     for texture = (texture area)
     do (upload-texture renderer area texture)
     finally (setf (tex-queue queue) nil)))

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

(defclass shader-source (gl-shader-source)
  ((usets :accessor usets)))
   
(defmethod initialize-instance :after ((obj shader-source) &key usets)
  (setf (usets obj) (mapcar #'(lambda (uset-name)
                                (or (get-uset-descriptor uset-name)
                                    (error "Uset ~S is not defined." uset-name)))
                            usets)))
 
;;; A shader object could be different for different programs because the uset
;;; strategies might be different. Should we keep a cache of objects for a set
;;; of uset strategies?


(defclass shader (shader-source gl-shader)
  ()
  (:documentation "The LPSG object that holds the source code for an OpenGL shader, information
  about its usets, ID in OpenGL, and any errors that result from its compilation."))

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
    (push-state renderer (default-graphics-state renderer))
    (draw-bundles renderer)
    (pop-state renderer)))

(defmethod draw-bundles ((renderer standard-renderer))
  (increment-frame-count renderer)
  (draw-queue renderer (render-stage renderer)))

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
