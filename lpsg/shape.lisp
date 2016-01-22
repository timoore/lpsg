;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

(defclass mirrored-resource (buffer-area)
  ((data :accessor data :initarg :data)
   (data-offset :accessor data-offset :initarg :data-offset :initform 0)
   (data-count :accessor data-count :initarg :data-count :initform 0
               :documentation "number of elements")
   (data-stride :accessor data-stride :initarg :data-stride :initform 0
                :documentation "offset between start of each element")
   (num-components :accessor num-components :initarg :num-components
                   :documentation "number of components per element. Redundant
  with buffer-area components?")
   (buffer-offset :accessor buffer-offset :initarg :buffer-offset :initform 0
                  :documentation "Offset of the data in the target buffer when it is mapped.")
   (upload-fn :accessor upload-fn :initarg :upload-fn
              :documentation "Function to upload Lisp data to a mapped buffer.
Will be created automatically, but must be specified for now.")))

;;; An upload function that should work for floats

(defun upload-resource-float (resource buffer-ptr)
  (let ((real-data-stride (if (zerop (data-stride resource))
                              (num-components resource)
                              (data-stride resource)))
        (effective-stride (if (zerop (stride resource))
                              (* (components resource) 4)
                              (stride resource)))
        (data (data resource)))
    (loop
       for i from 0 below (data-count resource)
       for src-idx = (+ (data-offset resource)  (* i real-data-stride))
       for dest = (cffi:inc-pointer buffer-ptr (+ (buffer-offset resource) (* i effective-stride)))
       do (loop
             for j from 0 below (num-components resource)
             for dest-component = (cffi:inc-pointer dest (* j 4))
             do (setf (cffi:mem-aref dest-component :float)
                      (float (row-major-aref data (+ src-idx j)) 1.0))))))

(defun upload-resource-short (resource buffer-ptr)
  (let ((real-data-stride (if (zerop (data-stride resource))
                              (num-components resource)
                              (data-stride resource)))
        (effective-stride (if (zerop (stride resource))
                              (* (components resource) 2)
                              (stride resource)))
        (data (data resource)))
    (loop
       for i from 0 below (data-count resource)
       for src-idx = (+ (data-offset resource)  (* i real-data-stride))
       for dest = (cffi:inc-pointer buffer-ptr (+ (buffer-offset resource) (* i effective-stride)))
       do (loop
             for j from 0 below (num-components resource)
             for dest-component = (cffi:inc-pointer dest (* j 2))
             do (setf (cffi:mem-aref dest-component :short)
                      (row-major-aref data (+ src-idx j)))))))

(defmethod initialize-instance :after ((obj mirrored-resource) &key)
  (unless (slot-boundp obj 'num-components)
    (setf (num-components obj) (components obj)))
  (when (and (not (slot-boundp obj 'upload-fn)) (slot-boundp obj 'buffer-type))
    (case (buffer-type obj)
      (:float
       (setf (upload-fn obj) #'upload-resource-float))
      (:short
       (setf (upload-fn obj) #'upload-resource-short)))))

;;; Definition of an individual vertex attribute
;;;
;;; data - a Lisp array. Data will be taken from consecutive elements of
;;; the array, as by row-major-aref, according to format.
;;;
;;; format - OpenGL format
;;;
;;; buffer - The OpenGL buffer that is the target for the attribute. A gl-buffer
;;;
;;; offset - offset from beginning of buffer to the beginning of data. For a 2d
;;; array, the offset is used on each row of data.
;;;
;;; stride - stride of data in buffer

(defclass vertex-attribute (mirrored-resource)
  ())

;;; attributes - alist of (name . vertex-attribute). 

(defclass shape (sink-node sink-node-mixin)
  ((attributes :accessor attributes :initarg :attributes :initform nil
               :documentation "Alist of (name . attribute). The names are later mapped to a vertex binding index.")
   (effect :accessor effect :initarg :effect)
   ;; XXX uset computation nodes?
   (usets :accessor usets :initarg :usets :initform nil)
   (drawable :accessor drawable :initarg :drawable)
   (bundles :accessor bundles :initform nil
            :documentation "The bundles that are created by EFFECT for this shape."))
  (:documentation "Class for geometry coupled with an effect.

When a shape is submitted to the renderer, all its input nodes are copied into the
environment objects that might be created. This means that a shape's inputs cannot be setf'ed after
submission.

Inputs

visiblep - true if shape is visible, false if not
"))

(defmethod initialize-instance :after ((obj shape) &key (visiblep t))
  (unless (slot-boundp obj 'drawable)
    (setf (drawable obj) (make-instance 'drawable)))
  (setf (input obj 'visiblep) visiblep))

(defgeneric attribute (obj attribute-name))

(defmethod attribute ((obj shape) attribute-name)
  (cdr (assoc attribute-name (attributes obj) :test #'equal)))

(defmethod (setf attribute) (attrib (obj shape) attribute-name)
  (let ((cell (assoc attribute-name (attributes obj) :test #'equal)))
    (if cell
        (setf (cdr cell) attrib)
        (setf (attributes obj) (acons attribute-name attrib (attributes obj))))
    attrib))

(defmethod ensure-attribute ((obj shape) attribute-name)
  (let ((attrib (attribute obj attribute-name)))
    (or attrib
        (setf (attribute obj attribute-name)
              (make-instance 'vertex-attribute)))))

;;; Delegate to drawable

(defmethod mode ((obj shape))
  (mode (drawable obj)))

(defmethod (setf mode) (mode (obj shape))
  (setf (mode (drawable obj)) mode))

(defmethod number-vertices ((obj shape))
  (number-vertices (drawable obj)))

(defmethod (setf number-vertices) (num (obj shape))
  (setf (number-vertices (drawable obj)) num))

;;; Utilities for allocation

(defun attributes-descriptor (shape)
  (mapcar (lambda (attr-entry)
            (let ((attr (cdr attr-entry)))
              (list (buffer-type attr) (components attr) (normalizedp attr))))
          (attributes shape)))

(defun max-attribute-alignment (descriptor)
   (reduce #'max descriptor
           :key (lambda (e) (get-component-size (car e)))
           :initial-value 4))

(defclass interleaved-attribute-allocator ()
  ((allocator-alist :accessor allocator-alist :initform nil)))

(defgeneric compute-shape-allocation (allocator shape))


(defmethod open-allocator ((allocator interleaved-attribute-allocator))
  (setf (allocator-alist allocator) nil))

(defmethod close-allocator ((allocator interleaved-attribute-allocator))
  (mapc (lambda (cell) (close-allocator (cdr cell))) (allocator-alist allocator)))

(defvar *open-allocators* nil)


;;; Interleaved allocation


(defun attribute-offsets (attr-desc)
  "Returns list-of-strides, total padded size"
  (let ((total-alignment (max-attribute-alignment attr-desc)))
    (loop
       with current-size = 0
       for (buf-type num-components) in attr-desc
       for component-size = (get-component-size buf-type)
       for alignment = (min 4 component-size)
       for element-size = (* component-size num-components)
       for offset = (round-up current-size alignment)
       collect offset into offsets
       do (incf current-size element-size)
       finally (let ((padded-size (round-up current-size total-alignment)))
                 (return (values offsets padded-size))))))

(defmethod compute-shape-allocation ((interleaved-allocator interleaved-attribute-allocator)
                                     (shape shape))
  (let* ((attr-descriptor (attributes-descriptor shape))
         (allocator (cdr (assoc attr-descriptor (allocator-alist interleaved-allocator)
                                :test #'equal)))
         (drawable (drawable shape)))
    (unless allocator
      (setq allocator (make-instance 'simple-allocator))
      (setf (allocator-alist interleaved-allocator)
            (acons attr-descriptor allocator (allocator-alist interleaved-allocator))))
    (multiple-value-bind (offsets vertex-size)
        (attribute-offsets attr-descriptor)
      (multiple-value-bind (buffer offset)
          (allocate-target allocator
                           :array-buffer
                           (* vertex-size (data-count (cdar (attributes shape)))) ;XXX
                           4)
        ;; The arrays are allocated as if they start at the beginning of the buffer...
        (mapc (lambda (cell attr-offset)
                (let ((attr (cdr cell)))
                  (setf (buffer attr) buffer
                        (offset attr) attr-offset
                        (buffer-offset attr) (+ offset attr-offset)
                        (stride attr) vertex-size)))
              (attributes shape) offsets)
        ;; ... and the base-vertex is set to make the indices point to the actual location of the
        ;; data.
        (when (typep drawable 'indexed-drawable)
          (let* ((attr (element-array drawable))
                 (index-size (get-component-size (buffer-type attr))))
            (multiple-value-bind (index-buffer index-offset)
                (allocate-target
                 allocator :element-array-buffer (* index-size (data-count attr)) index-size)
              (setf (buffer attr) index-buffer
                    (buffer-offset attr) index-offset
                    (base-vertex drawable) (/ offset vertex-size)))))))))


(defmethod compute-shape-allocation ((allocator simple-allocator) (shape shape))
  (flet ((allocate-attr (attr target)
           (let* ((component-size (get-component-size (buffer-type attr)))
                  (aligned-size (max component-size 4)))
             (multiple-value-bind (buffer offset)
                 (allocate-target allocator
                                  target
                                  (* component-size (components attr) (data-count attr))
                                  aligned-size)
               (setf (buffer attr) buffer
                     (offset attr) offset)))))
    (loop
       for (nil . attr) in (attributes shape)
       do (allocate-attr attr :array-buffer))
    (when (typep (drawable shape) 'indexed-drawable)
      (allocate-attr (element-array (drawable shape)) :element-array-buffer))))

(defmethod submit ((shape shape) renderer)
  "Submit SHAPE to RENDERER.

Calls (submit-with-effect SHAPE RENDERER (effect SHAPE))"
  (submit-with-effect shape renderer (effect shape)))

(defmethod retract ((shape shape) renderer)
  (retract-with-effect shape renderer (effect shape)))

(defmethod submit-with-effect :after ((shape shape) renderer effect)
  (declare (ignore effect))
  (loop
     for (name . vertex-attrib) in (attributes shape)
     do (progn
          ;; XXX Is vertex attrib mirrored? Does it actually need uploading?
          ;; Is there a better way than typep?
          (when (typep vertex-attrib 'mirrored-resource)
            (add-to-upload-queue renderer vertex-attrib))))
  (when (typep (drawable shape) 'indexed-drawable)
    (add-to-upload-queue renderer (element-array (drawable shape)))))

(defmethod retract ((shape shape) renderer)
  (retract-with-effect shape renderer (effect shape)))

;;; incremental computation stuff
;;; The "value" of a shape node is a list of (uset-name . uset-value).

(defmethod notify-invalid-input ((node shape) source input-name)
  nil)


