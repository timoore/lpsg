;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

(defclass mirrored-resource (buffer-area)
  ((data :accessor data :initarg :data)
   (data-offset :accessor data-offset :initarg :data-offset :initform 0)
   (data-size :accessor data-size :initarg :data-size :initform 0)
   (data-stride :accessor data-stride :initarg :data-stride :initform 0
                :documentation "number of elements")
   (num-components :accessor num-components :initarg :num-components)
   (upload-fn :accessor upload-fn :initarg :upload-fn
              :documentation "Function to upload Lisp data to a mapped buffer.
Will be created automatically, but must be specified for now.")))

(defmethod initialize-instance :after ((obj mirrored-resource) &key)
  (unless (slot-boundp obj 'num-components)
    (setf (num-components obj) (components obj))))

;;; An upload function that should work for floats

(defun upload-resource-float (resource buffer-ptr)
  (let ((real-data-stride (if (zerop (data-stride resource))
                              (num-components resource)
                              (data-stride resource)))
        (effective-stride (if (zerop (stride resource))
                              (* (components resource) 4)
                              (stride resource))))
    (loop
       for i from 0 below (data-size resource) ; ??? Is this how limit should be specified?
       for src-idx = (+ (data-offset resource)  (* i real-data-stride))
       for dest = (cffi:inc-pointer buffer-ptr (+ (* i effective-stride)))
       do (setf (mem-aref dest :float) (float (row-major-aref data srd-idx) 1.0)))))

(defun upload-resource-short (resource buffer-ptr)
  (let ((real-data-stride (if (zerop (data-stride resource))
                              (num-components resource)
                              (data-stride resource)))
        (effective-stride (if (zerop (stride resource))
                              (* (components resource) 2)
                              (stride resource))))
    (loop
       for i from 0 below (data-size resource) ; ??? Is this how limit should be specified?
       for src-idx = (+ (data-offset resource)  (* i real-data-stride))
       for dest = (cffi:inc-pointer buffer-ptr (+ (* i effective-stride)))
       do (setf (mem-aref dest :short) (row-major-aref data srd-idx)))))

(defmethod initialize-instance :after ((obj mirrored-resource) &key)
  (unless (slot-boundp obj 'upload-fn)
    (when (slot-boundp obj 'buffer-type)
      (case (buffer-type obj)
        (:float
         (setf (upload-fn obj) #'upload-resource-float))
        (:short
         (setf (upload-fn obj) #'upload-resource-short))))))

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

(defclass shape (consumer-node)
  ((attributes :accessor attributes :initarg :attributes :initform nil
               :documentation "alist of (name . attribute)")
   (environment :accessor environment :initarg :environment :initform nil)
   (effect :accessor effect :initarg :effect)
   ;; XXX uset computation nodes?
   (usets :accessor usets :initarg :usets :initform nil)
   (drawable :accessor drawable :initarg :drawable :initform nil)
   ;; XXX Bundles?
   (bundle :accessor bundle)))

(defmethod initialize-instance :after ((obj shape) &key)
  (setf (drawable obj) (make-instance 'drawable)))

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

#|
;;; Turn a shape into something that can be rendered.

(defmethod gl-finalized-p ((obj shape))
  (and (bundle obj)
       (gl-finalized-p (environment obj))
       (gl-finalized-p (attributes obj))
       (gl-finalized-p (bundle obj))))

;;; Allocate buffers for attributes
;;; upload?
;;; Finalize environment: shader, textures
;;; create the vertex attribute binding
;;; finish bundle

(defmethod gl-finalize ((obj shape) &optional errorp)
  (flet ((maybe-finalize (obj)
           (or (gl-finalized-p obj) (gl-finalize obj errorp))))
    (maybe-finalize (environment obj))
    (mapc (lambda (attr-pair) (maybe-finalize (cdr attr-pair))) (attributes obj))
    ;; Find location of each vertex attribute
    (let* ((locations (mapcar (lambda (attr-pair)
                                (cons (attribute-array-location (car attr-pair)
                                                                (environment obj))
                                      (cadr attr-pair)))
                              (attributes obj)))
           ;; What about attributes that are not active in the environment's shader?
           (attr-set (make-instance 'attribute-set :array-binding locations)))
      (when (bundle obj)
        (remove-bundle *renderer* bundle)) ;?? necessary?
      (maybe-finalize attr-set)
      (let ((gl-state (make-state environment)))
        ;; hook up usets
        (setf (bundle obj) (make-instance 'render-bundle
                                          :drawable (drawable obj)
                                          :gl-state gl-state
                                          :attribute-set attr-set)))
      (add-bundle *renderer* (bundle obj)))))
|#

;;; defmethod is here because it uses methods on SHAPE.

(defmethod submit (assembly renderer)
  (do-shapes assembly
    (lambda (shape)
      (submit-with-effect shape renderer (effect shape)))))

(defmethod submit-with-effect :after ((shape shape) renderer effect)
  (declare (ignore effect))
  (loop
     for (name . vertex-attrib) in (attributes shape)
     do (progn
          ;; XXX Is vertex attrib mirrored? Does it actually need uploading?
          ;; Is there a better way than typep?
          (when (typep vertex-attrib 'mirrored-resource)
            (add-to-upload-queue renderer vertex-attrib)))))
