;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

(defclass mirrored-resource (buffer-area)
  ((data :accessor data :initarg :data)
   (data-offset :accessor data-offset :initarg :data-offset :initform 0)
   (data-size :accessor data-size :initarg :data-size :initform 0)
   (data-stride :accessor data-stride :initarg :data-stride :initform 0
                :documentation "number of elements")
   (num-components :accessor num-components :initarg :num-components)))

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

(defclass shape ()
  ((attributes :accessor attributes :initarg :attributes :initform nil)
   (environment :accessor environment :initarg :environment :initform nil)
   (drawable :accessor drawable)
   (bundle :accessor bundle)))

(defmethod initialize-instance ((obj shape) &key)
  (setf (drawable obj) (make-instance 'drawable)))

(defgeneric attribute (obj attribute-name))

(defmethod attribute ((obj shape) attribute-name)
  (cdr (assoc attribute-name (attributes obj) :test #'equal)))

(defmethod (setf attribute) (attrib (obj shape) attribute-name)
  (let ((cell (assoc attribute-name (attributes obj) :test #'equal)))
    (if cell
        (setf (cdr cell) attrib)
        (setf (attributes obj) (acons attribute-name attrib)))
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
    (mapc (lambda (attr-pair) (maybe-finalize (cdr attr-pair)))
          (attributes obj))
    ;; Find location of each vertex attribute
    (let* ((locations (mapcar (lambda (attr-pair)
                                `(,@attr-pair ,(attribute-array-location (car attr-pair)
                                                                         (environment obj))))
                              (attributes obj)))
           (attr-set (make-instance 'attribute-set
                                    :array-binding (mapcar (lambda (loc)
                                                             `(,(caddr loc) . ,(cadr loc)))
                                                           locations))))
      (unless (slot-boundp obj 'bundle)
        (setf (bundle obj) (make-instance 'render-bundle :attribute-set attr-set))))))

