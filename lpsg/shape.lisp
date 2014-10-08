;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

(defclass buffered-resource ()
  ((usage :accessor usage :initarg :usage)
   (buffer :accessor buffer :initarg :buffer)
   (offset :accessor offset :initarg :offset :initform 0)
   (stride :accessor stride :initarg :stride :initform 0)
   (size)
   (buffer-type)
   (normalizedp :accessor normalizedp :initarg :normalizedp :initform nil)
   (num-components :accessor num-components :initarg :num-components)
   ))

(defclass mirrored-resource (buffered-resource)
  ((data :accessor data :initarg :data)
   (data-offset :accessor data-offset :initarg :data-offset :initform 0)
   (data-size :accessor data-size :initarg :data-size :initform 0)))

;;; Definition of an individual vertex attribute
;;;
;;; data - a Lisp array. If 1d, data will be taken from consecutive elements of
;;; the array, according to format. If 2d, each data item will be taken from
;;; the beginning of a row in the array. Arrays can be displaced.
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

;;; attributes - alist of (name . vertex-attribute). Name is tested EQUAL to a
;;; semantic from an effect.
(defclass shape ()
  ((attributes :accessor attributes :initarg :attributes :initform nil)
   (effect)
   (geometry :accessor geometry)
   (bundle)))

(defmethod initialize-instance ((obj shape) &key)
  (setf (geometry obj) (make-instance 'geometry)))

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

;;; Delegate to geometry

(defmethod mode ((obj shape))
  (mode (geometry obj)))

(defmethod (setf mode) (mode (obj shape))
  (setf (mode (geometry obj)) mode))

(defmethod number-vertices ((obj shape))
  (number-vertices (geometry obj)))

(defmethod (setf number-vertices) (num (obj shape))
  (setf (number-vertices (geometry obj)) num))

