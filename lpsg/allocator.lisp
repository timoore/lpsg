;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

(defgeneric open-allocator (allocator)
  (:documentation "Prepare ALLOCATOR for allocation."))

(defgeneric allocate-target (allocator target size alignment)
  (:documentation "Allocate SIZE storage with ALIGNMENT from a buffer object that will TARGET
  usage.

Returns values (buffer offset-in-buffer allocated-size)"))

(defgeneric close-allocator (allocator)
  (:documentation "Stop allocating from ALLOCATOR's buffers."))

(defclass allocator ()
  ())

(defclass simple-allocator (allocator)
  ((buffers :accessor buffers :initform nil
            :documentation "alist of (target . buffer)"))
  (:documentation "Class for managing allocations from several buffer objects.

Allocations are made from one buffer per target, with no provision for freeing storage."))

(defun round-up (val divisor)
  (* (ceiling val divisor) divisor))

(defmethod open-allocator ((alloc simple-allocator))
  nil)

(defmethod allocate-target ((alloc simple-allocator) target size alignment)
  (let ((target-alloc (assoc target (buffers alloc))))
    (unless target-alloc
      (setq target-alloc (cons target (make-instance 'gl-buffer :target target :size 0)))
      (push target-alloc (buffers alloc)))
    (let* ((buf (cdr target-alloc))
           (current (size buf))
           (alloc-offset (round-up current alignment))
           (new-size (+ alloc-offset size)))
      (setf (size buf) new-size)
      (values buf alloc-offset new-size))))

(defmethod close-allocator ((alloc simple-allocator))
  (setf (buffers alloc) nil))

;;; The freelist allocator seemed like a good idea at the time, but is probably unnecessary for
;;; most buffer uses.

(defclass freelist-allocator ()
  ((free-list :accessor free-list :initform nil)))

(defmethod initialize-instance :after ((obj freelist-allocator) &key size)
  (when size
    (push (list 0 size) (free-list obj))))

(defgeneric allocate (allocator size &optional alignment))

(defmethod allocate ((allocator freelist-allocator) size &optional (alignment 4))
  (let ((rounded-size (* (ceiling size alignment) alignment)))
    (loop
       for region in (free-list allocator)
       for (offset region-size) = region
       if (>= region-size rounded-size)
       do (progn
            (if (eql rounded-size region-size)
                (setf (free-list allocator) (delete region (free-list allocator)))
                (setf (car region) (+ offset rounded-size)
                      (cadr region) (- region-size rounded-size)))
            (return-from allocate (values offset rounded-size))))
    nil))

;;; Obsolete.  
(defun deallocate-in-buffer (buffer allocation)
  (push allocation (free-list buffer)))

(defun release-buffer (buffer)
  (gl:delete-buffers (list (id buffer)))
  (setf (id buffer) 0)
  nil)
