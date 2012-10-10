;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Copyright (c) 2012, Tim Moore (moore@bricoworks.com)
;;;   All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;;  o Redistributions of source code must retain the above copyright notice,
;;;    this list of conditions and the following disclaimer.
;;;  o Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;  o Neither the name of the author nor the names of the contributors may be
;;;    used to endorse or promote products derived from this software without
;;;    specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package #:lpsg)

;;; A simple reference counting protocol for objects that should do some
;;; cleanup e.g., release OpenGL resources, when they are no longer used.

(defclass reference-counted ()
  ((refcount :accessor refcount :initform 0 :type fixnum)))

(defgeneric ref (obj))

(defmethod ref ((obj reference-counted))
  (incf (slot-value obj 'refcount)))

(defgeneric dereferenced (obj))

(defmethod dereferenced ((obj t))
  nil)

(defgeneric unref (obj))

(defmethod unref ((obj reference-counted))
  (when (zerop (decf (slot-value obj 'refcount)))
    (dereferenced obj)))

(defclass gl-buffer ()
  ((id :accessor id :initarg :id)
   (size :accessor size :initarg :size)
   (usage :accessor usage :initarg :usage)
   (free-list :accessor free-list :initform nil)
   (alloc-tail :accessor alloc-tail)))

(defmethod initialize-instance :after ((obj gl-buffer) &key)
  (setf (alloc-tail obj) (list obj)))

(defun reserve-buffer (target &optional (usage :static-draw) (size 104856))
  (let* ((ids (gl:gen-buffers 1))
         (id (car ids))
         (buf (make-instance 'gl-buffer :id id :size size :usage usage)))
    (gl:bind-buffer target id)
    (%gl:buffer-data target size (cffi:null-pointer) usage)
    (push (list* 0 size (alloc-tail buf)) (free-list buf))
    buf))

(defun allocation-offset (alloc)
  (car alloc))

(defun allocation-size (alloc)
  (cadr alloc))

(defun allocation-buffer (alloc)
  (caddr alloc))

(defun allocate-from-buffer (buffer size &optional (alignment 4))
  (let ((rounded-size (* (ceiling size alignment) alignment)))
    (loop
       for region in (free-list buffer)
       for (offset region-size) = region
       if (>= region-size rounded-size)
       do (progn
            (let ((allocation (list* offset rounded-size (alloc-tail buffer))))
              (if (eql rounded-size region-size)
                  (setf (free-list buffer) (delete region (free-list buffer)))
                  (setf (car region) (+ offset rounded-size)
                        (cadr region) (- region-size rounded-size)))
              (return-from allocate-from-buffer allocation))))
    nil))

(defun deallocate-in-buffer (buffer allocation)
  (push allocation (free-list buffer)))

(defun release-buffer (buffer)
  (gl:delete-buffers (list (id buffer)))
  (setf (car (alloc-tail buffer)) nil))

(defclass geometry (reference-counted)
  ((mode :accessor mode :initarg :mode)
   (number-vertices :accessor number-vertices :initarg :number-vertices)
   (indices :accessor indices :initarg :indices :initform nil)
   (index-usage :accessor index-usage :initarg :index-usage
                :initform :static-draw)
   (vertex-attributes :accessor vertex-attributes :initarg :vertex-attributes
                      :documentation "A symbol (cl-opengl gl-array-format or a
  list of (attrib-number type size")
   (vertex-data :accessor vertex-data :initarg :vertex-data
                :documentation "a single gl-array, or a list of gl-array
  objects corresponding to the vertex attributes")
   (vertex-usage :accessor vertex-usage :initarg :vertex-usage
                 :initform :static-draw)
   (array-buffer :accessor array-buffer)
   (array-buffer-allocation :accessor array-buffer-allocation :initform nil)
   (element-buffer :accessor element-buffer)
   (element-buffer-allocation :accessor element-buffer-allocation :initform nil)
   (vao :accessor vao :initarg :vao :initform 0)))

(defclass render-bundle ()
  ((geometry :reader geometry :initarg :geometry)
   (gl-state)))

(defmethod initialize-instance :after ((obj render-bundle) &key)
  (let ((geometry (slot-value obj 'geometry)))
    (when geometry
      (ref geometry))))

(defmethod (setf geometry) (new-val (obj render-bundle))
  (when (slot-boundp obj 'geometry)
    (let ((old-val (slot-value obj 'geometry)))
      (when old-val
        (unref old-val))))
  (when new-val
    (ref new-val))
  (setf (slot-value obj 'geometry) new-val)
  new-val)

(defmethod dereferenced :after ((obj geometry))
  (with-slots ((array-alloc array-buffer-allocation)
               (element-alloc element-buffer-allocation))
      obj
    (when array-alloc
      (deallocate-in-buffer (allocation-buffer array-alloc) array-alloc)
      (setf array-alloc nil))
    (when element-alloc
      (deallocate-in-buffer (allocation-buffer element-alloc)
                            element-alloc)
      (setf element-alloc nil))))

(defgeneric loadedp (obj))

(defmethod loadedp ((obj geometry))
  (slot-boundp obj 'array-buffer))

(defgeneric upload-buffers (renderer obj))

(defgeneric allocate-buffer-storage (renderer size target usage))

(defclass renderer ()
  ((buffers :accessor buffers :initform nil)
   (bundles :accessor bundles :initform nil)
   (new-bundles :accessor new-bundles :initform nil)))

(defmethod allocate-buffer-storage ((renderer renderer) size target usage)
  (loop
     for buffer in (buffers renderer)
     if (eql (usage buffer) usage)
     do (let ((alloc (allocate-from-buffer buffer size)))
          (when alloc
            (return-from allocate-buffer-storage alloc))))
  (let ((new-buf (reserve-buffer target usage)))
    (push new-buf (buffers renderer))
    (allocate-from-buffer new-buf size)))


(defmethod upload-buffers (renderer (obj geometry))
  (let* ((data-size (gl-array-byte-size (vertex-data obj)))
         (data-alloc (allocate-buffer-storage renderer
                                              data-size
                                              :array-buffer
                                              (vertex-usage obj)))
         (data-buffer (allocation-buffer data-alloc)))
    (gl:bind-buffer :array-buffer (id data-buffer))
    (gl:buffer-sub-data :array-buffer (vertex-data obj)
                        :buffer-offset (allocation-offset data-alloc)
                        :size data-size)
    (setf (array-buffer obj) data-buffer)
    (setf (array-buffer-allocation obj) data-alloc)
    (when (indices obj)
      (let* ((index-size (gl-array-byte-size (indices obj)))
             (index-alloc (allocate-buffer-storage renderer
                                                   index-size
                                                   :element-array-buffer
                                                   (index-usage obj)))
            (index-buffer (allocation-buffer index-alloc)))
        (setf (element-buffer obj) index-buffer)
        (setf (element-buffer-allocation obj) index-alloc)
        (gl:bind-buffer :element-array-buffer (id index-buffer))
        (gl:buffer-sub-data :element-array-buffer (indices obj)
                            :buffer-offset (allocation-offset index-alloc)
                            :size index-size)))))

(defun add-bundle (renderer bundle)
  (push bundle (new-bundles renderer)))

(defun update-bundle (renderer bundle))

(defun remove-bundle (renderer bundle)
  (setf (new-bundles renderer) (delete bundle (new-bundles renderer)))
  (setf (bundles renderer) (delete bundle (bundles renderer)))
  (setf (geometry bundle) nil)          ; dereferences geometry
  bundle)

(defgeneric upload-bundles (renderer))

(defgeneric draw (renderer))

(defun draw-render-groups (renderer)
  (upload-bundles renderer)
  (draw renderer))

(defmethod draw ((renderer renderer))
  (loop
     for bundle in (bundles renderer)
     for geom = (geometry bundle)
     do (progn
          (gl:bind-vertex-array (vao geom))
          (if (indices geom)
              (let ((index-offset (allocation-offset
                                   (element-buffer-allocation geom))))
                (%gl:draw-elements (mode geom)
                                   (number-vertices geom)
                                   (gl::cffi-type-to-gl
                                    (gl::gl-array-type (indices geom)))
                                   (cffi:inc-pointer (cffi:null-pointer)
                                                     index-offset)))
              (gl:draw-arrays (mode geom) 0 (number-vertices geom))))))

(defgeneric upload-geometry (renderer geometry))

(defmethod upload-geometry ((renderer renderer) geometry)
  (let ((nullptr (cffi:null-pointer))
        (vao (gl:gen-vertex-array))
        (binder (gl::find-vertex-array-binder (vertex-attributes geometry))))
    (gl:bind-vertex-array vao)
    ;; This call binds buffers
    (upload-buffers renderer geometry)
    (let ((array-offset
           (allocation-offset (array-buffer-allocation geometry))))
      (funcall binder (cffi:inc-pointer nullptr array-offset)))
    (gl:bind-vertex-array 0)
    (setf (vao geometry) vao)))

(defmethod upload-bundles ((renderer renderer))
  (loop
     for bundle in (new-bundles renderer)
     if (not (loadedp (geometry bundle)))
     do (upload-geometry renderer (geometry bundle)))
  (setf (bundles renderer) (nconc (new-bundles renderer) (bundles renderer)))
  (setf (new-bundles renderer) nil))

(defgeneric close-renderer (renderer))

(defmethod close-renderer ((renderer renderer))
  (loop
     for bundle in (bundles renderer)
       do (setf (geometry bundle) nil)))
