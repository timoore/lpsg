;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Copyright (c) 2012, 2013, Tim Moore (moore@bricoworks.com)
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

(defun report-render-error (condition stream)
  (let ((args (format-arguments condition))
        (obj (gl-object condition)))
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

(defclass renderer ()
  ((buffers :accessor buffers :initform nil)
   (bundles :accessor bundles :initform nil)
   (new-bundles :accessor new-bundles :initform nil)
   (current-state :accessor current-state :initform nil)))

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

(defclass gl-object ()
  ((id :accessor id :initarg :id :initform 0
       :documentation "ID from OpenGL of object"))
  (:documentation "Abstract class for objects allocated in OpenGL."))

(defgeneric gl-valid-p (obj))

(defmethod gl-valid-p ((obj gl-object))
  (not (zerop (id obj))))

(defclass gl-buffer (gl-object)
  ((size :accessor size :initarg :size)
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

(defun round-up (val divisor)
  (* (ceiling val divisor) divisor))

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
  ((mode :accessor mode :initarg :mode
         :documentation "A mode for an OpenGL draw-elements or draw-array call,
  e.g. :triangles")
   (number-vertices :accessor number-vertices :initarg :number-vertices
                    :documentation "The total number of vertices in this geometry.")
   (indices :accessor indices :initarg :indices :initform nil
            : "A gl-array (:unsigned-short) of indices into the vertex
   attributes, for each vertex of each geometry element. This can be NULL, in
   which case the geometry will be drawn using %gl:draw-elements.")
   (index-usage :accessor index-usage :initarg :index-usage
                :initform :static-draw
                :documentation "A hint for allocation of the index buffer
   storage")
   (vertex-attributes :accessor vertex-attributes :initarg :vertex-attributes
                      :documentation "A symbol (cl-opengl gl-array-format or a
  list of (attrib-number type size")
   (vertex-data :accessor vertex-data :initarg :vertex-data
                :documentation "a single gl-array, or a list of gl-array
  objects corresponding to the vertex attributes")
   (vertex-usage :accessor vertex-usage :initarg :vertex-usage
                 :initform :static-draw
                 :documentation "A hint for allocation of the vertex data buffer
   storage")
   (array-buffer :accessor array-buffer)
   (array-buffer-allocation :accessor array-buffer-allocation :initform nil)
   (element-buffer :accessor element-buffer)
   (element-buffer-allocation :accessor element-buffer-allocation :initform nil)
   (vao :accessor vao :initarg :vao :initform 0
        :documentation "OpenGL object for binding vertex attributes for
   rendering.")))

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

(defclass graphics-state ()
  ((bindings)
   (program :accessor program :initform :program)
   (uniform-sets :accessor uniform-sets :initform :uniform-sets)))

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
(defclass uniform-declaration ()
  ((name :accessor name :initarg :name)
   (full-name :accessor full-name)
   (gl-type :accessor gl-type :initarg :gl-type)
   (accessor :accessor accessor :initarg accessor)
   (local-offset :accessor local-offset :initarg :local-offset
                 :documentation "offset of uniform in local storage")
   (local-storage-setter :accessor local-storage-setter)))

;;; The uniform set descriptor. The strategy object controls how and when the
;;; uniform values will be set in OpenGL.

(defclass uset-descriptor ()
  ((uniforms :accessor uniforms)
   (local-storage-size :accessor local-storage-size :initform 0)
   (strategy :accessor strategy)))

;;; Strategies will be defined later. A strategy may need per-uset data, which
;;; will be a subclass of uset-strategy-data.

(defclass uset-strategy-data ()
  ((strategy :accessor strategy)))

;;; Uset variable values are stored locally, so they can be set by the
;;; application and then later uploaded to shader programs during
;;; rendering. They are stored in foreign memory in order to optimize the
;;; upload process by minimizing conversions and further foreign memory
;;; allocation. For simplicity we use one local storage layout, which is
;;; similar to the native C layout. It might be more optimal to choose the
;;; local layout based on the strategy, for example, use std140 with uniform
;;; blocks, but that complicates the implementation a lot and makes it
;;; harder to change strategies.
;;; 
(defclass uset ()
  ((descriptor :accessor descriptor :initarg :descriptor)
   (local-storage :accessor local-storage)
   (strategy-data :accessor strategy-data)))


(defun has-valid-strategy-p (uset)
  (with-slots (descriptor strategy-data)
      uset
    (and descriptor
         strategy-data
         (eq (strategy descriptor) (strategy strategy-data)))))


;;; Different memory layouts are needed, depending on the uniform set
;;; strategy. For uniforms in the default block, we need a C-like layout. If we
;;; are going to use uniform buffers, then we need to use a layout like
;;; std140 in the memory backing the uniform buffer object.

(defparameter *uniform-type-info* (make-hash-table :test 'eq))

(defclass uniform-type-info ()
  ((name :accessor name :initarg :name)
   (glsl-name :accessor glsl-name :initarg :glsl-name)
   (size :accessor size :initarg :size)
   (c-alignment :accessor c-alignment :initarg :c-alignment)
   (std140-alignment :accessor std140-alignment :initarg :std140-alignment)
   (stride :accessor stride :initarg :stride)
   (local-writer :accessor local-writer :initarg :local-writer)
   (uploader :accessor uploader :initarg :uploader)))

(defmacro define-uniform-type (sym glsl-name size c-alignment gl-alignment
                               stride)
  (let ((writer-sym (intern (concatenate 'simple-string
                                         (symbol-name '#:write-uniform-local-)
                                         (symbol-name sym))))
        (uploader-sym (intern (concatenate 'simple-string
                                           (symbol-name '#:upload-uniform-)
                                           (symbol-name sym)))))
    `(setf (gethash ',sym *uniform-type-info*)
           (make-instance 'uniform-type-info
                          :name ,sym :glsl-name ,glsl-name :size ,size
                          :c-alignment ,c-alignment
                          :std140-alignment ,gl-alignment
                          :stride ,stride
                          :local-writer #',writer-sym
                          :uploader #',uploader-sym))))

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
     for i from 0 below 4
     do (loop
             for j from 0 below 4
             do (setf (cffi:mem-aref ptr '%gl:float (+ (* i 4) j))
                      (aref val i j)))))
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

(defclass default-uniform-strategy (strategy)
  ((per-program-locations :accessor per-program-locations
                          :initarg :per-program-locations
                          :initform nil)))

(defclass uniform-definition ()
  ((decl :accessor decl)
   (location :accessor location)
   (offset :accessor offset)))

(defgeneric gl-finalize (obj &optional errorp)
  :documentation "Allocate any OpenGL resources needed for OBJ and perform any
  tasks needed to use it (e.g. link a shader program)")

(defgeneric gl-finalized-p (obj))

(defclass shader (gl-object)
  ((shader-type :accessor shader-type :initarg :shader-type)
   (source :accessor source :initarg :source :initform nil)
   (declared-usets :accessor declared-usets :initarg :declared-usets
                   :initform nil)
   (status :accessor status :initarg :status)
   (compiler-log :accessor compiler-log :initarg :compiler-log :initform nil)))

(defmethod gl-finalized-p ((obj shader))
  (slot-boundp obj 'status))

(defmethod gl-finalize ((obj shader) &optional (errorp t))
  (if (gl-finalized-p obj)
      (status obj)
      (let ((id (gl:create-shader (shader-type obj))))
        (setf (id obj) id)
        (gl:shader-source id (source obj))
        (gl:compile-shader id)
        (let ((status (gl:get-shader id :compile-status)))
          (setf (status obj) status)
          (unless status
            (setf (compiler-log obj) (gl:get-shader-info-log id))
            (when errorp
              (error 'render-error :gl-object obj :error-log (compiler-log obj)
                     :format-control "The shader ~S has compile errors.")))
          status))))

(defclass program (gl-object)
  ((shaders :accessor shaders :initarg :shaders :initform nil)
   ;; (name location type size)
   (uniforms :accessor uniforms :initform nil
             :documentation "Information on uniforms declared within
  the program shader source.") 
   (uset-descriptors :accessor uset-descriptors :initform nil)
   (status :accessor status :initarg :status)
   (link-log :accessor link-log :initarg :link-log :initform nil)
   (current-usets :accessor current-usets :initform nil)))

;;; Set the uniform values in a program, assuming  that it is currently bound.
(defun upload-uset-to-program (uset program)
  (let ((descriptor (descriptor uset)))
    ))

(defmethod gl-finalized-p ((obj program))
  (slot-boundp obj 'status))

(defmethod gl-finalize ((obj program) &optional (errorp t))
  (flet ((err (&rest args)
           (if errorp
               (apply #'error args)
               (return-from gl-finalize nil))))
    (let ((id (gl:create-program)))
    (setf (id obj) id)
    (with-slots (shaders)
        obj
      (loop
         for shader in shaders
         when (null (gl-finalize shader))
         do (err 'render-error
                 :gl-object shader
                 :format-control "The shader ~S is not finalized."))
      (loop
         for shader in shaders
         do (gl:attach-shader id (id shader))))
    (gl:link-program id)
    (unless (setf (status obj) (gl:get-program id :link-status))
      (setf (link-log obj) (gl:get-program-info-log id))
      (err 'render-error
           :gl-object obj :error-log (link-log obj)
           :format-control "The program ~S has link errors."))
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
    obj)))

(defvar *uset-descriptors* (make-hash-table :test 'eq))

(defun define-uset (name variables &key (strategy :default) usage)
  (declare (ignore usage))
  (let ((descriptor (make-instance 'uset-descriptor :strategy strategy))
        (decls (mapcar #'(lambda (clause)
                           (destructuring-bind (name gl-type
                                                     &optional accessor)
                               clause
                             (make-instance 'uniform-declaration
                                            :name name
                                            :gl-type gl-type)))
                       variables)))
    ;; lay out local storage
    (loop
       with offset = 0
       for var in decls
       for typeinfo = (gethash (gl-type var) *uniform-type-info*)
       do (if typeinfo
              (with-slots (size c-alignment local-writer)
                  typeinfo
                ;; XXX Do something with size when arrays are supported
                (setq offset (round-up offset c-alignment))
                (setf (local-offset var) offset)
                (let ((raw-setter-fun local-writer)
                      (offset offset))  ;rebind to close over
                  (setf (local-storage-setter var)
                        #'(lambda (storage val)
                            (cffi:with-pointer-to-vector-data (ptr storage)
                              (funcall raw-setter-fun
                                       (cffi:inc-pointer ptr offset)
                                       val)))))
                (incf offset size)))
       finally (let ((local-size (round-up offset 8))) ;double alignment
                 (setf (local-storage-size descriptor) local-size)))
    (setf (gethash name *uset-descriptors*) descriptor)
    descriptor))

(defun ensure-uset-descriptor (descriptor)
  (let* ((name (if (consp descriptor)
                   (car descriptor)
                   descriptor))
         (desc (gethash name *uset-descriptors*)))
    (cond (desc
           (return-from ensure-uset-descriptor desc))
          ((not (consp descriptor))
           (error 'render-error
                  :format-control "~S is not a descriptor definition."
                  :format-arguments (list desc)))
          (t (let ((clauses (cadr descriptor)))
               (setq desc
                     (loop
                        for clause in desc
                        collect (if (consp clause)
                                    (make-instance 'uniform-declaration
                                                   :name (car clause)
                                                   :gl-type (cadr clause))
                                    clause))))))
    (setf (gethash name *uset-descriptors*) desc)
    desc))

(defun ensure-descriptors (descriptors)
  (mapcar #'ensure-descriptor descriptors))

(defun make-shader (stage uset-descriptors source)
  (let ((descriptors (ensure-descriptors uset-descriptors))
        )))

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

(defgeneric bind-state (renderer state))



(defmethod bind-state ((renderer renderer) (state graphics-state))
  (with-slots (current-state)
      renderer
    (when (eq current-state state)
      (return-from bind-state nil))
    (let* ((old-program (program current-state))
           (new-program (program state))
           (prog-descriptors (uset-descriptors new-program)))
      (unless (eq new-program old-program)
        (gl:use-program (id new-program)))
      (loop
         for set in (uniform-sets state))
    
      )))

(defun draw-render-groups (renderer)
  (upload-bundles renderer)
  (draw renderer))

(defmethod draw ((renderer renderer))
  (loop
     for bundle in (bundles renderer)
     for geom = (geometry bundle)
     do (progn
          (bind-state renderer (gl-state bundle))
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
