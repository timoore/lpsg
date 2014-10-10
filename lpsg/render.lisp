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

(defclass renderer (assembly)
  ((buffers :accessor buffers :initform nil)
   (bundles :accessor bundles :initform nil)
   (new-bundles :accessor new-bundles :initform nil)
   (current-state :accessor current-state :initform nil)
   (finalize-queue :accessor finalize-queue :initform nil)
   (upload-queue :accessor upload-queue :initform nil)))

(defmethod add-object :after ((parent renderer) object)
  (labels ((do-all-children (obj)
             (unless (gl-finalized-p obj)
               (push obj (finalize-queue renderer)))
             (when (typep obj 'assembly)
               (mapc #'do-all-children (children object)))))
    (do-all-children object)))

(defun process-finalize-queue (renderer)
  (loop
     for obj in (finalize-queue renderer)
     if (not (gl-finalized-p obj))
     do (gl-finalize obj))
  (setf (finalize-queue renderer) nil))

(defclass gl-object ()
  ((id :accessor id :initarg :id :initform 0
       :documentation "ID from OpenGL of object"))
  (:documentation "Abstract class for objects allocated in OpenGL."))

(defgeneric gl-valid-p (obj))

(defmethod gl-valid-p ((obj gl-object))
  (not (zerop (id obj))))

(defclass allocator ()
  ((free-list :accessor free-list :initform nil)))

(defmethod initialize-instance :after ((obj allocator) &key size)
  (when size
    (push (list 0 size) (free-list obj))))

(defgeneric allocate (allocator size &optional alignment))

(defun round-up (val divisor)
  (* (ceiling val divisor) divisor))

(defmethod allocate ((allocator allocator) size &optional (alignment 4))
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

(defclass gl-buffer (allocator gl-object)
  ((size :accessor size :initarg :size)
   (usage :accessor usage :initarg :usage)))

(defun reserve-buffer (target &optional (usage :static-draw) (size 104856))
  (let* ((ids (gl:gen-buffers 1))
         (id (car ids))
         (buf (make-instance 'gl-buffer :id id :size size :usage usage)))
    (gl:bind-buffer target id)
    (%gl:buffer-data target size (cffi:null-pointer) usage)
    buf))

(defun allocation-offset (alloc)
  (car alloc))

(defun allocation-size (alloc)
  (cadr alloc))

(defun allocation-buffer (alloc)
  (caddr alloc))

(defun allocate-from-buffer (buffer size &optional (alignment 4))
  (let ((rounded-size (* (ceiling size alignment) alignment)))
    (multiple-value-bind (offset rounded-size)
        (allocate buffer size alignment)
      (list offset rounded-size buffer))
    nil))

(defun deallocate-in-buffer (buffer allocation)
  (push allocation (free-list buffer)))

(defun release-buffer (buffer)
  (gl:delete-buffers (list (id buffer)))
  (setf (id buffer) 0)
  nil)

(defclass drawable ()
  ((mode :accessor mode :initarg :mode
         :documentation "A mode for an OpenGL draw-elements or draw-array call,
  e.g. :triangles")
   (number-vertices :accessor number-vertices :initarg :number-vertices
                    :documentation "The total number of vertices in this geometry.")))

(defclass geometry (drawable)
  ((indices :accessor indices :initarg :indices :initform nil
            :documentation "A gl-array (:unsigned-short) of indices into the vertex
   attributes, for each vertex of each geometry element. This can be NULL, in
   which case the geometry will be drawn using %gl:draw-array.")
   (index-usage :accessor index-usage :initarg :index-usage
                :initform :static-draw
                :documentation "A hint for allocation of the index buffer
   storage")
   (vertex-attributes :accessor vertex-attributes :initarg :vertex-attributes
                      :documentation "A symbol (cl-opengl gl-array-format) or a
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
  ((geometry :accessor geometry :initarg :geometry)
   (gl-state :reader gl-state :initarg :gl-state)))

(defclass graphics-state ()
  ((bindings)
   (program :accessor program :initarg :program :initform nil)
   (uniform-sets :accessor uniform-sets :initarg :uniform-sets :initform nil)))

(defgeneric gl-finalize (obj &optional errorp)
  (:documentation "Allocate any OpenGL resources needed for OBJ and perform any
  tasks needed to use it (e.g. link a shader program)"))

(defgeneric gl-finalized-p (obj))

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

(defclass shader (shader-source gl-object)
  ((status :accessor status :initarg :status)
   (compiler-log :accessor compiler-log :initarg :compiler-log :initform nil)))

(defmethod gl-finalized-p ((obj shader))
  (slot-boundp obj 'status))

(defmethod gl-finalize ((obj shader) &optional (errorp t))
  (if (gl-finalized-p obj)
      (status obj)
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
                     :format-control "The shader ~S has compile errors.")))
          status))))

(defclass program (gl-object)
  ((shaders :accessor shaders :initarg :shaders :initform nil)
   (compiled-shaders :accessor compiled-shaders :initform nil)
   ;; (name location type size)
   (uniforms :accessor uniforms :initform nil
             :documentation "Information on uniforms declared within
  the program shader source.") 
   (status :accessor status :initarg :status)
   (link-log :accessor link-log :initarg :link-log :initform nil)
   ;; elements are (desc strategy (most-recent-uset counter))
   (uset-alist :accessor uset-alist :initform nil)))

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
         (strategy (cadr uset-entry))
         (last-uset-data (caddr uset-entry)))
    (when (and strategy
               (not (up-to-date-p uset (car last-uset-data) (cdr last-uset-data))))
      (funcall (uploader strategy) uset)
      (setf (car last-uset-data) uset
            (cdr last-uset-data) (counter uset)))
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
      (loop
         for (nil strategy) in (uset-alist obj)
         do (initialize-uset-strategy strategy obj))
      obj)))

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
    (let* ((old-program (and current-state (program current-state)))
           (new-program (program state)))
      (unless (eq new-program old-program)
        (gl:use-program (id new-program)))
        ;; XXX bindings
      (loop
         for uset in (uniform-sets state)
         do (upload-uset-to-program uset new-program))
      (setf (current-state renderer) state))))

(defun draw-render-groups (renderer)
  (upload-bundles renderer)
  (draw renderer))

(defmethod draw ((renderer renderer))
  ;; XXX Should we set the state to something known here?
  (setf (current-state renderer) nil)
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
