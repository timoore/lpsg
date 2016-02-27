;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

(defclass texture-area ()
  ((level :accessor level :initarg :level)
   (texture :accessor texture :initarg :texture)
   (x-offset :accessor x-offset :initarg :x-offset
             :documentation "X offset of data in texture image.")
   (y-offset :accessor y-offset :initarg :y-offset
             :documentation "Y offset of data in texture image.")
   (width :accessor width :initarg :width)
   (height :accessor height :initarg :height)
   (generate-mipmap-p :accessor generate-mipmap-p :initarg :generate-mipmap-p))
  (:default-initargs :level 0 :generate-mipmap t))

(define-gl-object texture ()
  ((target :accessor target :initarg :target)
   (internal-format :accessor internal-format :initarg :internal-format)
   (pixel-format :accessor pixel-format :initarg :pixel-format)
   (data-type :accessor data-type :initarg :data-type)
   (base-level :accessor base-level :initarg :base-level))
  (:default-initargs :base-level 0))

(defclass texture-2d (texture)
  ((width :accessor width :initarg :width)
   (height :accessor height :initarg :height)))

(defmethod gl-finalize ((obj texture-2d) &optional errorp)
  (setf (id obj) (gl:gen-texture))
  (gl:active-texture 0)
  (let ((target (target obj)))
    (gl:bind-texture (target obj) (id obj))
    (gl:tex-image-2d (target obj)
                     0                  ;XXX level?
                     (internal-format obj)
                     (width obj)
                     (height obj)
                     0                  ; border must be 0
                     (pixel-format obj)
                     (data-type obj)
                     (cffi:null-pointer))
    (gl:tex-parameter target :texture-base-level (base-level obj))
    (gl:bind-texture (target obj) 0)))

(defclass raw-texture-resource (texture-area)
  ((data :accessor data :initarg :data)
   (data-offset :accessor data-offset :initarg :data-offset :initform 0)
   (data-count :accessor data-count :initarg :data-count :initform 0
               :documentation "number of elements")
   (row-alignment :accessor row-alignment :initarg :row-alignment))
  (:default-initargs :row-alignment 1)
  (:documentation "Class for texture data stored in foreign memory."))

;;; Texture upload queue stuff

;;; TODO: Arrange queue by texture object. Push areas onto the end of the queue.
(defmethod add-to-upload-queue ((queue texture-upload-queue) (obj texture-area))
  (push obj (tex-queue queue)))

(defgeneric upload-texture (renderer texture-area texture))

(defmethod process-upload-queue :after (renderer (queue texture-upload-queue))
  (loop
     for area in (tex-queue queue)
     for texture = (texture area)
     do (upload-texture renderer area texture)
     finally (setf (tex-queue queue) nil)))

(defmethod upload-texture (renderer (area raw-texture-resource) (texture texture-2d))
  (gl:active-texture 0)
  (let ((target (target texture)))
    (gl:bind-texture target (id texture))
    (gl:pixel-store :unpack-alignment (row-alignment area))
    (gl:tex-sub-image-2d target
                         (level area)
                         (x-offset area)
                         (y-offset area)
                         (width area)
                         (height area)
                         (pixel-format texture)
                         (data-type texture)
                         (cffi:inc-pointer (data area) (data-offset area)))
    (when (generate-mipmap-p area)
      (gl:generate-mipmap target))
    (gl:bind-texture target 0)))

(define-gl-object sampler ()
  ((wrap-s :accessor wrap-s :initarg :wrap-s)
   (wrap-t :accessor wrap-t :initarg :wrap-t)
   (wrap-r :accessor wrap-r :initarg :wrap-r)
   (border-color :accessor border-color :initarg :border-color)
   (min-filter :accessor min-filter :initarg :min-filter)
   (mag-filter :accessor mag-filter :initarg :mag-filter)
   (min-lod :accessor min-lod :initarg :min-lod)
   (max-lod :accessor max-lod :initarg :max-lod)
   (lod-bias :accessor lod-bias :initarg :lod-bias)
   (compare-mode :accessor compare-mode :initarg :compare-mode)
   (compare-func :accessor compare-func :initarg :compare-func))
  (:default-initargs :wrap-s :repeat :wrap-t :repeat :wrap-r :repeat
                     :texture-border-color #(0.0 0.0 0.0 0.0) :min-filter :nearest-mipmap-linear
                     :mag-filter :linear :min-lod -1000.0 :max-lod 1000.0 :lod-bias 0.0
                     :compare-mode :none :compare-func :lequal))

(defmethod gl-finalize ((obj sampler) &optional errorp)
  (let ((id (gl:gen-sampler)))
    (setf (id obj) id)
    (gl:sampler-parameter id :texture-wrap-s (wrap-s obj))
    (gl:sampler-parameter id :texture-wrap-t (wrap-t obj))
    (gl:sampler-parameter id :texture-wrap-r (wrap-r obj))
    (gl:sampler-parameter id :texture-border-color (border-color obj))
    (gl:sampler-parameter id :texture-min-filter (min-filter obj))
    (gl:sampler-parameter id :texture-mag-filter (mag-filter obj))
    (gl:sampler-parameter id :texture-min-lod (min-lod obj))
    (gl:sampler-parameter id :texture-max-lod (max-lod obj))
    (gl:sampler-parameter id :texture-lod-bias (lod-bias obj))
    (gl:sampler-parameter id :texture-compare-mode (compare-mode obj))
    (gl:sampler-parameter id :texture-compare-func (compare-func obj)))
  )

(defclass graphics-state ()
  ((texture-bindings)
   (program :accessor program :initarg :program :initform nil))
  (:documentation "Class that stores all OpenGL state."))

(defmethod gl-finalized-p ((obj graphics-state))
  (gl-finalized-p (program obj)))

(defmethod gl-finalize ((obj graphics-state) &optional errorp)
  (gl-finalize (program obj) errorp))

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

