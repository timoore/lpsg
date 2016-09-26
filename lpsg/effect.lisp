;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;; An effect represents the rendered appearance of a shape. It is responsable for creating bundles
;;; and their environments and putting them in the appropriate render queues. The effect object
;;; contains graphics environments. The SIMPLE-EFFECT class only has one environment, but other
;;; effects might have different environments for different passes.

(in-package #:lpsg)

(defclass effect ()
  ((attribute-map :accessor attribute-map :initform nil :initarg :attribute-map
                  :documentation "list of (symbol glsl-name) where glsl-name is a string")
   (visiblep :input-accessor visiblep :initarg :visiblep :initform t)
   ;; This slot exists to enable the invalidation of the node when an input (usually in a subclass)
   ;; changes.
   (status :compute-function status :initform t))
  (:metaclass compute-class)
  (:documentation "Class that represents the rendered appearance of a shape.

 EFFECT is responsable for creating bundles and their environments and putting them in the
  appropriate render queues. The effect object contains graphics environments. The SIMPLE-EFFECT
  class only has one environment, but other effects might have different environments for different
  passes."))

(defmethod status ((e effect))
  t)

(defun init-attr-set-from-shape (attrib-set shape attr-map)
  " Initialize attribute set from shape and drawable attributes. The actual vertex
attribute index for an attribute may not be known until the shader
program is linked, so make it invalid for now and let gl-finalize sort
it out. If an attribute is not supported by the environment, no problem;
just ignore it."
  (setf (array-bindings attrib-set)
        (mapcan (lambda (entry)
                  (let ((gl-name (cdr (assoc (car entry) attr-map))))
                    (if gl-name
                        (list (list gl-name (cdr entry) -1))
                        nil)))
                (attributes shape)))
  (when (typep (drawable shape) 'indexed-drawable)
    (setf (element-binding attrib-set) (element-array (drawable shape))))
  attrib-set)

(defclass simple-effect (effect)
  ((gl-state :accessor gl-state :initarg :gl-state :documentation "@c(graphics-state) object used
  to render shapes that use this effect")
   (environment :accessor environment :initarg :environment
                :documentation "@c{environment} object used to implement the effect."))
  (:metaclass compute-class)
  (:documentation "This class supports effects which are simply the application of OpenGL state,
with uset parameters, to a shape."))

(defgeneric simple-effect-usets (effect)
  (:documentation "Create the list of usets used by the effect's environment.")
  (:method-combination nconc))

(defmethod simple-effect-usets :around ((effect simple-effect))
  (if (next-method-p)
      (call-next-method)
      nil))

(defclass shape-attribute-set (attribute-set)
  ())

(defmethod initialize-instance :after ((obj shape-attribute-set) &key shape attribute-map)
  (when (and shape attribute-map)
    (init-attr-set-from-shape obj shape attribute-map)))

(defmethod submit-with-effect (shape renderer (effect simple-effect) &key (extra-usets nil))
  (let* ((env (make-instance 'environment
                             :attribute-map (attribute-map effect)
                             :gl-state (gl-state effect)
                             :uniform-sets (append (simple-effect-usets effect) extra-usets)))
         (attr-map (attribute-map env)))
    (let* ((attrib-set (make-instance 'shape-attribute-set :shape shape :attribute-map attr-map))
           (bundle (make-instance 'render-bundle
                                  :attribute-set attrib-set :shape shape :environment env)))
      (setf (environment effect) env)
      (push bundle (bundles shape))
      (push bundle (finalize-queue renderer))
      ;; Use only one render-stage / render-queue for now.
      (let ((rq (default-render-queue renderer)))
        (unless rq
          (setq rq (make-instance 'unordered-render-queue))
          (add-rendered-object (render-stage renderer) rq))
        (add-rendered-object rq bundle)))))

(defmethod retract-with-effect (shape renderer (effect simple-effect))
  (let ((rq (find-if-queue #'render-queue-p)))
    (when rq
      (loop
         for bundle in (bundles shape)
         do (remove-rendered-object rq bundle)))))

(defmethod update-effect progn ((effect simple-effect))
  (setf (visiblep (environment effect)) (visiblep effect)))


(defmethod invalidate ((node effect))
  (push (lambda ()
          (update-effect node)          ; Subclass updates environments, usets, etc.
          (status node))                ; Reset this dummy computed slot.
        *deferred-updates*))

