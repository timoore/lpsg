;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

;;; Classes for the nodes in an incremental computation graph

;;; A computation graph represents computations that are evaluated in a lazy manner. The inputs to
;;; the computation can change and invalidate the whole computation, but the calculation won't be
;;; evaluated until its results are needed. In LPSG, the usets that are used by environments are
;;; kept up-to-date using computation graphs.

;;; 3 concepts:
;;; * computed slots - generic function of one argument with cached value
;;; * nodes subscribe to events signalled by other nodes - invalidate cached values
;;; * connections - specify how to get a value from another node
;;; ** setf-able accessor function + slot name(?)
;;; ** node is invalidated when upstream nodes are
;;; also:
;;; connected nodes and connections are stored in slots, but also possible to store in lists
;;; first-class objects, so connections can be copied to other objects or forwarded.

;;; MOP implementation

(defvar *deferred-updates* nil "List of functions that will perform some action on nodes.")

(defun do-deferred-updates ()
  (mapc #'funcall *deferred-updates*)
  (setq *deferred-updates* nil))

;;; This uses both the design patterns found on http://www.cliki.net/MOP%20design%20patterns !
(defclass compute-class (closer-mop:standard-class)
  ((computed-slots :accessor computed-slots :initform nil)))

(defclass compute-object ()
  ((receivers :accessor receivers :initform nil) 
   (connections :accessor connections :initform nil)))

(defun %input-source (cell)
  (car cell))

(defun %input-access-function (cell)
  (cdr cell))

;;; From Pascal Constanza

(defmethod initialize-instance :around ((class compute-class) &rest initargs
   &key direct-superclasses)
  (declare (dynamic-extent initargs))
  (if (loop for class in direct-superclasses
            thereis (subtypep class (find-class 'compute-object)))
     ;; 'compute-object is already one of the (indirect) superclasses
     (call-next-method)
     ;; 'compute-object is not one of the superclasses, so we have to add it
     (apply #'call-next-method
            class
            :direct-superclasses
            (append direct-superclasses
                    (list (find-class 'compute-object)))
            initargs)))

(defmethod reinitialize-instance :around ((class compute-class) &rest initargs
                                          &key (direct-superclasses '() direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (if direct-superclasses-p
    ;; if direct superclasses are explicitly passed
    ;; this is exactly like above
    (if (loop for class in direct-superclasses
              thereis (subtypep class (find-class 'compute-object)))
       (call-next-method)
       (apply #'call-next-method
              class
              :direct-superclasses
              (append direct-superclasses
                      (list (find-class 'compute-object)))
              initargs))
    ;; if direct superclasses are not explicitly passed
    ;; we _must_ not change anything
    (call-next-method)))
;;;

(defmethod closer-mop:validate-superclass ((class compute-class) (superclass standard-class))
  t)

(defclass compute-slot-definition (closer-mop:standard-slot-definition)
  ((compute-function :accessor compute-function :initarg :compute-function :initform nil)))

(defclass compute-slot-direct-definition (compute-slot-definition
                                          closer-mop:standard-direct-slot-definition)
  ((input-readers :accessor input-readers ::initarg :input-readers :initform nil)
   (input-writers :accessor input-writers :initarg :input-writers :initform nil)))

(defclass compute-slot-effective-definition (compute-slot-definition
                                             closer-mop:standard-effective-slot-definition)
  ((input-slot-p :accessor input-slot-p :initform nil)))

(defmethod initialize-instance :around ((obj compute-slot-direct-definition)
                                        &rest initargs
                                        &key input-reader input-writer input-accessor
                                          readers writers)
  (let ((is-input-slot-p (or input-reader input-writer input-accessor)))
    (when (and is-input-slot-p (or readers writers))
      (error "slot ~S cannot have both input and regular accessors."
             (closer-mop:slot-definition-name obj)))
    (if is-input-slot-p
        (flet ((listify (arg)
                 (if (and (listp arg) (not (eq (car arg)  'setf)))
                     arg
                     (list arg))))
          (let ((input-readers (listify input-reader))
                (input-writers (listify input-writer))
                (input-accessors (listify input-accessor)))
            (when input-accessors
              (setf input-readers (append input-accessors input-readers))
              (setf input-writers (append (mapcar (lambda (x)
                                                    `(setf ,x))
                                                  input-accessors)
                                          input-writers)))
            (apply #'call-next-method obj
                   :input-readers input-readers :input-writers input-writers initargs)))
        (call-next-method))))

(defmethod closer-mop:direct-slot-definition-class ((class compute-class)
                                                    &key)
  (find-class 'compute-slot-direct-definition))

(defclass compute-function-method (closer-mop:standard-reader-method)
  ())

(defclass input-reader-method (closer-mop:standard-reader-method)
  ())

(defclass input-writer-method (closer-mop:standard-writer-method)
  ())

(defun make-compute-method (class slot-def)
  (let* ((slot-name (closer-mop:slot-definition-name slot-def))
         (compute-fn (compute-function slot-def))
         (gf (closer-mop:ensure-generic-function compute-fn)))
    (closer-mop:ensure-method gf
                              `(lambda (obj)
                                 (if (slot-boundp obj ',slot-name)
                                     (slot-value obj ',slot-name)
                                     (setf (slot-value obj ',slot-name) (call-next-method))))
                              :qualifiers '(:around)
                              :specializers (list class)
                              :method-class (find-class 'compute-function-method))))

(defun make-slot-accessors (class slot-def)
  (let ((slot-name (closer-mop:slot-definition-name slot-def)))
    (when (compute-function slot-def)
      (make-compute-method class slot-def))
    (loop
       for reader-name in (input-readers slot-def)
       for gf = (closer-mop:ensure-generic-function reader-name)
       with method-class = (find-class 'input-reader-method)
       do (closer-mop:ensure-method gf
                                    `(lambda (obj)
                                       (let ((cell (slot-value obj ',slot-name)))
                                         (funcall (cdr cell) (car cell))))
                                    :specializers (list class)
                                    :method-class method-class))
    (loop
       for writer-name in (input-writers slot-def)
       for gf = (closer-mop:ensure-generic-function writer-name)
       with method-class = (find-class 'input-writer-method)
       do (closer-mop:ensure-method gf
                                    `(lambda (newval obj)
                                       (let ((cell (slot-value obj ',slot-name)))
                                         ;; XXX disconnect old val
                                         (setf (car cell) newval)
                                         (setf (cdr cell) #'identity)
                                         (notify-invalid obj)
                                         newval))
                                    :specializers (list (find-class 't) class)
                                    :method-class method-class))))

;;; This code was in an after method on shared-initialize, but it seems that Clozure CL doesn't use
;;; shared-intialize on metaobjects...

(defmethod initialize-instance :after ((obj compute-class) &key)
  (mapc (lambda (slot-def) (make-slot-accessors obj slot-def))
        (closer-mop:class-direct-slots obj)))

(defmethod reinitialize-instance :after ((obj compute-class) &key)
  (mapc (lambda (slot-def) (make-slot-accessors obj slot-def))
        (closer-mop:class-direct-slots obj)))

(defmethod closer-mop:effective-slot-definition-class ((class compute-class) &key)
  (find-class 'compute-slot-effective-definition))

(defmethod closer-mop:compute-effective-slot-definition :around
    ((class compute-class) name direct-defs)
  (let ((def (call-next-method)))
    ;; Verify that input slot accessors, compute functions, and regular accessor functions a are
    ;; consistent.
    
    (when (typep def 'compute-slot-effective-definition) ; Always true?
      (let ((reader-tail (member-if #'closer-mop:slot-definition-readers direct-defs))
            (writer-tail (member-if #'closer-mop:slot-definition-writers direct-defs))
            (input-reader-tail (member-if (lambda (sd)
                                            (and (typep sd 'compute-slot-direct-definition)
                                                 (input-readers sd)))
                                          direct-defs))
            (input-writer-tail (member-if (lambda (sd)
                                            (and (typep sd 'compute-slot-direct-definition)
                                                 (input-writers sd)))
                                          direct-defs))
            (compute-tail (member-if (lambda (sd)
                                       (and (typep sd 'compute-slot-direct-definition)
                                            (compute-function sd)))
                                     direct-defs))
            (slot-name (closer-mop:slot-definition-name def)))
        (when (or (and input-reader-tail (< (length input-reader-tail) (length writer-tail)))
                  (and input-writer-tail (< (length input-writer-tail) (length reader-tail))))
          (error "Slot ~S has conflicting regular and input accessors." slot-name))
        ;; XXX and other error checks
        (when (or input-reader-tail input-reader-tail)
          (setf (input-slot-p def) t))
        (when compute-tail
          (setf (compute-function def) (compute-function (car compute-tail))))))
    def))

(defgeneric invalidate (node))

(defun notify-invalid (obj)
  (let ((must-invalidate (loop
                            for slot in (computed-slots (class-of obj))
                            thereis (slot-boundp obj slot))))
    (if must-invalidate
        (progn
          (invalidate obj)
          t)
        nil)))
  


(defmethod closer-mop:finalize-inheritance :after ((class compute-class))
  (setf (computed-slots class) (mapcan (lambda (slot)
                                         (if (and (typep slot 'compute-slot-effective-definition)
                                                  (compute-function slot))
                                             (list (closer-mop:slot-definition-name slot))
                                             nil))
                                       (closer-mop:class-slots class))))

(defmethod shared-initialize :around ((obj compute-object) slot-names &rest initargs &key)
  (let* ((metaclass (class-of obj))
         (slot-list (closer-mop:class-slots metaclass)))
    (flet ((input-slot-p (slot-def)
             (and (typep slot-def 'compute-slot-effective-definition)
                  (input-slot-p slot-def)))
           (input-plist-p (slot-def)
             #++
             (and (typep slot-def 'compute-slot-effective-definition)
                  (eq (input-slot slot-def) :plist))
             ;; XXX
             nil))
      (loop
         for slot in slot-list
         for slot-initargs = (closer-mop:slot-definition-initargs slot)
         when slot-initargs
         do
           (multiple-value-bind
                 (arg val tail)
               (get-properties initargs slot-initargs)
             (declare (ignore arg))
             (when tail
               (setf (slot-value obj (closer-mop:slot-definition-name slot))
                     (if (input-slot-p slot)
                         (cons val #'identity)
                         val)))))
      (loop
         for slot in slot-list
         for slot-name = (closer-mop:slot-definition-name slot)
         for slot-initfunc = (closer-mop:slot-definition-initfunction slot)
         when (and (or (eq slot-names t)
                       (member slot-name slot-names :test #'eq))
                   (not (slot-boundp obj slot-name)))
         do (cond ((input-slot-p slot)
                   (if  slot-initfunc
                        (setf (slot-value obj slot-name)
                              (cons (funcall slot-initfunc) #'identity))
                        (setf (slot-value obj slot-name) (cons nil nil))))
                  ((input-plist-p slot)
                   (setf (slot-value obj slot-name) nil))))
      (call-next-method obj slot-names))))


(defgeneric %connect (receiver local-slot transmitter accessor-fn &key))

(defmethod %connect ((receiver compute-object) local-slot (transmitter compute-object) accessor-fn
                     &key)
  (setf (car (slot-value receiver local-slot)) transmitter)
  (setf (cdr (slot-value receiver local-slot)) accessor-fn)
  (push receiver (receivers transmitter))
  (notify-invalid receiver)
  receiver)

(defgeneric connect (receiver local-slot transmitter accessor-fn &key))

(defmethod  connect ((receiver compute-object) local-slot
                     (transmitter compute-object) transmitter-slot
                     &key)
  (%connect receiver local-slot transmitter (lambda (obj) (funcall transmitter-slot obj))))

(defmethod  connect ((receiver compute-object) local-slot
                     (transmitter compute-object) (transmitter-slot symbol)
                     &key)
  (%connect receiver local-slot transmitter (lambda (obj)
                                              (funcall (symbol-function transmitter-slot) obj))))

(defgeneric disconnect (receiver local-slot transmitter &key))

(defmethod disconnect ((receiver compute-object) local-slot (transmitter compute-object) &key)
  (let* ((cell (slot-value receiver local-slot))
         (source (%input-source cell)))
    (setf (receivers source) (delete receiver (receivers source)))
    (setf (car cell) nil)
    (setf (cdr cell) nil)))

(defgeneric connect-plist (reciever local-slot key transmitter accessor-fn &key))

(defmethod connect-plist ((reciever compute-object) local-slot key
                          (transmitter compute-object) accessor-fn &key)
  (setf (getf (slot-value reciever local-slot) key) (cons transmitter accessor-fn))
  (push reciever (receivers transmitter))
  (notify-invalid receiver)
  receiver)

(defgeneric disconnect-plist (receiver local-slot key))

(defmethod disconnect-plist ((receiver compute-object) local-slot key)
  (let* ((cell (slot-value receiver local-slot))
         (source (%input-source cell)))
    (setf (car cell) nil)
    (setf (cdr cell) nil)
    (when (remf (receivers source) key)
      (setf (receivers source) (delete receiver (receivers source)))
      (notify-invalid receiver))))

(defgeneric connectedp (receiver local-slot))

(defmethod connectedp ((receiver compute-object) local-slot)
  (let ((cell (slot-value receiver local-slot)))
    (not (null (cdr cell)))))

(defmethod invalidate ((obj compute-object))
  (let ((computed-slots (computed-slots (class-of obj))))
    (loop
       for slot in computed-slots
       do (slot-makunbound obj slot))
    (mapc #'notify-invalid (receivers obj))))

;;; Apparently we need to make sure the method classes are finalized before we start creating
;;; method objects.

(mapc (lambda (name)
        (closer-mop:finalize-inheritance (find-class name)))
      '(compute-function-method input-reader-method input-writer-method))

(defclass if-then-node ()
  ((test :input-accessor test :initarg :test)
   (then :input-accessor then :initarg :then)
   (else :input-accessor else :initarg else)
   (result :compute-function result))
  (:metaclass compute-class)
  (:documentation "Choose the value of the @c(then) or @c(else) input, based on the value of the
  @c(if) input."))

(defmethod result ((node if-then-node))
  (if (test node)
      (then node)
      (else node)))

(defclass input-node ()
  ((in :input-accessor in :initarg :in)
   (out :compute-function out))
  (:metaclass compute-class))

(defmethod out ((obj input-node))
  (in obj))
  
#++
(progn

  (defclass two-arg ()
    ((arg1 :initarg :arg1)
     (arg2 :initarg :arg2)))
  
  (defgeneric plus (obj))

  (defclass plus-node (two-arg)
    ((arg1 :input-accessor arg1)
     (arg2 :input-accessor arg2)
     (plus :compute-function plus))
    (:metaclass compute-class))

  (defmethod plus ((obj plus-node))
    (+ (arg1 obj) (arg2 obj)))

  (defclass mult-node (two-arg)
    ((arg1 :input-accessor arg1)
     (arg2 :input-accessor arg2)
     (mult :compute-function mult))
    (:metaclass compute-class))
  
  (defmethod mult ((node mult-node))
    (* (arg1 node) (arg2 node)))

  (defparameter *source1* (make-instance 'input-node :in 4))
  (defparameter *source2* (make-instance 'input-node :in 8))

  (defparameter *plus-node* (make-instance 'plus-node))
  (defparameter *mult-node* (make-instance 'mult-node))

  (connect *plus-node* 'arg1 *source1* 'out)
  (connect *plus-node* 'arg2 *source2* 'out)
  
  (connect *mult-node* 'arg1 *source1* 'out)
  (connect *mult-node* 'arg2 *source2* 'out)

  (defparameter *mult-node2* (make-instance 'mult-node))
  (connect *mult-node2* 'arg1 *plus-node* 'plus)
  (connect *mult-node2* 'arg2 *mult-node* 'mult)

  ;; Value of *mult-node2* should be 384.
  (format t "~%*mult-node2*: ~S" (mult *mult-node2*))

  (defparameter *mult-node3* (make-instance 'mult-node))
  (connect *mult-node3* 'arg1 *mult-node2* 'mult)
  (setf (arg2 *mult-node3*) 10)

  ;; Value of *mult-node3* should be 3840.
  (format t "~%*mult-node3*: ~S" (mult *mult-node3*))
  )
