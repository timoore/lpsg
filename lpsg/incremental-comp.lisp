;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

;;; Classes for the nodes in an incremental computation graph

;;; A computation graph represents computations that are evaluated in a lazy manner. The inputs to
;;; the computation can change and invalidate the whole computation, but the calculation won't be
;;; evaluated until its results are needed. In LPSG, the usets that are used by environments are
;;; kept up-to-date using computation graphs.

;;; Each node in the graph has an associated value which is retrieved with the VALUE function. This
;;; value may be constant, but in general it needs to be computed using the values of the nodes
;;; that are inputs (called "sources") to the given node. The value is usually cached and doesn't
;;; need to be recomputed for each call to VALUE. However, if the value of an input has changed
;;; since the cached value was updated, then it will be recomputed.

;;; When a node is invalidated, all the nodes which use its value (called "sinks") are invalidated
;;; too. Note that no computation will be done until a node's value is explicitly called for.

;;; A node's inputs are named. They need not be specified in advance; they behave like named object
;;; properties. The values that are computed can be quite general i.e., any Lisp object
;;; other than subclasses of COMPUTATION-GRAPH-NODE.

(defgeneric value (node)
  (:documentation "Return NODE's value."))

;;; Any list object has a value -- itself.
(defmethod value ((node t))
  node)

(defgeneric validp (node)
  (:documentation "Returns T if a node's value is valid. "))

(defmethod validp ((node t))
  t)

(define-protocol-class sink-node ()
  ((:accessor inputs :documentation "alist of (input-name . source-node).")
   (:accessor input :lambda-list (node name)
              :documentation "Returns a NODE's input named NAME. Second value indicates if input
exists or not.")
   (:generic delete-input (node input-name))
   (:generic notify-invalid-input (node invalid-source input-name)))
  (:documentation "Node that consumes values via named inputs."))

(defclass sink-node-mixin ()
  ((inputs :accessor inputs :initform nil :initarg :inputs))
  (:documentation "Mixin class that provides slots and some methods for the SINK-NODE protocol
class"))

(defmethod input ((node sink-node-mixin) input-name)
  (let ((input-entry (assoc input-name (inputs node))))
    (if input-entry
        (values (cdr input-entry) t)
        (values nil nil))))

(defmethod (setf input) (new-val (node sink-node-mixin) input-name)
  (let ((input-entry (assoc input-name (inputs node))))
    (if input-entry
        (setf (cdr input-entry) new-val)
        (setf (inputs node) (acons input-name new-val (inputs node)))))
  new-val)

(defun input-value (node input-name)
  (multiple-value-bind (source sourcep)
      (input node input-name)
    (if sourcep
        (value source)
        (error "Input ~S does not exist." input-name))))

(defmethod notify-invalid-input ((node sink-node) invalid-source input-name)
  (declare (ignore invalid-source input-name))
  nil)

(define-protocol-class source-node ()
  ((:accessor validp)
   (:accessor sinks :documentation "list of (node . input-names)")
   (:generic delete-sink (node sink input-name)))
  
  (:documentation "Note: the input names belong to the sink nodes of this node."))

;;; Let arbitrary Lisp objects serve as inputs.

(defmethod delete-sink ((node t) sink input-name)
  nil)

(defclass source-node-mixin ()
  ((validp :accessor validp :initform nil)
   (sinks :accessor sinks :initform nil))
  (:documentation "Mixin class that provides slots and some methods for the SOURCE-NODE protocol
class."))

(defmethod (setf input) :after ((new-val source-node-mixin) sink input-name)
  (let ((node-entry (find sink (sinks new-val) :key #'car)))
    (if node-entry
        (pushnew input-name (cdr node-entry))
        (push (list sink input-name) (sinks new-val)))))

(defmethod delete-sink ((node source-node-mixin) sink input-name)
  (let ((node-entry (find sink (sinks node) :key #'car)))
    (when node-entry
      (setf (cdr node-entry) (delete input-name (cdr node-entry)))
      (when (null (cdr node-entry))
        (setf (sinks node) (delete sink (sinks node) :key #'car))))))

(defmethod notify-invalid-input :after ((node source-node-mixin) invalid-source input-name)
  (declare (ignorable invalid-source input-name))
  (when (validp node)
    (setf (validp node) nil)
    (loop
       for (sink . inputs) in (sinks node)
       do (mapc (lambda (name) (notify-invalid-input sink node name))
                inputs))))

(defmethod delete-input ((node sink-node-mixin) input-name)
  (multiple-value-bind (source sourcep)
      (input node input-name)
    (when sourcep
      (setf (inputs node) (delete input-name (inputs node) :key #'car))
      (delete-sink source node input-name)))
  nil)

(defclass source-sink-mixin (source-node-mixin sink-node-mixin)
  ()
  (:documentation "Convenience mixin class the slots necessary to implement sources and sinks."))

(define-protocol-class computation-node (source-node sink-node)
  (( :accessor cached-value)
   (:generic compute (node))))

(defmethod value ((node computation-node))
  (if (validp node)
      (cached-value node)
      (let ((new-value (compute node)))
        (setf (cached-value node) new-value)
        (setf (validp node) t)
        new-value)))

(defclass computation-node-mixin ()
  ((cached-value :accessor cached-value)))

(defclass input-value-node (source-node source-node-mixin)
  ((value :accessor value))
  (:documentation "A node whose value can be set. Useful as the source to multiple nodes."))

(defmethod initialize-instance :after ((node input-value-node) &key (value nil valuep))
  (when valuep
    (setf (slot-value node 'value) value)
    (setf (validp node) t)))

(defmethod (setf value) :after (value (node input-value-node))
  (setf (validp node) nil)
  (notify-invalid-input node node nil)
  (setf (validp node) t))

(defmethod notify-invalid-input ((node input-value-node) source input-name)
  (declare (ignore source input-name))
  nil)
;;; Testing

#+(or)
(progn
  (defclass plus-node (computation-node computation-node-mixin source-sink-mixin)
    ())

  (defmethod compute ((node plus-node))
    (let ((arg1 (input-value node 'arg1))
          (arg2 (input-value node 'arg2)))
      (+ arg1 arg2)))

  (defclass mult-node (computation-node computation-node-mixin source-sink-mixin)
    ())

  (defmethod compute ((node mult-node))
    (let ((arg1 (input-value node 'arg1))
          (arg2 (input-value node 'arg2)))
      (* arg1 arg2)))

  (defparameter *source1* (make-instance 'input-value-node))
  (defparameter *source2* (make-instance 'input-value-node))

  (defparameter *plus-node* (make-instance 'plus-node))
  (defparameter *mult-node* (make-instance 'mult-node))

  (setf (input *plus-node* 'arg1) *source1*)
  (setf (input *plus-node* 'arg2) *source2*)

  (setf (input *mult-node* 'arg1) *source1*)
  (setf (input *mult-node* 'arg2) *source2*)

  (setf (value *source1*) 4)
  (setf (value *source2*) 8)

  (defparameter *mult-node2* (make-instance 'mult-node))
  (setf (input *mult-node2* 'arg1) *plus-node*)
  (setf (input *mult-node2* 'arg2) *mult-node*)
  ;; Value of *mult-node2* should be 384.
  (format t "~%*mult-node2*: ~S" (value *mult-node2*))

  (defparameter *mult-node3* (make-instance 'mult-node))
  (setf (input *mult-node3* 'arg1) *mult-node2*)
  (setf (input *mult-node3* 'arg2) 10)
  ;; Value of *mult-node3* should be 3840.
  (format t "~%*mult-node3*: ~S" (value *mult-node3*))
  )
