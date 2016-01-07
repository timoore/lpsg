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
  (:documentation "Compute NODE's value."))

(defmethod value ((node t))
  node)

(defgeneric validp (node)
  (:documentation "Returns T if a node's value is valid. "))

(defclass consumer-node ()
  ((inputs :accessor inputs :initarg :inputs :initform nil
           :documentation "alist of (input-name . source-node)."))
  (:documentation "Node that consumes values via named inputs."))

(defclass computation-node (consumer-node)
  ((validp :accessor validp :initform nil)
   (sinks :accessor sinks :initform nil :documentation "list of (node . input-names)")
   (cached-value :accessor cached-value :initarg :inital-value))
  (:documentation "Note: the input names belong to the sink nodes of this node."))

(defgeneric computation-node-p (node))

(defmethod computation-node-p ((node computation-node))
  t)

(defmethod computation-node-p ((node t))
  nil)

(defgeneric input (node name)
  (:documentation "Returns the value of a node's input. Second value indicates if input exists or
  not."))

(defgeneric add-source (node source input-name))
(defgeneric delete-source (node source input-name))

(defmethod input ((node consumer-node) input-name)
  (let ((input-entry (assoc input-name (inputs node))))
    (if input-entry
        (values (cdr input-entry) t)
        (values nil nil))))

(defun input-value (node input-name)
  (multiple-value-bind (source sourcep)
      (input node input-name)
    (if sourcep
        (value source)
        (error "Input ~S does not exist." input-name))))

(defgeneric (setf input) (new-val node input-name))
(defgeneric add-sink (node sink input-name))

(defmethod add-sink ((node t) sink input-name)
  nil)

(defgeneric delete-sink (node sink input-name))

(defmethod delete-sink ((node t) sink input-name)
  nil)

(defmethod (setf input) (new-val (node consumer-node) input-name)
  (let ((input-entry (assoc input-name (inputs node))))
    (if input-entry
        (setf (cdr input-entry) new-val)
        (setf (inputs node) (acons input-name new-val (inputs node)))))
  (add-sink new-val node input-name)
  new-val)

(defgeneric delete-input (node input-name))

(defmethod delete-input ((node consumer-node) input-name)
  (multiple-value-bind (source sourcep)
      (input node input-name)
    (when sourcep
      (setf (inputs node) (delete input-name (inputs node) :key #'car))
      (delete-sink source node input-name)))
  nil)

(defgeneric compute (node))
(defgeneric invalidate-calculation (node invalid-source input-name))

(defmethod (setf input) :after (new-val (node computation-node) input-name)
  (invalidate-calculation node new-val input-name))

(defmethod add-sink ((node computation-node) sink input-name)
  (let ((node-entry (find sink (sinks node) :key #'car)))
    (if node-entry
        (pushnew input-name (cdr node-entry))
        (push (list sink input-name) (sinks node)))
    (values sink input-name)))

(defmethod delete-sink ((node computation-node) sink input-name)
  (let ((node-entry (find sink (sinks node) :key #'car)))
    (when node-entry
      (setf (cdr node-entry) (delete input-name (cdr node-entry)))
      (when (null (cdr node-entry))
        (setf (sinks node) (delete sink (sinks node) :key #'car))))))

(defmethod value ((node computation-node))
  (if (validp node)
      (cached-value node)
      (let ((new-value (compute node)))
        (setf (cached-value node) new-value)
        (setf (validp node) t)
        new-value)))

(defmethod invalidate-calculation ((node computation-node) invalid-source input-name)
  (declare (ignorable invalid-source input-name))
  (when (validp node)
    (setf (validp node) nil)
    (loop
       for (sink . inputs) in (sinks node)
       do (mapc (lambda (name) (invalidate-calculation sink node name))
                inputs))))

(defclass input-value-node (computation-node)
  ()
  (:documentation "A node whose value can be set. Useful as the source to multiple nodes."))

(defmethod initialize-instance :after ((node input-value-node) &key (value nil valuep))
  (when valuep
    (setf (value node) value)
    (setf (validp node) t)))

(defgeneric (setf value) (value node))

(defmethod (setf value) (value (node input-value-node))
  (setf (cached-value node) value)
  (invalidate-calculation node node nil)
  (setf (validp node) t)
  value)

(defgeneric connect (source sink input-name)
  (:documentation "Connect the output of SOURCE to the input of SINK named INPUT-NAME."))

(defmethod connect ((source computation-node) (sink consumer-node) input-name)
  (add-source sink source input-name)
  (add-sink source sink input-name))

;;; Testing

#+(or)
(progn
  (defclass plus-node (computation-node)
    ())

  (defmethod compute ((node plus-node))
    (let ((arg1 (input-value node 'arg1))
          (arg2 (input-value node 'arg2)))
      (+ arg1 arg2)))

  (defclass mult-node (computation-node)
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
