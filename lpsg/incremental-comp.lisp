;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

;;; Classes for the nodes in an incremental computation graph

;;; Source nodes supply a value to the named input of another node which is called a "sink node."

;;; A named source can contain a list i.e., the conceptual source is a list of
;;; values.

;;; The SOURCES and SINKS of a computation are named by a symbol so that they
;;; can be easily managed. A computation node class is free to also keep a
;;; source or sink in a slot for convenient access.

(defclass consumer-node ()
  ((validp :accessor validp :initform nil)
   (inputs :accessor inputs :initarg :inputs :initform nil
           :documentation "alist of (input-name . source-node)."))
  (:documentation "Node that consumes values via named inputs."))

(defgeneric add-source (node source input-name))
(defgeneric delete-source (node source input-name))

(defmethod add-source ((node consumer-node) source input-name)
  (let ((source-entry (assoc input-name (inputs node))))
    (if source-entry
        (setf (cdr source-entry) source)
        (setf (inputs node) (acons input-name source (inputs node)))))
  (inputs node))

(defmethod delete-source ((node consumer-node) source input-name)
  (let ((source-entry (assoc input-name (inputs node))))
    (when source-entry
      (setf (cdr source-entry) nil))))

(defclass computation-node (consumer-node)
  ((sinks :accessor sinks :initform nil :documentation "list of (node . input-names)")
   (cached-value :accessor cached-value :initarg :inital-value))
  (:documentation "Note: the input names belong to the sink nodes of this node."))

(defgeneric add-sink (node sink input-name))

(defgeneric delete-sink (node sink input-name))

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

(defgeneric value (node))

(defmethod value ((node computation-node))
  (if (validp node)
      (cached-value node)
      (let ((new-value (compute node)))
        (setf (cached-value node) new-value)
        (setf (validp node) t)
        new-value)))

(defgeneric compute (node))

(defgeneric invalidate-calculation (node invalid-source input-name))

(defmethod invalidate-calculation ((node consumer-node) invalid-source input-name)
  (declare (ignorable invalid-source input-name)))

(defmethod invalidate-calculation :after ((node consumer-node) invalid-source input-name)
  (declare (ignorable invalid-source input-name))
  (setf (validp node) nil))

(defmethod invalidate-calculation ((node computation-node) invalid-source input-name)
  (declare (ignorable invalid-source input-name))
  (when (validp node)
    (loop
       for (sink . inputs) in (sinks node)
       do (mapc (lambda (name) (invalidate-calculation sink node name))
                inputs))))

(defclass input-value-node (computation-node)
  ())

(defmethod initialize-instance :after ((node input-value-node) &key (value nil valuep))
  (when valuep
    (setf (value node) value)))

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
    ()
    (:default-initargs :inputs (list (list 'arg1) (list 'arg2))))

  (defmethod compute ((node plus-node))
    (let ((arg1 (value (cdr (assoc 'arg1 (inputs node)))))
          (arg2 (value (cdr (assoc 'arg2 (inputs node))))))
      (+ arg1 arg2)))

  (defclass mult-node (computation-node)
    ()
    (:default-initargs :inputs (list (list 'arg1) (list 'arg2))))

  (defmethod compute ((node mult-node))
    (let ((arg1 (value (cdr (assoc 'arg1 (inputs node)))))
          (arg2 (value (cdr (assoc 'arg2 (inputs node))))))
      (* arg1 arg2)))

  (defparameter *source1* (make-instance 'input-value-node))
  (defparameter *source2* (make-instance 'input-value-node))

  (defparameter *plus-node* (make-instance 'plus-node))
  (defparameter *mult-node* (make-instance 'mult-node))

  (connect *source1* *plus-node* 'arg1)
  (connect *source2* *plus-node* 'arg2)

  (connect *source1* *mult-node* 'arg1)
  (connect *source2* *mult-node* 'arg2)


  (setf (value *source1*) 4)
  (setf (value *source2*) 8)

  (defparameter *mult-node2* (make-instance 'mult-node))
  (connect *plus-node* *mult-node2* 'arg1)
  (connect *mult-node* *mult-node2* 'arg2)
  ;; Value of *mult-node2* should be 384.
  )
