;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

;;; Classes for the nodes in an incremental computation graph

;;; A named source can contain a list i.e., the conceptual source is a list of
;;; values.

;;; The SOURCES and SINKS of a computation are named by a symbol so that they
;;; can be easily managed. A computation node class is free to also keep a
;;; source or sink in a slot for convenient access.

(defclass computation-node ()
  ((sinks :accessor sinks :initform nil)
   (cached-value :accessor cached-value :initarg :inital-value)
   (validp :accessor validp :initform t)))

(defgeneric add-sink (node sink))

(defgeneric delete-sink (node))

(defmethod add-sink ((node computation-node) sink)
  (pushnew sink (sinks node))
  sink)

(defmethod delete-sink ((node computation-node))
  (delete node (sinks node)))

(defgeneric value (node))

(defmethod value ((node computation-node))
  (if (validp node)
      (cached-value node)
      (let ((new-value (compute node)))
        (setf (cached-value node) new-value)
        (setf (validp node) t)
        new-value)))

(defgeneric compute (node))

(defgeneric invalidate-calculation (node invalid-source))

(defmethod invalidate-calculation ((node computation-node) invalid-source)
  (declare (ignorable invalid-source))
  (when (validp node)
    (mapc (lambda (sink) (invalidate-calculation sink node))
          (sinks node))
    (setf (validp node) nil)))

(defclass input-value-node (computation-node)
  ())

(defgeneric (setf value) (value node))

(defmethod (setf value) (value (node input-value-node))
  (setf (cached-value node) value)
  (invalidate-calculation node node)
  value)

;;; Testing

#+(or)
(progn
(defclass plus-node (computation-node)
  ((arg1 :accessor arg1)
   (arg2 :accessor arg2)))

(defmethod compute ((node plus-node))
  (+ (value (arg1 node) frame)
     (value (arg2 node) frame)))

(defclass mult-node (computation-node)
  ((arg1 :accessor arg1)
   (arg2 :accessor arg2)))

(defmethod compute ((node mult-node))
  (* (value (arg1 node) frame)
     (value (arg2 node) frame)))

(defvar *source1* (make-instance 'input-value-node))
(defvar *source2* (make-instance 'input-value-node))

(defvar *plus-node* (make-instance 'plus-node))
(defvar *mult-node* (make-instance 'mult-node))

(setf (arg1 *plus-node*) *source1*)
(add-sink *source1* *plus-node*)
(setf (arg2 *plus-node*) *source2*)
(add-sink *source2* *plus-node*)

(setf (arg1 *mult-node*) *source1*)
(add-sink *source1* *mult-node*)
(setf (arg2 *mult-node*) *source2*)
(add-sink *source2* *mult-node*)

(setf (value *source1*) 4)
(setf (value *source2*) 8)
)
