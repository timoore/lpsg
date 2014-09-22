;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

;;; Classes for the nodes in an incremental computation graph

;;; A named source can contain a list i.e., the conceptual source is a list of
;;; values.

;;; The SOURCES and SINKS of a computation are named by a symbol so that they
;;; can be easily managed. A computation node class is free to also keep a
;;; source or sink in a slot for convenient access.

(defvar *frame-counter* 0)

(defclass computation-node ()
  ((sinks :accessor sinks :initform nil)
   (cached-value :accessor cached-value :initarg :inital-value)
   (invalidated-on-frame :accessor invalidated-on-frame :initform 0)
   (refreshed-on-frame :accessor refreshed-on-frame :initform -1)))

(defgeneric add-sink (node sink))

(defgeneric delete-sink (node))


(defmethod add-sink ((node computation-node) sink)
  (pushnew sink (sinks node))
  sink)

(defmethod delete-sink ((node computation-node))
  (delete node (sinks node)))

(defgeneric value (node &optional frame))

(defmethod value ((node computation-node) &optional (frame *frame-counter*))
  (if (<= (invalidated-on-frame node) (refreshed-on-frame node))
      (cached-value node)
      (let ((new-value (compute node frame)))
        (setf (cached-value node) new-value)
        (setf (refreshed-on-frame node) frame)
        new-value)))

(defgeneric compute (node &optional frame))

(defun invalidate-calculation (node &optional (frame *frame-counter*))
  (when (<= (invalidated-on-frame node) frame)
    (mapc (lambda (sink) (invalidate-calculation sink frame))
          (sinks node))
    (setf (invalidated-on-frame node) frame)))

(defclass input-value-node (computation-node)
  ())

(defgeneric (setf value) (value node &optional frame))

(defmethod (setf value) (value (node input-value-node)
                         &optional (frame *frame-counter*))
  (setf (cached-value node) value)
  (invalidate-calculation node frame)
  (setf (refreshed-on-frame node) frame)
  value)

;;; Testing

#+(or)
(progn
(defclass plus-node (computation-node)
  ((arg1 :accessor arg1)
   (arg2 :accessor arg2)))

(defmethod compute ((node plus-node) &optional frame)
  (+ (value (arg1 node) frame)
     (value (arg2 node) frame)))

(defclass mult-node (computation-node)
  ((arg1 :accessor arg1)
   (arg2 :accessor arg2)))

(defmethod compute ((node mult-node) &optional frame)
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

(setf *frame-counter* 1)

(setf (value *source1*) 4)
(setf (value *source2*) 8)
)
