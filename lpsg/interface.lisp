;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;; Early defgeneric forms for LPSG.

(in-package #:lpsg)

(defgeneric submit (assembly renderer))

(defgeneric submit-with-effect (shape renderer effect))


(defgeneric add-object (parent child))

