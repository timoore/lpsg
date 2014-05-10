;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Copyright (c) 2014 Tim Moore (moore@bricoworks.com)
;;;   All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;;  o Redistributions of source code must retain the above copyright notice,
;;;    this list of conditions and the following disclaimer.
;;;  o Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;  o Neither the name of the author nor the names of the contributors may be
;;;    used to endorse or promote products derived from this software without
;;;    specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package #:lpsg)

;;; A mutable object contains a counter that is incremented whenever the object
;;; is mutated. By keeping a copy of the counter when the object was last read,
;;; one can tell if one has already seen the most recent copy of the object.
;;;
;;; The counter is not updated automatically, but helper forms are provided to
;;; make it convenient to update the counter when multiple elements of a
;;; mutable object are updated.
;;;
;;; If an object is mutated, objects that hold it may need to have their
;;; counters updated too.

(defclass mutable ()
  ((counter :accessor counter :initform 0)))

(defmacro with-mutable (obj &body body)
  `(progn
     (incf (counter ,obj))
     ,@body))

(defun up-to-date-p (mutable-object local-ref local-counter)
  (and (eq mutable-object local-ref)
       (eql (counter mutable-object) local-counter)))

