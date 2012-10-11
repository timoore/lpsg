;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Copyright (c) 2012, Tim Moore (moore@bricoworks.com)
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

(defun ortho-matrix (left right bottom top near far)
  (let ((result (make-array '(4 4) :element-type 'single-float)))
    (setf (aref result 0 0) (/ 2.0 (- right left)))
    (setf (aref result 0 1) 0.0)
    (setf (aref result 0 2) 0.0)
    (setf (aref result 0 3) 0.0)
    (setf (aref result 1 0) 0.0)
    (setf (aref result 1 1) (/ 2.0 (- top bottom)))
    (setf (aref result 1 2) 0.0)
    (setf (aref result 1 3) 0.0)
    (setf (aref result 2 0) 0.0)
    (setf (aref result 2 1) 0.0)
    (setf (aref result 2 2) (/ -2.0 (- far near)))
    (setf (aref result 2 3) 0.0)
    (setf (aref result 3 0) (- (/ (+ right left) (- right left))))
    (setf (aref result 3 1) (- (/ (+ top bottom) (- top bottom))))
    (setf (aref result 3 2) (- (/ (+ far near) (- far near))))
    (setf (aref result 3 3) 1.0)
    result))
