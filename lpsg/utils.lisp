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

(defun identity-matrix ()
  (make-array '(4 4) :element-type 'single-float
              :initial-contents '((1.0 0.0 0.0 0.0)
                                  (0.0 1.0 0.0 0.0)
                                  (0.0 0.0 1.0 0.0)
                                  (0.0 0.0 0.0 1.0))))

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

(defun perspective-matrix (left right bottom top near far)
  (let ((result (make-array '(4 4) :element-type 'single-float)))
    (setf (aref result 0 0) (/ (* 2.0 near) (- right left)))
    (setf (aref result 0 1) 0.0)
    (setf (aref result 0 2) 0.0)
    (setf (aref result 0 3) 0.0)
    (setf (aref result 1 0) 0.0)
    (setf (aref result 1 1) (/ (* 2.0 near) (- top bottom)))
    (setf (aref result 1 2) 0.0)
    (setf (aref result 1 3) 0.0)
    (setf (aref result 2 0) (/ (+ right left) (- right left)))
    (setf (aref result 2 1) (/ (+ top bottom) (- top bottom)))
    (setf (aref result 2 2) (/ (- (+ far near)) (- far near)))
    (setf (aref result 2 3) -1.0)
    (setf (aref result 3 0) 0.0)
    (setf (aref result 3 1) 0.0)
    (setf (aref result 3 2) (- (/ (* 2.0 far near) (- far near))))
    (setf (aref result 3 3) 0.0)
    result))

(defun perspective (fovy aspect near far)
  "Create a perspective transform matrix. FOVY is the vertical angle of view; ASPECT is the aspect
ratio of the viewing window.

Like gluPerspective.1"
  (let* ((height/2 (* near (tan (/ fovy 2.0))))
         (width/2 (* height/2 aspect)))
    (perspective-matrix (- width/2) width/2 (- height/2) height/2 near far)))

(defun vec+ (v1 v2)
  (let ((result (make-array (list (length v1)) :element-type (array-element-type v1))))
    (loop
       for i from 0 below (length v1)
       do (setf (aref result i) (+ (aref v1 i) (aref v2 i))))
    result))

(defun vec- (v1 v2)
  (let ((result (make-array (list (length v1)) :element-type (array-element-type v1))))
    (loop
       for i from 0 below (length v1)
       do (setf (aref result i) (- (aref v1 i) (aref v2 i))))
    result))


(defun dot (v1 v2)
  (let ((result 0))
    (loop
       for i from 0 below (length v1)
       do (setf result (+ result (* (aref v1 i) (aref v2 i)))))
    result))

(defun cross (v1 v2)
  (let ((result (make-array '(3) :element-type (array-element-type v1))))
    (setf (aref result 0) (- (* (aref v1 1) (aref v2 2)) (* (aref v2 1) (aref v1 2))))
    (setf (aref result 1) (- (* (aref v2 0) (aref v1 2)) (* (aref v1 0) (aref v2 2))))
    (setf (aref result 2) (- (* (aref v1 0) (aref v2 1)) (* (aref v2 0) (aref v1 1))))
    result))
