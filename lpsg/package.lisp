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

(defpackage #:lpsg
  ;; Some symbols conflict with cl-opengl's.
  (:shadowing-import-from #:sb-cga
                          #:translate
                          #:rotate
                          #:scale)
  (:shadowing-import-from #:kit.math
                          #:frustum)
  (:use #:cl #:gl #:sb-cga #:kit.math)
  (:export
   #:render-bundle
   ;; renderer protocol
   #:renderer
   #:open-renderer
   #:close-renderer
   #:add-bundle
   #:draw
   #:submit
   #:submit-with-effect
   #:retract
   #:retract-with-effect
   #:standard-renderer
   #:shader
   #:program
   ;; buffers and attributes
   #:gl-buffer
   #:buffer
   #:attribute
   #:data
   #:attributes
   #:vertex-attribute
   ;; usets
   #:define-uset
   ;; graphics state
   #:graphics-state
   #:units
   #:uniform-sets
   #:environment
   #:effect
   #:simple-effect
   #:texture-area
   #:texture-2d
   #:sampler
   #:gl-state
   #:units
   ;; Utilities
   ;; allocators and allocation
   #:compute-shape-allocation
   #:simple-allocator
   #:interleaved-attribute-allocator
   #:open-allocator
   #:allocate-target
   #:close-allocator
   #:with-allocator
   ;; incremental computation
   #:if-then-node
   #:then
   #:else
   #:sink-node
   #:sink-node-mixin
   #:source-node
   #:source-node-mixin
   #:source-sink-mixin
   #:computation-node
   #:computation-node-mixin
   #:delete-sink
   #:value
   #:compute
   #:notify-invalid-input
   #:input-value-node
   #:input
   #:input-value
   ;; inputs
   #:visiblep
   ;; utilities
   #:make-cube-shape
   ))
