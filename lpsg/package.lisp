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
   #:render-queue
   #:unordered-render-queue
   #:ordered-render-queue
   #:render-stage
   #:render-queues
   #:add-rendered-object
   #:remove-rendered-object
   #:map-render-queue
   #:find-if-queue
   #:do-render-queue
   #:finalize-queue
   #:render-target-stage
   #:clear-colors
   #:depth-clear
   #:framebuffer-object
   #:retract
   #:retract-with-effect
   #:standard-renderer
   #:default-render-queue
   ;; buffers and attributes
   #:gl-buffer
   #:buffer
   #:attribute
   #:data
   #:attributes
   #:buffer-attribute
   #:mirrored-buffer-resource
   #:schedule-upload
   #:vertex-attribute
   ;; shapes
   #:shape
   #:standard-shape
   #:drawable
   #:array-drawable
   #:indexed-drawable
   ;; usets
   #:define-uset
   ;; graphics state
   #:graphics-state
   ;; XXX glstate-foo should probably not be external
   #:glstate-program                    ;?
   #:glstate-texunits                   ;?
   #:gl-cull-face
   #:gl-depth-func
   #:gl-depth-range
   #:gl-texunits
   #:gltexture-unit
   #:gl-viewport
   #:make-modes
   #:units
   #:uniform-sets
   #:environment
   #:effect
   #:simple-effect
   #:simple-effect-usets
   #:update-effect
   #:gl-object
   #:texture-area
   #:raw-mirrored-texture-resource
   #:texture
   #:texture-2d
   #:sampler
   #:shader
   #:program
   #:gltexture-unit
   #:gl-state
   #:units
   #:gl-finalize
   #:gl-finalized-p
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
   #:test
   #:then
   #:else
   #:result
   #:compute-class
   #:notify-invalid
   #:connect
   #:invalidate
   #:input-node
   #:in
   #:out
   ;; inputs
   #:visiblep
   ;; utilities
   #:make-cube-shape
   ))
