;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(cl:in-package #:asdf-user)

(defsystem lpsg
  :description "OpenGL rendering pipeline."
  :version "0.0.0"
  :author "Tim Moore <timoore33@gmail.com>"
  :license "Revised BSD"
  :depends-on (cl-opengl sb-cga mathkit alexandria trivial-garbage serapeum)
  :components
  ((:module "lpsg"
    :components
    ((:module base
      :pathname ""
      :serial t
      :components
      ((:file "package")
       (:file "macros")
       (:file "protocol")
       (:file "interface")))
     (:file "incremental-comp" :depends-on (base))
     (:file "uset" :depends-on (base))
     (:file "allocator" :depends-on (base))
     (:file "glstate" :depends-on (base))
     (:file "render" :depends-on (base "uset" "glstate"))
     (:file "environment" :depends-on (base "render" "incremental-comp" "glstate"))
     (:file "bundle" :depends-on (base "render" "environment" "glstate"))
     (:file "effect" :depends-on (base "render" "environment" "bundle" "glstate"))
     (:file "shape" :depends-on (base "render" "environment" "allocator" "incremental-comp"))
     (:file "assembly" :depends-on (base "shape"))
     (:file "solids" :depends-on (base "render" "shape"))))
   (:module
    "tinker"
    :depends-on ("lpsg")
    :components
    ((:file "package")
     (:file "camera" :depends-on ("package"))
     (:file "draggers" :depends-on ("package"))))))
