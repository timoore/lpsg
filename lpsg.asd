;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(cl:in-package #:asdf-user)

(defsystem lpsg
  :description "OpenGL rendering pipeline."
  :version "0.0.0"
  :author "Tim Moore <timoore33@gmail.com>"
  :license "Revised BSD"
  :depends-on (cl-opengl sb-cga mathkit alexandria trivial-garbage serapeum closer-mop)
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
     (:file "environment" :depends-on (base "render" "glstate"))
     (:file "bundle" :depends-on (base "render" "environment" "glstate"))
     (:file "effect" :depends-on (base "render" "environment" "glstate"  "bundle"
                                       "incremental-comp"))
     (:file "shape" :depends-on (base "render" "environment" "allocator" "incremental-comp"))
     (:file "assembly" :depends-on (base "shape"))))
   (:module
    "scene"
    :depends-on ("lpsg")
    :components
    ((:file "package")
     (:file "camera" :depends-on ("package"))
     (:file "draggers" :depends-on ("package"))
     (:file "solids" :depends-on ("package"))))))
