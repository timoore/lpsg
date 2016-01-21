;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(cl:in-package #:asdf-user)

(defsystem lpsg
  :description "OpenGL rendering pipeline."
  :version "0.0.0"
  :author "Tim Moore <timoore33@gmail.com>"
  :license "Revised BSD"
  :depends-on (cl-opengl sb-cga mathkit alexandria trivial-garbage)
  :components
  ((:module "lpsg"
    :components
    ((:file "package")
     (:file "macros")
     (:file "interface" :depends-on ("package"))
     (:file "protocol" :depends-on ("package"))
     (:file "incremental-comp" :depends-on ("package" "protocol"))
     (:file "uset" :depends-on ("package"))
     (:file "allocator" :depends-on ("package"))
     (:file "render" :depends-on ("package" "uset" "interface"))
     (:file "environment" :depends-on ("package" "render" "incremental-comp"))
     (:file "bundle" :depends-on ("package" "render" "environment"))
     (:file "effect" :depends-on ("package" "render" "environment" "bundle"))
     (:file "shape" :depends-on ("package" "render" "environment" "allocator" "incremental-comp"))
     (:file "assembly" :depends-on ("package" "shape"))
     (:file "solids" :depends-on ("package" "render" "shape"))))))
