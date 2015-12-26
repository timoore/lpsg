;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(cl:in-package #:asdf-user)

(defsystem lpsg
  :description "OpenGL rendering pipeline."
  :version "0.0.0"
  :author "Tim Moore <timoore33@gmail.com>"
  :license "Revised BSD"
  :depends-on (cl-opengl sb-cga mathkit)
  :components
  ((:module "lpsg"
    :components
    ((:file "package")
     (:file "interface" :depends-on ("package"))
     (:file "incremental-comp" :depends-on ("package"))
     (:file "assembly" :depends-on ("package"))
     (:file "uset" :depends-on ("package"))
     (:file "allocator" :depends-on ("package"))
     (:file "render" :depends-on ("package" "uset" "assembly" "interface"))
     (:file "environment" :depends-on ("package" "render"))
     (:file "bundle" :depends-on ("package" "render" "environment"))
     (:file "effect" :depends-on ("package" "render" "environment" "bundle"))
     (:file "shape" :depends-on ("package" "render" "environment" "allocator"))
     (:file "solids" :depends-on ("package" "render" "shape"))))))
