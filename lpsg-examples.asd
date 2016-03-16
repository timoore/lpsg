;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(cl:in-package #:asdf-user)

(defsystem lpsg-examples
  :description "OpenGL rendering pipeline examples"
  :depends-on (lpsg glop cl-opengl)
  :components
  ((:module "examples"
    :components
    ((:file "package")
     (:file "viewer" :depends-on ("package"))
     (:file "cube" :depends-on ("package" "viewer"))
     (:file "texture" :depends-on ("package" "viewer"))))))
