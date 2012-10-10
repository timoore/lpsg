;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem lpsg
  :description "OpenGL rendering pipeline."
  :depends-on (cl-opengl)
  :components
  ((:module "lpsg"
    :components
    ((:file "package")
     (:file "render" :depends-on ("package"))))))
