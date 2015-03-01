;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(defsystem lpsg-examples
  :description "OpenGL rendering pipeline examples"
  :depends-on (lpsg glop cl-opengl)
  :components
  ((:module "examples"
    :components
    ((:file "package")
     (:file "cube" :depends-on ("package"))))))
