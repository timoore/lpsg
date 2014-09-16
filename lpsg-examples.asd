;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(defsystem lpsg-examples
  :description "OpenGL rendering pipeline examples"
  :depends-on (lpsg glop cl-opengl cl-glu)
  :components
  ((:module "examples"
    :components
    ((:file "package")
     (:file "rotator-glop" :depends-on ("package"))))))
