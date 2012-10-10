;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(defsystem lpsg-examples
  :description "OpenGL rendering pipeline examples"
  :depends-on (lpsg cl-opengl cl-glu cl-glut)
  :components
  ((:module "examples"
    :components
    ((:file "package")
     (:file "rotator" :depends-on ("package"))))))
