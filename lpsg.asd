;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem lpsg
  :description "OpenGL rendering pipeline."
  :depends-on (cl-opengl)
  :components
  ((:module "lpsg"
    :components
    ((:file "package")
     (:file "utils" :depends-on ("package"))
     (:file "mutable" :depends-on ("package"))
     (:file "assembly" :depends-on ("package"))
     (:file "uset" :depends-on ("package" "mutable"))
     (:file "incremental-comp" :depends-on ("package"))
     (:file "render" :depends-on ("package" "mutable" "uset" "assembly"))))))
