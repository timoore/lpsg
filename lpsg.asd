;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem lpsg
  :description "OpenGL rendering pipeline."
  :depends-on (cl-opengl)
  :components
  ((:module "lpsg"
    :components
    ((:file "package")
     (:file "interface" :depends-on ("package"))
     (:file "incremental-comp" :depends-on ("package"))
     (:file "utils" :depends-on ("package"))
     (:file "assembly" :depends-on ("package"))
     (:file "uset" :depends-on ("package"))
     (:file "render" :depends-on ("package" "uset" "assembly" "interface"))
     (:file "environment" :depends-on ("package" "render"))
     (:file "bundle" :depends-on ("package" "render" "environment"))
     (:file "effect" :depends-on ("package" "render" "environment" "bundle"))
     (:file "shape" :depends-on ("package" "render" "environment"))
     (:file "solids" :depends-on ("package" "render" "shape"))))))
