;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;; McClim's DEFINE-PROTOCOL-CLASS macro, changed around to define generic functions instead of
;;; slot definitions.

(in-package #:lpsg)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-accessor (class-name gf-form)
    (let ((reader-gf nil)
          (writer-gf nil)
          (gf-option (car gf-form)))
      (destructuring-bind (name &key (lambda-list nil lambda-list-p)
                                (documentation nil documentationp))
          (cdr gf-form)
        (let ((doc-list (and documentationp `((:documentation (,documentation))))))
          (when (or (eq gf-option :reader) (eq gf-option :accessor)))
          (let ((ll (if lambda-list-p
                        lambda-list
                        `(,class-name))))
            (setf reader-gf `((defgeneric ,name ,ll ,@doc-list))))
          (when (or (eq gf-option :writer) (eq gf-option :accessor))
            (let ((ll (if lambda-list-p
                          `(new-val ,@lambda-list)
                          `(new-val ,class-name))))
              (setf writer-gf `((defgeneric (setf ,name) ,ll ,@doc-list)))))))
      `(,@reader-gf ,@writer-gf))))


(defmacro define-protocol-class (name super-classes generic-functions &rest options)
  (let* ((sym-name (symbol-name name))
	 (protocol-predicate
	  (intern (concatenate 'string
			       sym-name
			       (if (find #\- sym-name) "-" "")
			       (symbol-name '#:p))))
	 (predicate-docstring
	  (concatenate 'string
		       "Protocol predicate checking for class " sym-name))
         (gf-forms
          (loop
             for gf in generic-functions
             append (case (car gf)
                      (:generic
                       `((defgeneric ,@(cdr gf))))
                      ((:accessor :reader :writer)
                       (make-accessor name gf))
                      (t (error "Unrecognized option ~S." (car gf)))))))
    `(progn
       (defclass ,name ,super-classes () ,@options)

       (let ((the-class (find-class ',name)))
         (defmethod initialize-instance :after ((object ,name) &key &allow-other-keys)
           (when (eq (class-of object) the-class)
             (error "~S is a protocol class and thus can't be instantiated" ',name))))

       (defgeneric ,protocol-predicate (object)
	 (:method ((object t))
	   nil)
	 (:method ((object ,name))
	   t)
	 (:documentation ,predicate-docstring))

       ,@gf-forms
       ',name)))
