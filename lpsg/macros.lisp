;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:lpsg)

(defmacro with-allocator ((allocator &rest allocator-args) &body body)
  (multiple-value-bind (forms declarations)
      (alexandria:parse-body body)
    `(let ((,allocator (make-instance ,@allocator-args)))
       ,@declarations
       (open-allocator ,allocator)
       (unwind-protect
            (progn
              ,@forms)
         (close-allocator ,allocator)))))

;; Hair for keeping the correct order of evaluation of macro keyword arguments.
(defun sort-by-keys (keyarg-list key-alist)
  (let ((new-alist (copy-list key-alist)))
    (loop
       for key in keyarg-list by #'cddr
       for key-entry = (assoc key new-alist)
       if key-entry
       collect key-entry into result
       and do (setf new-alist (delete key new-alist :key #'car))
       finally (return (nconc result new-alist)))))

(defun getassoc (item alist &key (default nil) (key #'identity) (test #'eql))
  (let ((cell (assoc item alist :key key :test test)))
    (if cell
        (values (cdr cell) t)
        (values default nil))))

(define-setf-expander getassoc (item alist &rest keyargs
                                &key (default nil) (key '#'identity) (test '#'eql)
                                &environment env)
  ;; Standard setf values for the alist place
  (multiple-value-bind (alist-dummies alist-vals alist-store alist-store-form alist-read-form)
      (get-setf-expansion alist env)
    (let* ((itemvar (gensym "ITEM"))
           (cellvar (gensym "CELL"))
           (defaultvar (gensym "DEFAULT"))
           (keyvar (gensym "KEY"))
           (testvar (gensym "TEST"))
           (storevar (gensym "STORE"))
           (key-alist `((:default ,defaultvar ,default)
                        (:key ,keyvar ,key)
                        (:test ,testvar ,test))))
      (setq key-alist (sort-by-keys keyargs key-alist))
      (values `(,itemvar ,@alist-dummies ,@(mapcar #'cadr key-alist))
              `(,item ,@alist-vals ,@(mapcar #'caddr key-alist))
              `(,storevar)
              `(let* ((,@alist-store ,alist-read-form)
                      (,cellvar (assoc ,itemvar ,@alist-store :key ,keyvar :test ,testvar)))
                 (if ,cellvar
                     (setf (cdr ,cellvar) ,storevar)
                     (progn
                       (setq ,@alist-store (acons ,itemvar ,storevar ,@alist-store))
                       ,alist-store-form))
                 ,storevar)
              `(getassoc ,itemvar ,alist-read-form ,@(mapcan (lambda (v)
                                                               `(,(car v) ,(cadr v)))
                                                             key-alist))))))
