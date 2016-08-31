;;;; Utility functions to create assemblies of shapes

(in-package #:lpsg-scene)

(defun rref* (array &rest indices)
  (declare (dynamic-extent indices))
  "Return a row (last rank) of @cl:param(array) as 3 values. @c(rref*) is a place for @c(setf)."
  (case (array-rank array)
    (1
     (values (aref array 0)
             (aref array 1)
             (aref array 2)))
    (2
     (let ((i (car indices)))
       (values (aref array i 0)
               (aref array i 1)
               (aref array i 2))))
    (3
     (destructuring-bind (i j)
         indices
       (values (aref array i j 0)
               (aref array i j 1)
               (aref array i j 2))))
    (otherwise
     (values (apply #'aref array (append indices '(0)))
             (apply #'aref array (append indices '(1)))
             (apply #'aref array (append indices '(2)))))))

(defun rref2* (array &rest indices)
  (declare (dynamic-extent indices))
  "Return a row (last rank) of @cl:param(array) as 2 values. @c(rref*) is a place for @c(setf)."
  (case (array-rank array)
    (1
     (values (aref array 0)
             (aref array 1)))
    (2
     (let ((i (car indices)))
       (values (aref array i 0)
               (aref array i 1))))
    (3
     (destructuring-bind (i j)
         indices
       (values (aref array i j 0)
               (aref array i j 1))))
    (otherwise
     (values (apply #'aref array (append indices '(0)))
             (apply #'aref array (append indices '(1)))))))

(defun rref4* (array &rest indices)
  (declare (dynamic-extent indices))
  "Return a row (last rank) of @cl:param(array) as 4 values. @c(rref*) is a place for @c(setf)."
  (case (array-rank array)
    (1
     (values (aref array 0)
             (aref array 1)
             (aref array 2)
             (aref array 3)))
    (2
     (let ((i (car indices)))
       (values (aref array i 0)
               (aref array i 1)
               (aref array i 2)
               (aref array i 3))))
    (3
     (destructuring-bind (i j)
         indices
       (values (aref array i j 0)
               (aref array i j 1)
               (aref array i j 2)
               (aref array i j 3))))
    (otherwise
     (values (apply #'aref array (append indices '(0)))
             (apply #'aref array (append indices '(1)))
             (apply #'aref array (append indices '(2)))
             (apply #'aref array (append indices '(3)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-rref*-macro (num-elements array indices)
    (let* ((index-temps (mapcar (lambda (sym)
                                  (declare (ignore sym))
                                  (gensym))
                                indices))
           (index-binding (mapcar #'list index-temps indices)))
      (alexandria:once-only (array)
        (loop
           for i from 0 below num-elements
           collect `(aref ,array ,@index-temps ,i) into values
           finally
             (return `(let ,index-binding
                        (values ,@values))))))))

(define-compiler-macro rref* (array &rest indices)
  (make-rref*-macro 3 array indices))

(define-compiler-macro rref2* (array &rest indices)
  (make-rref*-macro 2 array indices))

(define-compiler-macro rref4* (array &rest indices)
  (make-rref*-macro 4 array indices))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-rref*-expander (accessor num-elements array indices)
    (let* ((array-var (gensym "ARRAY"))
           (index-vars (mapcar (lambda (index)
                                 (declare (ignore index))
                                 (gensym "INDEX"))
                               indices))
           (store-vars (loop
                          for i from 0 below num-elements
                          collect (gensym "STORE")))
           (setf-pairs (loop
                          for store-var in store-vars
                          for elem from 0
                          append `((aref ,array-var ,@index-vars ,elem) ,store-var))))
      (values `(,array-var ,@index-vars)
              `(,array ,@indices)
              store-vars
              `(progn
                 (setf ,@setf-pairs)
                 (values ,@store-vars))
              `(,accessor ,array-var ,@index-vars))))
  )

(define-setf-expander rref* (array &rest indices)
  (make-rref*-expander 'rref* 3 array indices))

(define-setf-expander rref2* (array &rest indices)
  (make-rref*-expander 'rref2* 2 array indices))

(define-setf-expander rref4* (array &rest indices)
  (make-rref*-expander 'rref4* 2 array indices))


(defparameter *cube-verts*
  (make-array '(8 3)
              :initial-contents '((0.5 -0.5 -0.5)
                                  (0.5 0.5 -0.5)
                                  (-0.5 0.5 -0.5)
                                  (-0.5 -0.5 -0.5)
                                  (0.5 -0.5 0.5)
                                  (0.5 0.5 0.5)
                                  (-0.5 0.5 0.5)
                                  (-0.5 -0.5 0.5))
              :element-type 'single-float))

;;; Indices of the squares that make up the faces. These will be rendered as triangles.

(defparameter *cube-faces*
  (make-array '(6 4)
              :initial-contents
              '((0 3 2 1)               ;bottom
                (4 5 6 7)               ;top
                (0 4 7 3)               ;front
                (1 2 6 5)               ;back
                (0 1 5 4)               ;right
                (3 7 6 2))))            ;left

(defun make-cube-shape ()
  "Create a cube shape.

The cube has VERTEX and NORMAL vertex attributes. The resulting shape has an indexed drawable."
  (let ((vertex-array (make-array '(24 3) :element-type 'single-float))
        (normal-array (make-array '(24 3) :element-type 'single-float))
        (element-array (make-array 36)) ; 6 vertices per face
        ;; Scratch vectors for normal computation
        (v1 (alloc-vec))
        (v2 (alloc-vec)))
    (loop
       for i from 0 below 6
       ;; for each face, construct a surface normal
       for idx0 = (aref *cube-faces* i 0)
       for idx1 = (aref *cube-faces* i 1)
       for idx2 = (aref *cube-faces* i 3)
       do (progn
            (loop
               for j from 0 below 3
               do (progn
                    (setf (aref v1 j)
                          (- (aref *cube-verts* idx1 j) (aref *cube-verts* idx0 j)))
                    (setf (aref v2 j)
                          (- (aref *cube-verts* idx2 j) (aref *cube-verts* idx0 j)))))
            (let ((normal (cross-product v1 v2)))
              ;; Copy the geometry...
              (loop
                 for k from 0 below 4
                 for vidx = (+ (* i 4) k)
                 do 
                   (setf (rref* vertex-array vidx) (rref* *cube-verts* (aref *cube-faces* i k)))
                   (setf (rref* normal-array vidx) (rref* normal)))
              ;; ... and now the indices for the triangles. For each face, we want a
              ;; pattern of [0 1 2 2 3 0]. It would be easier just to specify that!
              (loop
                 for k from 0 below 3
                 do (setf (aref element-array (+ (* i 6) k)) (+ (* i 4) k)))
              (loop
                 for k from 3 below 6
                 do (setf (aref element-array (+ (* i 6) k)) (+ (* i 4) (mod (1- k) 4)))))))
    ;; Now we can construct the vertex attributes and the shape
    (let* ((vertex-attr (make-instance 'vertex-attribute
                                      :data vertex-array :data-count 24
                                      :components 3 :buffer-type :float))
           (normal-attr (make-instance 'vertex-attribute
                                       :data normal-array :data-count 24
                                       :components 3 :buffer-type :float))
           (element-attr (make-instance 'mirrored-buffer-resource
                                        :data element-array :data-count 36
                                        :components 1 :buffer-type :short))
           (cube-shape (make-instance 'standard-shape
                                      :drawable (make-instance 'indexed-drawable
                                                               :mode :triangles
                                                               :vertex-count 36
                                                               :element-array element-attr))))
      (setf (attribute cube-shape 'vertex) vertex-attr)
      (setf (attribute cube-shape 'normal) normal-attr)
      cube-shape)))

