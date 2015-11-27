;;;; Utility functions to create assemblies of shapes

(in-package #:lpsg)

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

(defun copy-vec3-row (dest dest-row src src-row)
  (loop
     for i from 0 below 3
     do (setf (aref dest dest-row i) (aref src src-row i))))

(defun copy-vec3-to-row (dest dest-row src)
  (loop
     for i from 0 below 3
     do (setf (aref dest dest-row i) (aref src i))))

(defun make-cube-shape ()
  (let ((vertex-array (make-array '(24 3) :element-type 'single-float))
        (normal-array (make-array '(24 3) :element-type 'single-float))
        (element-array (make-array 36)) ; 6 vertices per face
        ;; Scratch vectors for normal computation
        (v1 (make-array 3 :element-type 'single-float))
        (v2 (make-array 3 :element-type 'single-float)))
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
            (let ((normal (cross v1 v2))
                  (vert-base-idx (* i 4 3)))
              ;; Copy the geometry...
              (loop
                 for k from 0 below 4
                 do (progn
                      (copy-vec3-row vertex-array (+ (* i 4) k)
                                     *cube-verts* (aref *cube-faces* i k))
                      (copy-vec3-to-row normal-array (+ (* i 4) k) normal)))
              ;; ... and now the indices for the triangles.
              (loop
                 for k from 0 below 3
                 do (setf (aref element-array (+ (* i 6) k)) (+ (* i 4) k)))
              (loop
                 for k from 3 below 6
                 do (setf (aref element-array (+ (* i 6) k)) (+ (* i 4) (mod k 4)))))))
    ;; Now we can construct the vertex attributes and the shape
    (let* ((vertex-attr (make-instance 'vertex-attribute
                                      :data vertex-array :data-count 24
                                      :components 3 :buffer-type :float))
           (normal-attr (make-instance 'vertex-attribute
                                       :data normal-array :data-size 24
                                       :components 3 :buffer-type :float))
           (element-attr (make-instance 'mirrored-resource
                                        :data element-array :data-size 36
                                        :components 1 :buffer-type :short))
           (cube-shape (make-instance 'shape
                                      :drawable (make-instance 'indexed-drawable
                                                               :mode :triangles
                                                               :vertex-count 36
                                                               :element-array element-attr))))
      (setf (attribute cube-shape 'vertex) vertex-attr)
      (setf (attribute cube-shape 'normal) normal-attr)
      cube-shape)))

