(in-package :cl-tuples-tests)

(in-suite cl-tuples-tests)

(file-enable-tuples-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-symbol "QUAD" (FIND-PACKAGE "TUPLE-TYPES")))
  (cl-tuples::make-tuple-symbol 'quad 'fixnum 0 '(a b  c d)))

(cl-tuples::def-tuple quad)
(cl-tuples::def-tuple-struct quad)
(cl-tuples::def-tuple-maker  quad)
(cl-tuples::def-tuple-setter quad)
(cl-tuples::def-tuple-getter quad)
(cl-tuples::def-tuple-set quad)
(cl-tuples::def-new-tuple quad)
(cl-tuples::def-tuple-maker* quad)

(defparameter *quad* (new-quad))

(def-test test-tuple-primitives ()
  (is (equalp (multiple-value-list (quad-values* 8 4 3 1)) '(8 4 3 1)))
  (let ((my-quad (make-quad 3 7 5 9)))
    (is (equalp (multiple-value-list (quad* my-quad)) '(3 7 5 9)))
    (is (equalp my-quad #(3 7 5 9)))
    (is (set-quad my-quad 5 1 2 3))
    (is (equalp my-quad #(5 1 2 3)))
    (quad-setter* my-quad #{9 10 7 6})
    (is (equalp my-quad #(9 10 7 6))))
  (let ((fresh-quad (new-quad))
        (another-quad (make-quad 5 6 10 11)))
    (is (equalp fresh-quad #(0 0 0 0)))
    (is (equalp another-quad  #(5 6 10 11)))
    (is (equalp (make-quad* #{ 5 2 9 12 }) #(5 2 9 12)))))

(cl-tuples::def-tuple-array-maker quad)
(cl-tuples::def-tuple-aref* quad)
(cl-tuples::def-tuple-aref  quad)
(cl-tuples::def-tuple-aref-setter* quad)
(cl-tuples::def-tuple-aref-setter quad)
(cl-tuples::def-tuple-array-dimensions quad)
(cl-tuples::def-tuple-vector-push quad)
(cl-tuples::def-tuple-vector-push-extend quad)
(cl-tuples::def-tuple-vector-push* quad)
(cl-tuples::def-tuple-vector-push-extend* quad)

(defparameter *quads* (make-quad-array 3 :initial-element 0 :adjustable t :fill-pointer 2))

(def-test test-tuple-arrays ()
  (is (equalp (multiple-value-list (quad-aref-setter* *quads* 1 #[ quad* 4 5 6 19 ])) '( 4 5 6 19)))
  (is (equalp (multiple-value-list (quad-aref* *quads* 1)) '(4 5 6 19)))
  (is (equalp (quad-aref  *quads* 1) #[ quad 4 5 6 19]))
  (is (equalp (quad-aref-setter *quads* 1 #(2 4 3 9))  #[ quad 2 4 3 9 ]))
  (is (equalp (multiple-value-list (quad-aref* *quads* 1)) '(2 4 3 9)))
  (is (= (quad-array-dimensions *quads*) 2))
  (let
      ;; array extension
      ((new-quads (make-quad-array 4 :initial-element 0 :adjustable t :fill-pointer 2)))
    (is (= (quad-vector-push  #[ quad 8 9 22 34 ] new-quads) 3))
    (is (equalp (quad-aref new-quads 2) #[ quad 8 9 22 34 ]))
    (is (= (quad-vector-push-extend #[ quad 27 28 29 34 ] new-quads) 4))
    (is (equalp (quad-aref new-quads 3) #[ quad 27 28 29 34 ])))
  (let
      ;; array extension
      ((new-quads (make-quad-array 4 :initial-element 0 :adjustable t :fill-pointer 2)))
    (is (= (quad-vector-push*  #[ quad* 8 9 22 34 ] new-quads) 3))
    (is (equalp (quad-aref new-quads 2) #[ quad 8 9 22 34] ))
    (is (= (quad-vector-push-extend* #[ quad* 27 28 29 34 ] new-quads) 4))
    (is (equalp (quad-aref new-quads 3) #[ quad 27 28 29 34]))))

(cl-tuples::def-with-tuple quad)
(cl-tuples::def-with-tuple* quad)
(cl-tuples::def-with-tuple-aref quad)

(def-test test-tuple-macros ()
  (let ((my-quad (make-quad 9 10 7 6)))
    (with-quad my-quad (e1 e2 e3 e4)
      (is (equalp (list e1 e2 e3 e4) '(9 10 7 6)))))
  (let ((my-quad (make-quad 3 1 4 5)))
    (with-quad* (quad* my-quad) (e1 e2 e3 e4)
      (is (equalp (list e1 e2 e3 e4) '(3 1 4 5)))))
  (with-quad-aref (*quads* 1 (el1 el2 el3 el4))
    (is (equalp (vector el1 el2 el3 el4) (quad-aref *quads* 1)))))

;; generalised reference ?

(cl-tuples::def-tuple-setf*  quad)
(cl-tuples::def-tuple-array-setf*  quad)
(cl-tuples::def-tuple-array-setf quad)

(def-test test-tuple-setf ()
  (let ((test-quad (new-quad))
        (test-quads (make-quad-array 9)))
    (is (equalp (multiple-value-list (setf (quad* test-quad)  #[ quad* -1 -2 -3 -4])) '( -1 -2 -3 -4)))
    (is (equalp test-quad #(-1 -2 -3 -4)))
    (is (equalp (multiple-value-list (setf (quad-aref* test-quads 1) #[ quad* -4 -3 -2 -1])) '(-4 -3 -2 -1)))
    (is (equalp (setf (quad-aref test-quads 2)  #( -10 -11 -12 -13)) #[ quad -10 -11 -12 -13]))
    (is (equalp (quad-aref test-quads 2)  #[ quad -10 -11 -12 -13]))))

(def-tuple-type pair
  :tuple-element-type (unsigned-byte 8)
  :initial-element 0
  :elements (first second))

(defparameter *test-pair*
  (make-pair 1 2))

(defparameter *pair-array*
  (make-pair-array 2 :initial-element 0 :adjustable t :fill-pointer 1))

(def-test test-tuple-type ()
  (let ((*test-pair* (make-pair 1 2)))
    (is (equalp *test-pair* #[ pair 1 2 ]))
    (is (equalp (multiple-value-list (pair* *test-pair*)) '( 1 2 )))
    (is (equalp (multiple-value-list  (setf (pair* *test-pair*) #[ pair* 3 7])) '(3 7)))
    (is (equalp *test-pair* #[ pair 3 7 ]))))

;; test the vectors

(defparameter *v2d* (make-vector2d* #[ vector2d* 1.0 2.0 ]))
;; ;; basic vector math
(defparameter *vector0* (make-vector3d* #[ vector3d* 0.0 0.0 0.0 ] ))
(defparameter *vector1* (make-vector3d* #[ vector3d* 1.0 1.0 1.0 ] ))
(defparameter *vectorx* (make-vector3d* #[ vector3d* 1.0 0.0 0.0 ] ))
(defparameter *vectory* (make-vector3d* #[ vector3d* 0.0 1.0 0.0 ] ))
(defparameter *vectorz* (make-vector3d* #[ vector3d* 0.0 0.0 1.0 ] ))
(defparameter *test-vector* (new-vector3d))

(defun === (x y &optional (epsilon 0.00001))
  "Approx == for a pair or pair of lists of numbers"
  (flet ((compare (x y)
           (< (abs (- x y)) epsilon)))
    (cond
      ((and (numberp x) (numberp y))
       (compare x y))
      ((and (listp x) (listp y))
       (every #'identity (mapcar #'compare x y))))))

(def-test test-vectors ()
  (is (equalp (multiple-value-list
               (vector2d-scale* (vector2d* *v2d*) 0.5)) '( 0.5 1.0  )))
  (is (=== 0.0 (vector3d-length* (vector3d* *vector0*))))
  (is (=== (sqrt 3.0) (vector3d-length* (vector3d* *vector1*))))
  (is (equalp
       (multiple-value-list
        (vector3d-normal* (vector3d* *vector1*))) '(0.57735026 0.57735026 0.57735026)))
  (is (equalp
       (multiple-value-list
        (vector3d-cross* (vector3d* *vectorx*) (vector3d* *vectory*))) '(0.0 0.0 1.0)))
  (is (===
       (vector3d-dot* (vector3d* *vectorx*) (vector3d-normal* (vector3d* *vector1*)))
       0.57735026))
  (is (===
       (vector3d-length* (vector3d* *vector1*))
       (sqrt 3))))

(def-test test-matrices ()
  (flet ((torad (x) (coerce (* x (/ FAST-PI 180.0)) 'fast-float)))
    (let* ((rotatexccw (make-matrix44* (rotatex-matrix44* (torad 90.0))))
           (rotatexcw  (make-matrix44* (rotatex-matrix44* (torad -90.0))))
           (vector0 (make-vector3d 0.0 0.0 0.0))
           (vector1 (make-vector3d 1.0 1.0 1.0))
           (vertex0 (make-vertex3d* (vector3d-vertex3d* (vector3d*  vector0))))
           (vertex1 (make-vertex3d* (vector3d-vertex3d* (vector3d* vector1)))))
      (is (equalp
           (multiple-value-list
            (vector3d-difference*  (vector3d-values* 0.0 0.0 0.0) (vector3d-values* 1.0 1.0 1.0))) '(-1.0 -1.0 -1.0)))
      (is (=== (vertex3d-distance* (vertex3d* vertex0) (vertex3d* vertex1))
               1.7320508))
      (is (=== (let ((result (multiple-value-list
                              (transform-vertex3d*
                               (matrix44* rotatexccw)
                               (transform-vertex3d*
                                (matrix44* rotatexcw)
                                (vertex3d-values*  0.0 0.0 1.0 1.0))))))
                 (format t "~A~%" result)
                 result)
               '(0.0 0.0 1.0 1.0))))))
