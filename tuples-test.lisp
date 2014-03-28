
(defpackage :cl-tuples-test
  (:use :cl-tuples :cl)
  (:export "test-cl-tuples"))

(in-package :cl-tuples-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :cl-tuples-debug *features*))

(file-enable-tuples-syntax)

(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
	 (let ((*test-name* (append *test-name* (list ',name))))
	   ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
	 ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (cl-tuples::with-gensyms (result)
    `(let ((,result t))
	   ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
	   ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro with-test (test-sym test &rest forms)
  (cl-tuples::with-gensyms (result)
	`(progn
	   ,@forms
	   (let
		   ((,result ,test))
		 (assert ,result)
		 (setf ,test-sym (and ,test-sym ,result))))))

(defmacro always-pass (&body body)
  `(prog1
	   t
	 ,@body))

(eval-when (:compile-toplevel :load-toplevel)
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

(deftest test-tuple-primitives ()
  (check
	(equalp (multiple-value-list (quad-values* 8 4 3 1)) '(8 4 3 1)))
  (always-pass
	(let ((my-quad (make-quad 3 7 5 9)))
	  (check
		(equalp (multiple-value-list (quad* my-quad)) '(3 7 5 9))
		(equalp my-quad #(3 7 5 9))
		(set-quad my-quad 5 1 2 3))
	  (equalp my-quad #(5 1 2 3))
	  (quad-setter* my-quad #{9 10 7 6})
	  (equalp my-quad #(9 10 7 6)))
	(let ((fresh-quad (new-quad))
		  (another-quad (make-quad 5 6 10 11)))
	  (check 
		(equalp fresh-quad #(0 0 0 0))
		(equalp another-quad  #(5 6 10 11))
		(equalp (make-quad* #{ 5 2 9 12 }) #(5 2 9 12))))))			 



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

(deftest test-tuple-arrays ()
  (check 
	(equalp (multiple-value-list (quad-aref-setter* *quads* 1 #[ quad* 4 5 6 19 ])) '( 4 5 6 19))
	(equalp (multiple-value-list (quad-aref* *quads* 1)) '(4 5 6 19))
	(equalp (quad-aref  *quads* 1) #[ quad 4 5 6 19])
	(equalp (quad-aref-setter *quads* 1 #(2 4 3 9))  #[ quad 2 4 3 9 ]))
  (equalp (multiple-value-list (quad-aref* *quads* 1)) '(2 4 3 9))
  (= (quad-array-dimensions *quads*) 2)
  (always-pass
	(let
		;; array extension
		((new-quads (make-quad-array 4 :initial-element 0 :adjustable t :fill-pointer 2)))
	  (check
		(= (quad-vector-push  #[ quad 8 9 22 34 ] new-quads) 3)
		(equalp (quad-aref new-quads 2) #[ quad 8 9 22 34 ])
		(= (quad-vector-push-extend #[ quad 27 28 29 34 ] new-quads) 4)
		(equalp (quad-aref new-quads 3) #[ quad 27 28 29 34 ]))))
  (always-pass
	(let
		;; array extension
		((new-quads (make-quad-array 4 :initial-element 0 :adjustable t :fill-pointer 2)))
	  (check
		(= (quad-vector-push*  #[ quad* 8 9 22 34 ] new-quads) 3)
		(equalp (quad-aref new-quads 2) #[ quad 8 9 22 34] )
		(= (quad-vector-push-extend* #[ quad* 27 28 29 34 ] new-quads) 4)
		(equalp (quad-aref new-quads 3) #[ quad 27 28 29 34])))))


(cl-tuples::def-with-tuple quad)
(cl-tuples::def-with-tuple* quad)
(cl-tuples::def-with-tuple-aref quad)

(deftest test-tuple-macros ()
  (always-pass
	(let ((my-quad (make-quad 9 10 7 6)))
	  (with-quad my-quad (e1 e2 e3 e4) 
		(check (equalp (list e1 e2 e3 e4) '(9 10 7 6)))))
	(let ((my-quad (make-quad 3 1 4 5)))
	  (with-quad* (quad* my-quad) (e1 e2 e3 e4)
		(check (equalp (list e1 e2 e3 e4) '(3 1 4 5)))))
	(with-quad-aref (*quads* 1 (el1 el2 el3 el4)) 
	  (check (equalp (vector el1 el2 el3 el4) (quad-aref *quads* 1))))))

;; generalised reference ?

(cl-tuples::def-tuple-setf*  quad)   
(cl-tuples::def-tuple-array-setf*  quad)
(cl-tuples::def-tuple-array-setf quad)

(deftest test-tuple-setf ()
  (always-pass
	(let ((test-quad (new-quad))
		  (test-quads (make-quad-array 9)))
	  (check
		(equalp (multiple-value-list (setf (quad* test-quad)  #[ quad* -1 -2 -3 -4])) '( -1 -2 -3 -4))
		(equalp test-quad #(-1 -2 -3 -4))
		(equalp (multiple-value-list (setf (quad-aref* test-quads 1) #[ quad* -4 -3 -2 -1])) '(-4 -3 -2 -1))
		(equalp (setf (quad-aref test-quads 2)  #( -10 -11 -12 -13)) #[ quad -10 -11 -12 -13])
		(equalp (quad-aref test-quads 2)  #[ quad -10 -11 -12 -13])))))

(test-tuple-primitives)
(test-tuple-arrays)
(test-tuple-macros)
(test-tuple-setf)

(def-tuple-type pair
	:tuple-element-type (unsigned-byte 8)
	:initial-element 0
	:elements (first second))

(defparameter *test-pair* 
  (make-pair 1 2))

(defparameter *pair-array* 
  (make-pair-array 2 :initial-element 0 :adjustable t :fill-pointer 1))

(deftest test-tuple-type ()
  (check
	(equalp *test-pair* #[ pair 1 2 ])
	(equalp (multiple-value-list (pair* *test-pair*)) '( 1 2 ))
	(equalp (multiple-value-list  (setf (pair* *test-pair*) #[ pair* 3 7])) '(3 7))
	(equalp *test-pair* #[ pair 3 7 ])))

(test-tuple-type) 

;; test the vectors

(defparameter *v2d* (make-vector2d* #[ vector2d* 1.0f0 2.0f0 ]))
;; ;; basic vector math
(defparameter *vector0* (make-vector3d* #[ vector3d* 0.0f0 0.0f0 0.0f0 ] ))
(defparameter *vector1* (make-vector3d* #[ vector3d* 1.0f0 1.0f0 1.0f0 ] ))
(defparameter *vectorx* (make-vector3d* #[ vector3d* 1.0f0 0.0f0 0.0f0 ] ))
(defparameter *vectory* (make-vector3d* #[ vector3d* 0.0f0 1.0f0 0.0f0 ] ))
(defparameter *vectorz* (make-vector3d* #[ vector3d* 0.0f0 0.0f0 1.0f0 ] ))
(defparameter *test-vector* (new-vector3d))

(defun === (x y &optional (epsilon 1f-5))
  "Approx == for a pair or pair of lists of numbers"
  (flet ((compare (x y)
		   (< (abs (- x y)) epsilon)))
	(cond
	  ((and (numberp x) (numberp y))
	   (compare x y))
	  ((and (listp x) (listp y))
	   (every #'identity (mapcar #'compare x y))))))


(deftest test-vectors ()
  (check
   (equalp (multiple-value-list (cl-tuples::vector2d-scale* (vector2d* *v2d*) 0.5f0))
           '( 0.5f0 1.0f0 ))
   (=== 0.0f0 (vector3d-length* (vector3d* *vector0*)))
   (=== (sqrt 3.0f0) (vector3d-length* (vector3d* *vector1*)))
   (equalp (multiple-value-list (vector3d-normal* (vector3d* *vector1*)))
           '(0.57735026f0 0.57735026f0 0.57735026f0))
   (equalp (multiple-value-list (vector3d-cross* (vector3d* *vectorx*) (vector3d* *vectory*)))
           '(0.0f0 0.0f0 1.0f0))
   (=== (vector3d-dot* (vector3d* *vectorx*) (vector3d-normal* (vector3d* *vector1*)))
        0.57735026f0)
   (=== (vector3d-length* (vector3d* *vector1*))
        (sqrt 3))))


(test-vectors)


(deftest test-matrices ()
	 (flet ((torad (x) (coerce (* x (/ FAST-PI 180f0)) 'fast-float)))
	   (let* ((rotatexccw (make-matrix44* (rotatex-matrix44* (torad 90f0))))
			  (rotatexcw  (make-matrix44* (rotatex-matrix44* (torad -90f0))))		 
			  (vector0 (make-vector3d 0.0f0 0.0f0 0.0f0))
			  (vector1 (make-vector3d 1.0f0 1.0f0 1.0f0))
			  (vertex0 (make-vertex3d* (vector3d-vertex3d* (vector3d*  vector0))))
			  (vertex1 (make-vertex3d* (vector3d-vertex3d* (vector3d* vector1)))))
		 (check
		   (equalp 
			(multiple-value-list 
			 (vector3d-difference*
                          (vector3d-values* 0.0f0 0.0f0 0.0f0) (vector3d-values* 1.0f0 1.0f0 1.0f0)))
                        '(-1.0f0 -1.0f0 -1.0f0))
		   (=== (vertex3d-distance* (vertex3d* vertex0) (vertex3d* vertex1))
				1.7320508f0)
		   (=== (let ((result (multiple-value-list 
							   (transform-vertex3d* 
								(matrix44* rotatexccw)
								(transform-vertex3d* 
								 (matrix44* rotatexcw) 
								 (vertex3d-values*  0.0f0 0.0f0 1.0f0 1.0f0))))))
				  (format t "~A~%" result)
				  result)
				'(0.0f0 0.0f0 1.0f0 1.0f0))))))

;; (flet ((torad (x) (coerce (* x (/ FAST-PI 180f0)) 'fast-float)))
;;   (let* ((rotatexccw (make-matrix44* (rotatex-matrix44* (torad 90f0))))
;; 		 (rotatexcw  (make-matrix44* (rotatex-matrix44* (torad -90f0)))))		 
;; 		 (result (multiple-value-list 
;; 				  (transform-vertex3d* 
;; 				   (rotatex-matrix44* (matrix44* rotatexccw)) 
;; 				   (transform-vertex3d* 
;; 					(matrix44* rotatexcw) 
;; 					(vertex3d-values*  0.0f0 0.0f0 1.0f0 1.0f0))))))
;; 	(format t "~A~%" result)
;; 	result))

;; ;; check expander functions and with functions
;; (flet ((torad (x) (coerce (* x (/ FAST-PI 180f0)) 'fast-float)))
;;   (rotatex-matrix44* (the fast-float (torad 90f0))))
	 
(test-matrices)

(defun test-cl-tuples ()
  ;; going to have to rewrite the tests to stop depending on top-level vars..
  t)

;; ;; test identity mult

;; (defparameter *test-matrix* (make-matrix44 (cl-tuples::make-test-matrix44)))
;; (defparameter *identity-matrix* (make-matrix44 (identity-matrix44)))

;; (defparameter *vertex0* (make-vertex3d (vector3d-vertex3d (vector3d  *vector0*))))
;; (defparameter *vertex1* (make-vertex3d (vector3d-vertex3d (vector3d  *vector1*))))
;; (defparameter *vertexx* (make-vertex3d #{1.0f0 0.0f0 0.0f0 1.0f0}))
;; (defparameter *vertexy* (make-vertex3d #{0.0f0 1.0f0 0.0f0 1.0f0}))
;; (defparameter *vertexz* (make-vertex3d #{0.0f0 0.0f0 1.0f0 0.0f0}))

;; (with-test *result*
;;   (equalp *test-vector* #(1.0f0 1.0f0 1.0f0))
;;   (setf *test-vector* (make-vector3d (delta-vector3d  (vertex3d  *vertex0*)  (vertex3d *vertex1*)))))

;; (with-test *result*
;;   (= *result* 1.7320508f0)
;;   (setf *result*
;; 		(vertex3d-distance (vertex3d  *vertex0*) (vertex3d  *vertex1*))))


;; (defun torad (x) (coerce  (* x (/ PI 180f0)) 'single-float))

;; ;; basic matrix math
;; (defparameter *rotatex* (make-matrix44 (rotatex-matrix44 (torad 90))))
;; (defparameter *rotatey* (make-matrix44 (rotatey-matrix44 (torad 90))))
;; (defparameter *rotatez* (make-matrix44 (rotatez-matrix44 (torad 90))))

;; (defparameter *vertexx0* (make-vertex3d  (transform-vertex3d
;; 										  (matrix44  *rotatex*)
;; 										  (vertex3d  *vertexx*))))

;; (defparameter *vertexx1* (make-vertex3d
;; 						  (transform-vertex3d
;; 						   (matrix44  *rotatey*)
;; 						   (vertex3d  *vertexx0*))))

;; (defparameter *vertexx2* (make-vertex3d
;; 						  (transform-vertex3d
;; 						   (matrix44  *rotatez*)
;; 						   (vertex3d  *vertexx1*))))

;; (defparameter *concat-transform*
;;   (make-matrix44 (matrix44-product
;; 				  (matrix44 *rotatex*)
;; 				  (matrix44-product (matrix44  *rotatey*) (matrix44 *rotatez*)))))

;; (defparameter *vertexx3* (make-vertex3d
;; 						  (transform-vertex3d
;; 						   (matrix44 *concat-transform*)
;; 						   (vertex3d *vertexx0*))))


;; (defparameter *vector-array* (make-vector3d-array 2 :adjustable t :fill-pointer 1))

;; (setf (vector3d-aref *vector-array* 0) (vector3d *vectorx*))

;; ;; to do - should return size
;; (vector3d-vector-push  (vector3d  *vectory*) *vector-array*)

;; ;; to do - doesnt extend array properly
;; (vector3d-vector-push-extend (vector3d *vectorz*) *vector-array*)

;; ;; ;; iterate across array, apply transforms
;; (loop
;;    for i from 0 below (vector3d-array-dimensions *vector-array*)
;;    do
;; 	 (setf (vector3d-aref *vector-array* i)
;; 		   (cl-tuples::transform-vector3d
;; 			(matrix44 *concat-transform*)
;; 			(vector3d-aref *vector-array* i))))



;; ;; quick test case for clos wrapper

;; (def-tuple-class camera
;; 	(:tuples
;; 	 ((up :type cl-tuples::vector3d)
;; 	  (forward :type vector3d)
;; 	  (location :type vertex3d)
;; 	  (vertices :type vertex3d :array 5))
;; 	 :slots
;; 	 ((focal-length :type single-float :accessor focal-length-of)
;; 	  (id :allocation :class :reader id-of))))

;; ;;*test stanza*
;; (defparameter *test-camera* (make-instance 'camera))
;; (setf (up-of *test-camera*) #{ 0.0f0 0.0f0 0.0f0 })
;; (up-of *test-camera*)
;; (setf (up-of *test-camera*) #{ 1.0f0 2.0f0 3.0f0 })
;; (setf (vertices-of *test-camera* 3) #{ 2.0f0 3.0f0 -2.5f0 1.0f0 })
;; (vertices-of *test-camera* 3)
;; (vertices-of *test-camera* 4)
;; (vertices-of *test-camera* 1)

;; (defparameter *test-shape* (make-vector3d-array 4))

;; (setf (vector3d-aref *test-shape* 0) (vector3d* 3.14f0 0.0f0 3.14f0))
;; (setf (vector3d-aref *test-shape* 1) (vector3d* 3.14f0 0.0f0 -3.14f0))
;; (setf (vector3d-aref *test-shape* 2) (vector3d* -3.14f0 0.0f0 -3.14f0))
;; (setf (vector3d-aref *test-shape* 3) (vector3d* -3.14f0 0.0f0 3.14f0))


;; (defparameter *test-quaternion* (make-quaternion
;; 								 (angle-axis-quaternion
;; 								  (angle-axis* 0.0f0 1.0f0 0.0f0 (/ 3.14f0 2.0f0)))))


;; (defparameter *test-matrix*
;;   (make-matrix33
;;    (quaternion-matrix33 (quaternion *test-quaternion*))))


;; (loop
;;    for index from 0 below (vector3d-array-dimensions *test-shape*)
;;    do
;; 	 (setf (vector3d-aref *test-shape* index)
;; 		   (quaternion-transform-vector3d
;; 			(vector3d-aref *test-shape* index)
;; 			(quaternion *test-quaternion*))))

;; (loop
;;    for index from 0 below (vector3d-array-dimensions *test-shape*)
;;    do
;; 	 (setf (vector3d-aref *test-shape* index)
;; 		   (transform-vector3d
;; 			(matrix44-matrix33 (matrix44 *rotatey*))
;; 			(vector3d-aref *test-shape* index))))
