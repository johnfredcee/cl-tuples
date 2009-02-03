
(in-package :cl-tuples)

(def-tuple-type quaternion
    :tuple-element-type fast-float
	:initial-element 0.0f0
    :elements (x y z w))

(export-tuple-operations quaternion)

(def-tuple-type angle-axis
    :tuple-element-type fast-float
	:initial-element 0.0f0
    :elements (x y z a))

(export-tuple-operations angle-axis)

;; need conjugate, angle-axis conversion, slerp

(def-tuple-op quaternion-conjugate 
  ((q quaternion (x y z w)))
  (quaternion* (- x) (- y) (- z) w))

(def-tuple-op quaternion-scale 
    ((q quaternion (x y z w))
     (s single-float))
  "Multiply a quat by a scalar"
  (:return quaternion
   (quaternion*
    (* s x) (* s y) (* s z) (* s  w))))

(def-tuple-op quaternion-norm
 ((q quaternion (x y z w)))
  (:return single-float
           (sqrt (+ (* x x) (* y y) (* z z) (* w w)))))
           


(def-tuple-op quaternion-unitize 
  ((q quaternion (x y z w)))
  "Ensure a quaternion is a unit"
  (:return quaternion             
           (let ((1/mag
                  (the single-float
                    (/ 1.0 (sqrt (+ (* x x) (* y y) (* z z) (* w w)))))))
             (quaternion* (* x 1/mag) (* y 1/mag) (* z 1/mag) (* w 1/mag)))))

(def-tuple-op quaternion-sum
  ((q0 quaternion (x0 y0 z0 w0))
   (q1 quaternion (x1 y1 z1 w1)))  
  "Sum the components of two quaternions"
  (:return quaternion
           (quaternion* (+ x0 x1) (+ y0 y1) (+ z0 z1) (+ w0 w1))))

(def-tuple-op quaternion-dot 
    ((quaternion-lhs quaternion (x0 y0 z0 w0)) 
     (quaternion-rhs quaternion (x1 y1 z1 w1)))
    "Dot product of two quaternions."
    (:return single-float
             (+ (* x0 x1) (* y0 y1) (* z0 z1) (* w0 w1))))

(def-tuple-op quaternion-inverse 
  ((q quarternion (x y z w)))
    "Inverse of quaternion"
    (:return quaternion     
             (let* ((mag 
                     (the single-float 
                       (sqrt (+ (* x x) (* y y) (* z z) (* w w)))))
                    (1/mag2  
                     (the single-float (/ 1.0 (* mag mag)))))
                    (declare (single-float mag2))
                    (quaternion* (/ x mag2) (/ y mag2) (/ z mag2) (/ w mag2)))))
               
                       
(def-tuple-op quaternion-product
    ((q-lhs quaternion (x1 y1 z1 w1))
     (q-rhs quaternion (x2 y2 z2 w2)))
  "Multiple of two quaternions"
  (:return quaternion
           (quaternion* (- (+ (* w1 x2) (* x1 w2) (* y1 z2)) (* z1 y2))
                        (- (+ (* w1 y2) (* y1 w2) (* z1 x2)) (* x1 z2))
                        (- (+ (* w1 z2) (* x1 y2) (* z1 w2)) (* y1 x2))
                        (- (* w1 w2) (* x1 x2) (* y1 y2) (* z1 z2)))))

(def-tuple-op quaternion-matrix33 
  ((q quaternion (x y z w)))
   "Convert a quaternion to a 3x3 rotation matrix."
   (:return matrix33
            (matrix33*
             (- 1 (* 2 y y) (* 2 z z))   (- (* 2 x y) (* 2 w z))   (+ (* 2 x z) (* 2 w y))
             (+   (* 2 x y) (* 2 w z))   (- 1 (* 2 x x) (* 2 z z)) (- (* 2 y z) (* 2 w z))
             (-   (* 2 x z) (* 2 w y))   (+  (* 2 y z) (* 2 w z))  (- 1 (* 2 x x) (* 2 y y)))))
   

(def-tuple-op angle-axis-quaternion
  ((aa angle-axis (x y z a)))
  "Convert an angle-axis tuple to a quaternion tuple"
  (:return quaternion
           (quaternion* (* x (sin (* 0.5 a))) 
                        (* y (sin (* 0.5 a))) 
                        (* z (sin (* 0.5 a))) 
                        (cos (* 0.5 a)))))
  
                
    
(def-tuple-op quaternion-transform-vector3d
    ((vector vector3d (vx vy vz))
     (quat quaternion (qx qy qz qw)))
  "Transform a 3d vector with a quaternion"
  (:return vector3d
           (with-quaternion 
               (quaternion-product
                (quaternion-product
                 (quaternion* qx qy qz qw)
                 (quaternion* vx vy vz 0.0))
                (quaternion-conjugate (quaternion* qx qy qz qw)))
               (rx ry rz rw)
             (vector3d* rx ry rz))))


(def-tuple-op vector3d-quaternion 
    ((vector vector3d (vx vy vz)))
  "Convert a 3d vector into q auqt for angular velocity purposes"
  (:return quaternion
           (quaternion* vx vy vz 0.0)))


     
             