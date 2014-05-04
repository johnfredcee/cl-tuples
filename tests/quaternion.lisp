(in-package :cl-tuples-tests)

(in-suite cl-tuples-tests)

(def-test angle-axis-matrix33* ()
  (is (equalp #(0.08060455
                -0.38177323
                1.3011687
                1.3011687
                0.08060455
                -0.38177323
                -0.38177323
                1.3011687
                0.08060455)
              (make-matrix33*
               (angle-axis-matrix33*
                (angle-axis-values*
                 1.0 1.0 1.0 1.0))))))
