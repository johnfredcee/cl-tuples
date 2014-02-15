(defpackage :cl-tuples-tests
  (:use :cl-tuples :cl :fiveam))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :cl-tuples-debug *features*))
