;;;; Silly emacs, this is -*- Lisp -*-

(defpackage :cl-tuples-system
  (:use :cl :asdf))

(in-package :cl-tuples-system)

(defsystem :cl-tuples
  :name "cl-tuples"
  :author "John Connors"
  :version "1.0"
  :licence "MIT"
  :description "Experimental Tuple Types Facade"
  :depends-on (:iterate :alexandria)
  :in-order-to ((test-op (load-op :cl-tuples-tests)))
  :perform (test-op :after (op c)
             (funcall (find-symbol (symbol-name :run!) :fiveam)
                      (find-symbol (symbol-name :cl-tuples-tests) :cl-tuples-tests)))
  :serial t
  :components ((:file "package")
			   (:file "utils")
			   (:file "symbols")
			   (:file "syntax")
			   (:file "tuple-expander")
			   (:file "tuples")
			   (:file "vector")
			   (:file "matrix")
			   (:file "quaternion")
			   (:file "colour")
			   (:file "triangle")
			   (:file "rect")
			   (:file "aabb")))
