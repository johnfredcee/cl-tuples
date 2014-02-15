(in-package :cl-tuples-system)

(defsystem :cl-tuples-tests
  :description "Tests for cl-tuples"
  :serial t
  :author "John Connors"
  :version "1.0"
  :licence "MIT"
  :depends-on (:cl-tuples :alexandria :fiveam)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "suite")
                             (:file "quaternion")))))
