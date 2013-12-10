(in-package :cl-tuples)

;; float that fits within range of x86 hardware register minus tag (rather sbcl oriented)
(deftype fast-float () 
  #+sbcl `(single-float   (#.(- (expt 2f0 63))) (#.(expt 2f0 63)))
  #-sbcl 'single-float)

(defconstant fast-pi
  #.(coerce pi 'fast-float))

;; TO DO -- possibly can be replaced by alexandria::symbolicate -- must check
(defun make-adorned-symbol (name &key prefix suffix asterisk package)
  "Construct symbol for use as a function name, prefixed or suffixed
with some string, and optionally an added asterisk"
  (check-type name (or string symbol))
  (check-type prefix (or symbol string null))
  (check-type suffix (or symbol string null))
  (intern (concatenate 'string
                       (when prefix
                         (string prefix))
                       (when prefix  "-")
                       (string name)
                       (when suffix
                         "-")
                       (when suffix
                         (string suffix))
                       (when asterisk
                         (string "*")))
          (if package package *package*)))

(defun last-char (str)
  (char str (1- (length str))))

(defun symbol-to-string (sym)
  "If the argument is a symbol or string, return it as a string."
  (check-type sym (or symbol string))
  (cond
    ((symbolp sym)
     (symbol-name sym))
    ((stringp sym)
     sym)))

(defun is-asterisk-symbol (s)
  (let 
          ((ss (symbol-to-string s)))
        (eql (aref ss (1- (length ss))) #\*)))


(defmacro multiply-arguments (operator factor arguments)
  `(,operator ,@(mapcar (lambda (argument) `(* ,factor ,argument)) arguments)))

(defun matrix-symbol (i j &optional (prefix '#:e))
  "Construct a symbol that will represent an elemnet of a matrix."
  (find-symbol (format NIL "~A~D~D" prefix i j)))

(defun matrix-minor (x y length &optional (prefix '#:e))
  (let ((symbol-prefix (format NIL "~A~D~:*~D" '#:matrix (1- length))))
    `(,(find-symbol (concatenate 'string symbol-prefix #.(string '#:-determinant*)))
      (,(find-symbol (concatenate 'string symbol-prefix #.(string '#:-values*)))
       ,@(iterate values
           (for i from 1 to length)
           (iterate
             (for j from 1 to length)
             (unless (or (eql i x) (eql j y))
               (in values (collect (matrix-symbol (1- i) (1- j) prefix))))))))))

(defun matrix-cofactors (length)
  (iterate values
    (for i from 1 to length)
    (iterate
      (for j from 1 to length)
      (for value = (matrix-minor i j length))
      (in values
          (collect (if (oddp (+ i j))
                       `(- ,value)
                       value))))))
