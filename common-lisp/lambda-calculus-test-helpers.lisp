(defpackage #:lambda-calculus-test-helpers
  (:use #:cl #:lisp-unit #:lc)
  (:export #:symb
           #:define-truth-table-test))

(in-package #:lambda-calculus-test-helpers)


;;; Symbol manipulation.

(defun symb (&rest pieces)
  (values (intern (string-upcase (apply #'concatenate
                                        'string
                                        pieces)))))

(define-test symb
  (assert-eq '||
             (symb))
  (assert-eq '!
             (symb "!"))
  (assert-eq 'this-is-a-test
             (symb "this-" "is-" "a-" "test")))

;;; Truth table test generation.

(defun generate-truth-table-assertion (f-name value)
  `(assert-eq ,(car value)
              (unboolify (,(symb "lambda-" f-name)
                           ,@(mapcar (lambda (x) `(boolify ,x))
                                     (cdr value))))))

(defun generate-truth-table-assertions (f-name f-values)
  (mapcar (lambda (value)
            (generate-truth-table-assertion f-name value))
          f-values))

(defmacro define-truth-table-test (f-name f-values)
  (let ((f-name (symbol-name f-name)))
    `(define-test ,(symb "lambda-" f-name)
       ,@(generate-truth-table-assertions f-name f-values))))
