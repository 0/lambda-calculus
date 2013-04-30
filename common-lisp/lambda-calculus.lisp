(defpackage #:lambda-calculus
  (:nicknames #:lc)
  (:use #:cl)
  (:export #:true
           #:false
           #:lambda-not
           #:lambda-and
           #:lambda-or
           #:lambda-xor
           #:boolify
           #:unboolify))

(in-package #:lambda-calculus)


;;; Church booleans.

(defun true (a b)
  (declare (ignore b))
  a)

(defun false (a b)
  (declare (ignore a))
  b)

;; Boolean operations.

(defun lambda-not (p)
  (lambda (a b)
    (funcall p b a)))

(defun lambda-and (p q)
  (funcall p q p))

(defun lambda-or (p q)
  (funcall p p q))

(defun lambda-xor (p q)
  (funcall p (lambda-not q) q))

;; Conversion functions.

(defun boolify (p)
  (if p
    #'true
    #'false))

(defun unboolify (p)
  (funcall p t nil))
