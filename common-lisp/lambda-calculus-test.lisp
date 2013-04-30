(defpackage #:lambda-calculus-test
  (:use #:cl #:lisp-unit #:lc
        #:lambda-calculus-test-helpers))

(in-package #:lambda-calculus-test)


;;; Church booleans.

(define-test boolification
  (assert-eq #'true
             (boolify t))
  (assert-eq #'false
             (boolify nil))
  (assert-eq t
             (unboolify #'true))
  (assert-eq nil
             (unboolify #'false)))

(define-truth-table-test not
  ((t   nil)
   (nil t)))

(define-truth-table-test and
  ((nil nil nil)
   (nil nil t)
   (nil t   nil)
   (t   t   t)))

(define-truth-table-test or
  ((nil nil nil)
   (t   nil t)
   (t   t   nil)
   (t   t   t)))

(define-truth-table-test xor
  ((nil nil nil)
   (t   nil t)
   (t   t   nil)
   (nil t   t)))
