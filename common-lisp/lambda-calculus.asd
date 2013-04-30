(asdf:defsystem #:lambda-calculus
  :depends-on (#:lisp-unit)
  :components ((:file "lambda-calculus")
               (:file "lambda-calculus-test-helpers")
               (:file "lambda-calculus-test" :depends-on ("lambda-calculus" "lambda-calculus-test-helpers"))))
