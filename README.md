# lambda-calculus

Toy [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) implementations. Why should Turing machines get all the glory?

## Common Lisp

Just Church booleans implemented directly using CL functions. The most interesting part is the overkill testing system.

To run in the REPL:

    CL-USER(1): (asdf:load-system :lambda-calculus)
    CL-USER(2): (in-package :lc)
    LC(3): (unboolify (lambda-and #'true #'false))
    NIL
    LC(4): (unboolify (lambda-and #'true #'true))
    T

To also run the tests:

    LC(5): (in-package :lambda-calculus-test)
    LAMBDA-CALCULUS-TEST(6): (run-tests)

Tested with [SBCL](http://www.sbcl.org/) 1.1.4.

## Python

A full REPL that parses the usual syntax, with a few whistles (bells not yet implemented):

    > (\x.x x) id                                 # Variables, abstraction, application.
    id
    > (\x y.y x) true id                          # Currying for multiple arguments.
    true
    > not true                                    # Built-in named functions.
    false
    > =apply.\f x.f x                             # Ugly syntax for defining global names.
    apply
    >                                             # Comments, if you haven't noticed.
    > (\p q r.p q r) id id                        # Obscure format for printing closures.
    \r.((p q) r) [p q]
    > (\p q r.p q r) id id id                     # Left-associative application.
    id
    > (\p q r.z) id id                            # Unevaluated closure contents.
    \r.z [p q]
    > (\p q r.z) id id id                         # Some error handling.
    Undefined variable: z
    > false (id z) true                           # Lazy evaluation.
    true
    >                                             # Linked lists.
    > =lst.cons true (cons false (cons apply empty))
    lst
    > last lst                                    # Handy list operations.
    apply
    > ,last                                       # Value inspection, named recursion.
    \x.(((empty? (tail x)) (head x)) (last (tail x))) []
    last
    >                                             # Anonymous recursion.
    > fix (\f x.(empty? (tail x)) (head x) (f (tail x))) lst
    apply

Tested with [CPython](http://python.org/) 3.3.2. Significantly improved by [rlwrap](http://utopia.knoware.nl/~hlub/rlwrap/).
