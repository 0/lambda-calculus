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

    > (\x.x x) id
    id
    > (\x y.y x) true id
    true
    > not true
    false
    > =apply.\f x.f x
    apply
    > apply not true
    false
    > (\p q r.p q r) id id
    \r.((p q) r) [p,q]
    > (\p q r.p q r) id id id
    id
    > (\p q r.z) id id
    \r.z [p,q]
    > (\p q r.z) id id id
    Undefined variable: z

Tested with [CPython](http://python.org/) 3.3.1. Significantly improved by [rlwrap](http://utopia.knoware.nl/~hlub/rlwrap/).
