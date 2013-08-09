#!/usr/bin/env python3

from nose.tools import assert_raises, eq_
from unittest import main, TestCase

from lambda_calculus import REPL, LambdaError


def make_repl():
	"""
	Make a REPL with some useful definitions for testing.
	"""

	r = REPL()

	r.run_lines([
		r'=a.(\x.y)',
		r'=b.(\x.y)',
		r'=c.(\x.y)',
		r'=d.(\x.y)',
		])

	return r


class VarTest(TestCase):
	def testUndefined(self):
		r = make_repl()

		with assert_raises(LambdaError):
			r.run_line(r'x')


class AbsTest(TestCase):
	def testMissingBits(self):
		r = make_repl()

		with assert_raises(LambdaError):
			r.run_line(r'\.')

		with assert_raises(LambdaError):
			r.run_line(r'\.x')

		with assert_raises(LambdaError):
			r.run_line(r'\x.')

		with assert_raises(LambdaError):
			r.run_line(r'\x y.')

	def testParams(self):
		r = make_repl()

		a = r.run_lines([
			r'(\x.x) a',
			r'(\one two.two) a b',
			r'(\one two three.three three three) a b id',
			])

		eq_(['a', 'b', 'id'], a)

	def testUncurrying(self):
		r = make_repl()

		a = r.run_lines([
			r'\x y.x',
			r'\x.\y.x',
			r'\a b c.\x y z.x',
			r'(\a b c.\x y z.x) id',
			r'(\a b c.\x y z.x) id id id id',
			])

		eq_([
			r'\x y.x []',
			r'\x y.x []',
			r'\a b c x y z.x []',
			r'\b c x y z.x [a]',
			r'\y z.x [a,b,c,x]',
			], a)


class AppTest(TestCase):
	def testApplications(self):
		r = make_repl()

		a = r.run_lines([
			r'id',
			r'id id',
			r'id id id',
			r'id id id id',
			r'true id false b',
			r'true (id false) b',
			r'true id (false b)',
			r'(\x y z.true y x) (id a) (true b c) (false b c)',
			])

		eq_(['id', 'id', 'id', 'id', 'b', 'false', 'id', 'b'], a)


class AssTest(TestCase):
	def testInvalidBits(self):
		r = make_repl()

		with assert_raises(LambdaError):
			r.run_line(r'=.')

		with assert_raises(LambdaError):
			r.run_line(r'=.x')

		with assert_raises(LambdaError):
			r.run_line(r'=x.')

		with assert_raises(LambdaError):
			r.run_line(r'=x y.id')

	def testAssignment(self):
		r = make_repl()

		with assert_raises(LambdaError):
			r.run_line(r'x')

		a = r.run_lines([
			r'=x.(\x.x)',
			r'x x x x x',
			r'x id'
			])

		eq_(['x', 'x', 'id'], a)


class SyntaxText(TestCase):
	def testComments(self):
		r = make_repl()

		a = r.run_lines([
			r'id # Just the function itself.',
			r'c#No spaces!',
			r'true a b # An application.',
			r'(false a) id (true (id b)) c # A more complex expression.',
			r'(\x.x) (true false false) # (\x.x) (false true true)',
			])

		eq_(['id', 'c', 'a', 'b', 'false'], a)

	def testMultiline(self):
		r = make_repl()

		a = r.run_lines([
			r'true' '\n' r'a' '\n' r'b',
			r'false # With a comment.' '\n' r'a b',
			])

		eq_(['a', 'b'], a)


class EvaluationTest(TestCase):
	def testLaziness(self):
		r = make_repl()

		with assert_raises(LambdaError):
			r.run_line(r'(id x)')

		a = r.run_lines([
			r'true a (id x)',
			])

		eq_(['a'], a)


class BooleanTest(TestCase):
	def testValues(self):
		r = make_repl()

		a = r.run_lines([
			r'true a b',
			r'false a b',
			])

		eq_(['a', 'b'], a)

	def testNot(self):
		r = make_repl()

		a = r.run_lines([
			r'not false',
			r'not true',
			])

		eq_(['true', 'false'], a)

	def testAnd(self):
		r = make_repl()

		a = r.run_lines([
			r'and false false',
			r'and false true',
			r'and true false',
			r'and true true',
			])

		eq_(['false', 'false', 'false', 'true'], a)

	def testOr(self):
		r = make_repl()

		a = r.run_lines([
			r'or false false',
			r'or false true',
			r'or true false',
			r'or true true',
			])

		eq_(['false', 'true', 'true', 'true'], a)

	def testXor(self):
		r = make_repl()

		a = r.run_lines([
			r'xor false false',
			r'xor false true',
			r'xor true false',
			r'xor true true',
			])

		eq_(['false', 'true', 'true', 'false'], a)


class PairTest(TestCase):
	def testPairs(self):
		r = make_repl()

		a = r.run_lines([
			r'first (pair a b)',
			r'second (pair a b)',
			r'first (first (pair (pair a b) (pair c d)))',
			r'second (first (pair (pair a b) (pair c d)))',
			r'first (second (pair (pair a b) (pair c d)))',
			r'second (second (pair (pair a b) (pair c d)))',
			])

		eq_(['a', 'b', 'a', 'b', 'c', 'd'], a)

	def testLists(self):
		r = make_repl()

		a = r.run_lines([
			r'empty? empty',
			r'empty? (cons a empty)',
			r'empty? (cons a (cons b empty))',
			r'head (cons a (cons b empty))',
			r'head (tail (cons a (cons b empty)))',
			r'tail (tail (cons a (cons b empty)))',
			r'last (cons a (cons b (cons c empty)))',
			])

		eq_(['true', 'false', 'false', 'a', 'b', 'empty', 'c'], a)


class FixedPointTest(TestCase):
	def testFix(self):
		r = make_repl()

		a = r.run_lines([
			r'fix \x.a',
			r'fix (\f x.x (f false) x) false',
			r'fix (\f x.x (f false) x) true',
			])

		eq_(['a', 'false', 'false'], a)


if __name__ == '__main__':
	main()
