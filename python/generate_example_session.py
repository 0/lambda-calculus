#!/usr/bin/env python3

from lambda_calculus import REPL, LambdaError


# Completely arbitrary threshold.
MAX_WIDTH = 43

# Session contents.
LINES = [
		(r'(\x.x x) id', 'Variables, abstraction, application.', r'id'),
		(r'(\x y.y x) true id', 'Currying for multiple arguments.', r'true'),
		(r'not true', 'Built-in named functions.', r'false'),
		(r'=apply.\f x.f x', 'Ugly syntax for defining global names.', r'apply'),
		(r'', 'Comments, if you haven\'t noticed.', r''),
		(r'(\p q r.p q r) id id', 'Obscure format for printing closures.', r'\r.((p q) r) [p q]'),
		(r'(\p q r.p q r) id id id', 'Left-associative application.', r'id'),
		(r'(\p q r.z) id id', 'Unevaluated closure contents.', r'\r.z [p q]'),
		(r'(\p q r.z) id id id', 'Some error handling.', r'Undefined variable: z'),
		(r'false (id z) true', 'Lazy evaluation.', r'true'),
		(r'=lst.cons true (cons false (cons apply empty))', 'Linked lists.', r'lst'),
		(r'last lst', 'Handy list operations.', r'apply'),
		(r',last', 'Value inspection, named recursion.', r'last', r'\x.(((empty? (tail x)) (head x)) (last (tail x))) []'),
		(r'fix (\f x.(empty? (tail x)) (head x) (f (tail x))) lst', 'Anonymous recursion.', r'apply'),
		]


def run_line(r, line, comment, expected, extra):
	try:
		actual = r.run_line(line)
	except LambdaError as exc:
		actual = str(exc)

	# Make sure the example session is actually valid.
	if expected != actual:
		raise ValueError((expected, actual))

	if len(line) > MAX_WIDTH:
		print('    > {} # {}'.format(' ' * MAX_WIDTH, comment))
		print('    > {}'.format(line))
	else:
		print('    > {}{} # {}'.format(line, ' ' * (MAX_WIDTH - len(line)), comment))

	for e in extra:
		print('    {}'.format(e))

	if expected:
		print('    {}'.format(expected))


if __name__ == '__main__':
	r = REPL()

	for line, comment, expected, *extra in LINES:
		run_line(r, line, comment, expected, extra)
