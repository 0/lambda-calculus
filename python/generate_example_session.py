#!/usr/bin/env python3

from io import StringIO

from lambda_calculus import LambdaError, REPL, set_extra_info


# Completely arbitrary threshold.
MAX_WIDTH = 43

# Session contents.
INPUT = [
		(r'(\x.x x) id', 'Variables, abstraction, application.'),
		(r'(\x y.y x) true id', 'Currying for multiple arguments.'),
		(r'not true', 'Built-in named functions.'),
		(r'=apply.\f x.f x', 'Ugly syntax for defining global names.'),
		(r'', 'Comments, if you haven\'t noticed.'),
		(r'(\p q r.p q r) id id', 'Obscure format for printing closures.'),
		(r'(\p q r.p q r) id id id', 'Left-associative application.'),
		(r'(\p q r.z) id id', 'Unevaluated closure contents.'),
		(r'', 'Some error handling.'),  # It doesn't look as nice if the comment
		(r'(\p q r.z) id id id', None), # is repeated in the error message.
		(r'false (id z) true', 'Lazy evaluation.'),
		(r'=lst.cons true (cons false (cons apply empty))', 'Linked lists.'),
		(r'last lst', 'Handy list operations.'),
		(r',last', 'Value inspection, named recursion.'),
		(r'fix (\f x.empty? (tail x) (head x) (f (tail x))) lst', 'Anonymous recursion.'),
		]


def prepare_input(lines, comment):
	"""
	Transform desired input lines and comment into actual lines that will be
	entered into the REPL.
	"""

	if isinstance(lines, str):
		lines = [lines]
	else:
		# Make sure we don't modify the original list.
		lines = lines.copy()

	if comment is not None:
		if len(lines[0]) > MAX_WIDTH:
			lines.insert(0, '{} # {}'.format(' ' * MAX_WIDTH, comment))
		else:
			lines[0] += '{} # {}'.format(' ' * (MAX_WIDTH - len(lines[0])), comment)

	return lines


def make_io_pair(in_lines):
	"""
	Make a pair of functions to perform virtual input and output.
	"""

	buf_out = StringIO()
	in_iter = iter(in_lines)

	def i(prompt):
		try:
			value = next(in_iter)
		except StopIteration:
			raise EOFError()

		buf_out.write(prompt)
		buf_out.write(value)
		buf_out.write('\n')

		return value

	def o(value):
		buf_out.write(str(value))
		buf_out.write('\n')

	def result():
		return buf_out.getvalue()

	return i, o, result


if __name__ == '__main__':
	prepared_input = []

	for ls, c in INPUT:
		prepared_input.extend(prepare_input(ls, c))

	i, o, result = make_io_pair(prepared_input)

	set_extra_info(o)

	r = REPL()
	r.run(i=i, o=o)

	for line in result().splitlines():
		print('    ' + line)
