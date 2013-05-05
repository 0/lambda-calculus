#!/usr/bin/env python3

from collections import ChainMap
import re


class LambdaError(Exception):
	pass


class Var():
	"""
	Variable.

	x
	"""

	def __init__(self, name):
		"""
		name: str
		"""

		self.name = name

	def eval(self, env):
		try:
			return env[self.name]
		except KeyError:
			# I'm free!
			raise LambdaError('Undefined variable: ' + str(self)) from None

	def __repr__(self):
		return self.__class__.__name__ + '(' + self.name + ')'

	def __str__(self):
		return self.name

class Abs():
	"""
	Abstraction.

	\\x y.z
	"""

	def __init__(self, param, body):
		"""
		param: Variable
		body: LambdaExpression
		"""

		self.param = param
		self.body = body

	def eval(self, env):
		return Closure(self.param, self.body, env)

	def __repr__(self):
		return self.__class__.__name__ + '(' + repr(self.param) + ', ' + repr(self.body) + ')'

	def __str__(self):
		return '\\' + str(self.param) + '.' + str(self.body)

class App():
	"""
	Application.

	x y
	"""

	def __init__(self, fn, arg):
		"""
		fn: LambdaExpression
		arg: LambdaExpression
		"""

		self.fn = fn
		self.arg = arg

	def eval(self, env):
		def body():
			fn = Thunk.un(self.fn.eval(env))
			arg = self.arg.eval(env)

			return fn.apply(arg)

		return Thunk(body)

	def __repr__(self):
		return self.__class__.__name__ + '(' + repr(self.fn) + ', ' + repr(self.arg) + ')'

	def __str__(self):
		return '(' + str(self.fn) + ' ' + str(self.arg) + ')'

class Ass():
	"""
	Global assignment.

	=x.y
	"""

	def __init__(self, var, value):
		"""
		var: Variable
		value: LambdaExpression
		"""

		self.var = var
		self.value = value

	def eval(self, env):
		# Obtain the result of the sub-expression, which will be returned as
		# the result of the overall expression.
		result = Thunk.un(self.value.eval(env))

		# Apply the side effect to the dictionary of globals.
		globs = env.maps[-1]

		try:
			name = self.var.name
		except AttributeError:
			raise LambdaError('Not a variable: ' + str(self.var)) from None

		try:
			result.global_name
		except AttributeError:
			# Doesn't already have a global name.
			result.global_name = name

		globs[name] = result

		return result

	def __repr__(self):
		return self.__class__.__name__ + '(' + repr(self.var) + ', ' + repr(self.value) + ')'

	def __str__(self):
		return '=' + str(self.var) + '.' + str(self.value)


class Closure():
	def __init__(self, param, body, env):
		"""
		param: Variable
		body: LambdaExpression
		env: ChainMap
		"""

		self.param = param
		self.body = body
		self.env = env

	def apply(self, arg):
		child_env = self.env.new_child()

		child_env[self.param.name] = arg

		return self.body.eval(child_env)

	def _non_globals(self):
		envs = [set(m) for m in self.env.maps[:-1]]

		if envs:
			return list(sorted(set.union(*envs)))
		else:
			return []

	def __repr__(self):
		return self.__class__.__name__ + '(' + repr(self.param) + ', ' + repr(self.body) + ', ' + repr(self.env) + ')'

	def __str__(self):
		return '\\' + str(self.param) + '.' + str(self.body) + ' [' + ','.join(self._non_globals()) + ']'

class Thunk():
	def __init__(self, body):
		"""
		body: function
		"""

		self.body = body

	@staticmethod
	def un(thing):
		"""
		Unthunk a value out of a potential thunk.
		"""

		while isinstance(thing, Thunk):
			thing = thing.call()

		return thing

	def call(self):
		return self.body()

	def __repr__(self):
		return 'Thunk'


class Parser():
	@classmethod
	def read(cls, text):
		"""
		Make a LambdaExpression out of some text.
		"""

		return cls._parse(cls._tokenize(text))

	@staticmethod
	def _tokenize(text):
		"""
		Split a string into tokens.
		"""

		pat = re.compile(r"""
				(?P<OPEN>\()|
				(?P<CLOSE>\))|
				(?P<LAMBDA>\\)|
				(?P<EQUAL>=)|
				(?P<DOT>\.)|
				(?P<COMMENT>\#.*?$)|
				(?P<SYMBOL>[^\s()\\.#]+)
				""", re.MULTILINE | re.VERBOSE)

		ignored_tokens = {'COMMENT'}
		simple_tokens = {'OPEN', 'CLOSE', 'LAMBDA', 'EQUAL', 'DOT'}
		data_tokens = {'SYMBOL'}

		for m in pat.finditer(text):
			for k, v in m.groupdict().items():
				if v is None:
					continue

				if k in ignored_tokens:
					continue
				elif k in simple_tokens:
					yield k, None
				elif k in data_tokens:
					yield k, v

				break

	@classmethod
	def _parse(cls, tokens, *, get_params=False):
		"""
		Parse a token stream into a LambdaExpression.
		"""

		if get_params:
			result = []
		else:
			result = None

		for t, d in tokens:
			new = None
			done = False

			if t == 'OPEN':
				new = cls._parse(tokens)
			elif t in {'CLOSE', 'DOT'}:
				done = True
			elif t == 'LAMBDA':
				params = cls._parse(tokens, get_params=True)

				if not params:
					raise LambdaError('No parameters in lambda')

				body = cls._parse(tokens)

				if not body:
					raise LambdaError('No body in lambda')

				new = Abs(params[-1], body)

				for param in params[-2::-1]:
					new = Abs(param, new)

				done = True
			elif t == 'EQUAL':
				var = cls._parse(tokens)

				if not var:
					raise LambdaError('No variable to assign to')

				value = cls._parse(tokens)

				if not value:
					raise LambdaError('No value to assign: ' + var.name)

				new = Ass(var, value)

				done = True
			elif t == 'SYMBOL':
				new = Var(d)

			if new is not None:
				if get_params:
					result.append(new)
				elif result is None:
					result = new
				else:
					result = App(result, new)

			if done:
				break

		return result


class REPL():
	"""
	Read, eval, print, loop.
	"""

	DEFAULTS = {
			'id': r'\x.x',

			# Church booleans.
			'true':  r'\t f.t',
			'false': r'\t f.f',

			'not': r'\p.p false true',
			'and': r'\p q.p q p',
			'or': r'\p q.p p q',
			'xor': r'\p q.p (not q) q',
			}

	def __init__(self):
		self.parser = Parser()
		self.globs = ChainMap()

		# Initialize the default globals.
		for name, code in self.DEFAULTS.items():
			self.run_line('=' + name + '.' + code)

	def run_line(self, line):
		expr = self.parser.read(line)

		if not expr:
			return ''

		obj = Thunk.un(expr.eval(self.globs))

		try:
			return obj.global_name
		except AttributeError:
			return str(obj)

	def run_lines(self, lines):
		return list(map(self.run_line, lines))

	def run(self):
		try:
			while True:
				try:
					result = self.run_line(input('> '))

					if result:
						print(result)
				except LambdaError as exc:
					print(exc)
		except EOFError:
			# Terminate gracefully.
			pass


if __name__ == '__main__':
	repl = REPL()

	repl.run()
