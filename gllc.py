class PExpr(object):
    def __init__(self, *subs):
        self.subs = subs
    def __mul__(self, other):
        return And(self, other)
    def __or__(self, other):
        return Or(self, other)
    def __truediv__(self, f):
        return Seman(self, f)
    def __pow__(self, other):
        return Seman(And(self, other), lambda tp: [tp[0]] + tp[1])
    def __lshift__(self, other):
        return Seman(And(self, other), lambda tp: tp[0])
    def __rshift__(self, other):
        return Seman(And(self, other), lambda tp: tp[1])

class Token(PExpr):
    def __call__(self, inp):
        lit, = self.subs
        if inp.startswith(lit):
            yield lit, inp[len(lit):]

class And(PExpr):
    def __call__(self, inp):
        psr1, psr2 = self.subs
        for r1, inp1 in psr1(inp):
            for r2, inp2 in psr2(inp1):
                yield (r1, r2), inp2

class Or(PExpr):
    def __call__(self, inp):
        for psr in self.subs:
            yield from psr(inp)

class Many(PExpr):
    def __call__(self, inp):
        psr, = self.subs
        agd = [([], inp)]
        while 1:
            agd1 = []
            for rs, inp in agd:
                for r1, inp1 in psr(inp):
                    agd1.append((rs+[r1], inp1))
            if agd1: agd = agd1
            else: break
        yield from agd

class Many1(PExpr):
    def __call__(self, inp):
        psr, = self.subs
        m = Many(psr)
        for r, inp1 in psr(inp):
            for rs, inp2 in m(inp1):
                yield [r] + rs, inp2

class Seman(PExpr):
    def __call__(self, inp):
        psr, func = self.subs
        for r, inp1 in psr(inp):
            yield func(r), inp1

# # For `Seman.__call__`:
# `r` may or may not be a tuple (may be already some
# semantic result)!  So deconstructing tuple should not
# happen here.

class Opt(PExpr):
    def __call__(self, inp):
        got = False
        for r1, inp1 in self.subs[0](inp):
            got = True
            yield ([r1], inp1)
        if not got:
            yield ([], inp)

class Full(PExpr):
    def __call__(self, inp):
        psr, = self.subs
        for r, inp1 in psr(inp):
            if not inp1:
                yield r

import re

class RgxToken(PExpr):
    def __call__(self, inp):
        lit, = self.subs
        m = re.match(lit, inp)
        if m:
            yield m.group(), inp[m.end():]



# Utilities

from functools import reduce
from operator import itemgetter
fst = itemgetter(0)
snd = itemgetter(1)

White = Many(Token(' ') | Token('\t') | Token('\n') | Token('\v'))

assert(list(White('   \n  \v b'))) == [([' ', ' ', ' ', '\n', ' ', ' ', '\v', ' '], 'b')]

def Word(lit):
    return Token(lit) * White / fst

assert(list(Word('abc')('abc   de'))) == [('abc', 'de')]


Digit = reduce(PExpr.__or__, map(Token, '0123456789'))

class OneOf(PExpr):
    def __init__(self, alts):
        self.alts = set(alts)
    def __call__(self, inp):
        if inp and inp[0] in self.alts:
            yield inp[0], inp[1:]

class NoneOf(PExpr):
    def __init__(self, alts):
        self.alts = alts
    def __call__(self, inp):
        if inp and inp[0] not in self.alts:
            yield inp[0], inp[1:]

from operator import or_
# OneOf = lambda xs: reduce(or_, map(Token, xs))
White = Many(OneOf(' \t\n'))
Digit = OneOf('0123456789')
Alpha = OneOf(map(chr, [*range(65, 91), *range(97, 123)]))

# print(list(White('    \n \vgg')))
# print(list(Digit('1234')))
# print(list(Alpha('abc')))
# assert 0

# =========================
#         Frontend
# =========================

class LazyExpr(PExpr):
    def __init__(self, name, context):
        self.name = name
        self.context = context
    def __repr__(self):
        return '-{}-'.format(self.name)
    def __call__(self, *args):
        return self.context[self.name](*args)

# a = Token('a')
# S = lambda inp: \
#     (a * a | a * S * a)(inp)
# 
# ctx = {}
# a = Token('a')
# S = LazyExpr('S', ctx)
# 
# S = ctx['S'] = a * a |\
#                a * S * a
# 
# from pprint import pprint
# pprint(list(S('aa')))
# pprint(list(S('aaaaaa')))
# assert 0

class Parser(dict):
    def __getattr__(self, k):
        if not k in self:
            return LazyExpr(k, self)
        else:
            return self[k]
    def __setattr__(self, k, v):
        if k not in self:
            self[k] = v
        else:
            self[k] |= v

def gll(func):
    return func(Parser())


if __name__ == '__main__':

    import unittest

    a = Token('a')
    b = Token('b')
    c = Token('c')
    ab = a * b
    abc = a * b * c
    a_b = a | b

    class Test(unittest.TestCase):

        def test_token(self):
            assert(list(a('abc'))) == [('a', 'bc')]
            assert(list(b('abc'))) == []
            assert(list(b('bbc'))) == [('b', 'bc')]

        def test_seq(self):

            assert(list(ab('abc')))  == [(('a', 'b'), 'c')]
            assert(list(abc('abc'))) == [((('a', 'b'), 'c'), '')]
            assert(list(abc('abd'))) == []

        def test_alt(self):
            assert(list(a_b('abc'))) == [('a', 'bc')]
            assert(list(a_b('bbc'))) == [('b', 'bc')]
            assert(list(a_b('cbc'))) == []

        def test_many(self):
            assert(list(Many(a)('b'))) == [([], 'b')]
            assert(list(Many(a_b)('aaaab'))) == [(['a', 'a', 'a', 'a', 'b'], '')]

    unittest.main()