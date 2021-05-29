gllc
===========

A *generalized-LL(1) parser combinator* core can be written within 60 lines.

Based on the core and additional helper object `Parser`, a simple parser for the examplar non-LL(1) grammar `S → a a | a S a` can be defined as

``` python
import gllc as gl

n   = gl.Parser()

n.a = gl.Token('a')

n.s = n.a * n.a |\
      n.a * n.s * n.a

>>> list(n.s('aaaaaa'))
[(('a', 'a'), 'aaaa'),
 ((('a', ('a', 'a')), 'a'), 'aa'),
 ((('a', (('a', ('a', 'a')), 'a')), 'a'), '')]
```

A few points here:

- The *recursive* access to the attribute `n.s` is supported by a *lazy* strategy,
- Combinator operations like `*`, `|` are *binary* and *left associative*.
- Parser namespace is managed by the object `n` pretty neatly. You can even combine parsers from different namespaces.
- Generalized parser normally delivers all *partial* parses i.e. ambiguous results.

This README mainly talks about the intrinsic relationship between parsing and *logical inference*, and how simple it is to use coroutines for the implementation. It also shows an interesting trick to simulate the *laziness* to make the parser support recursive symbols.

# A tiny combinator core

Parser combinators are handy tools to do ad-hoc parsing work. The most famous library for that is the Haskell *Parsec* library. However, *Parsec* disables the generalized mode by default (see semantics of the operator `<|>` and function `try` in related documentation), which means the user need to do extensive left-factoring work to get the grammar working. This could lead to the loss of intuition supporting the grammar design (sometimes we just want to play with the grammar tolerating some level of ambiguity).

However, it is not uneasy to build a *generalized* parsing core from scratch. Here we use *Python* for a concise implementation benefitting from the interesting features:

* generator
* operator overriding
* lazy access to object attributes

and that's all.


## Analogue of logic expressions

Parsing is no much different than proving logical expressions like AND/OR. For example with the BNF-style grammar

```
S → A B C
  | D
```

it means we can either prove **S** after proving **A** first, then **B** and finally **C**, or go another way by just proving **D**. Any success from the both ways means **S** is proved. Analogically, we can see the grammar as *definite clauses* like the following

``` python
S <= A & B & C
S <= D
```

where `&`, `|`, `<=` mean logical *and*, *or* and *implied-by*. Symbols like **A**, **B** and etc. are seen as *goal*s of logical inference. By the way, any symbol such as **A**, **B** etc. can be terminal/atomic like a logical *literal*, which can be tested directly for its trueness.

## Input as resource for reasoning

We see an input string as resource. What parsing goes further from logical reasoning is that, proving a goal "declaims" (i.e. consumes) a prefix of the resource (the *recognized* part) thus delivers a *split*. Subsequent provings can only make use of the *residual* resource after splitting.

Note there may be more than one possible ways of declaiming for splits. A generalized parser should yields all possible splittings. A split is then exactly a *parse result* as a pair, where the recognized part may have been tranformed (consumed as) a different type other than the string type:

``` haskell
Result : (<recognized-part>, <residual-part>)
```

and each parser, as a function, accepts an input string and yields a *sequence* of results, i.e.

``` haskell
Parser : <input> -> Seq Result
```

and empty sequence if there are no successful splitting at all.

<!--
With some formularization of all above, we have 3 most fundamental parsing cases given a start input string

+ parsing a *token* defined by a literal **lit**
    + if the start input is prefixed with **lit**, then yield its residual part with such prefix cut off
+ parsing an *AND* expression with operands **left** and **right**
    + parse **left** with start input
    + parse **right** operand with each residual input yielded after parsing **left**
+ parsing an *OR* expression with operands **left** and **right**
    + parse **left** with the start input, deliver results
    + parse **right** also with the start input as if parallelly

 -->
<!--
To express the parsing rule for **S** above a bit more formally, the parsing process can be described as:

> * prove **S** with input string **inp**:
>     * try prove ((**A** and **B**) and **C**)
>     	  * prove (**A** and **B**) with **inp**
>             * */\* Recursively proving (**A** and **B**)...\*/*
>     		  * getting resulted pairs
>     		  * for each such pair (**r1**, **inp1**)
> 				  * prove **C** with **inp1**
>					  * getting resulted pairs
> 				      * yield each (**r2**, **inp2**) from the pairs
>     * */\*But we **continue** finding prooves even after having some success above...\*/*
>     * try prove **D** with **inp**
>     		* yield results similarly

Note this process is able to send out (namely **yield**) results to its caller at some internal point of execution and gets hanged there. It continues further when called again (or pipelining results into some queue without halting).

Multiple concepts have covered such characteristics, such as
| Concept | Perspective |
|:-:|-|
| *generator* | as a structure implementing the `Iterable` interface |
| *coroutine* | as a (logically concurrent) execution routine |
| *continuation* | as a state of uncompleted computation |

In the parsing context such a pause- and continuable process is treated as a restricted type of *continuation* (where each halt point is hit only once and no re-entering is needed). The generator structure of *Python* suffices for simulate this.

-->

## Implementation

Based on the brief theory above, we can start thinking about the implementation. The very first object definition would be the most fundemantal `PExpr`, i.e. the *parser expression*. It can be defined as an *abstract class* supporting *AND* and *OR* operations as explained in the previous section (which are simulated by operation `__mul__` and `__or__`). The `__truediv__` and `__pow__` operators are used for semantic applications (reasons for these choices will be explained later). The constructor accepts sub-expression arguments which are the operands of the expression.

Each subtype of `PExpr` can support the `__call__` method which accepts input strings and yield parsing results.

``` python
class PExpr(object):
    def __init__(self, *subs):
        self.subs = subs
    def __mul__(self, other):
        return And(self, other)
    def __or__(self, other):
        return Or(self, other)
    def __truediv__(self, other):
        return Seman(self, other)
    def __pow__(self, other):
        return Seman(And(self, other), lambda tp: [tp[0]] + tp[1])
    def __call__(self, inp):
        raise NotImplemented
```

The `Token` parser is our first concret parser. It tries to match the prefix of the input and yields a split, or yields nothing when matching failed.

``` python
class Token(PExpr):
    def __call__(self, inp):
        lit, = self.subs
        if inp.startswith(lit):
            yield lit, inp[len(lit):]
```

The `And` expression extending `PExpr` having its left and right operands, namely `psr1` and `psr2` and `And` now defines a new parser. It calls the parsing method of its operands recursively, where the results of `psr2` is computed based on the results of `psr1`. One detail here is that the Python `*` operator is left-associative (like most oeprators) so that the left sub-expression of `And` may itself be an `And`. Thus the resulted tuple `(r1, r2)` has `r1` as a nested tuple within chained `And` expressions.

The reason for choosing `__mul__` to represent the *And* operation is that, `tuple` is treated as *product* type in relevant theories (cf. [Algebraic Data Type](https://en.wikipedia.org/wiki/Algebraic_data_type)).

``` python
class And(PExpr):
    def __call__(self, inp):
        psr1, psr2 = self.subs
        for r1, inp1 in psr1(inp):
            for r2, inp2 in psr2(inp1):
                yield (r1, r2), inp2
```

For the `Or` expression, the parsing yields results from both its operands, and results from either branch don't depend on the other branch. You can even think about submitting the parsing task of each operands to a parallel execution context.

``` python
class Or(PExpr):
    def __call__(self, inp):
        for psr in self.subs:
            yield from psr(inp)
```

Then we need the very famous *Kleene-closure* combinator, which can be implemented using a loop.
From the beginning on, the parsing prepares an *agenda* for tracking feasible results. In each iteration, it checks the agenda containing feasible splits, and tries to make new splits by consuming another sequence of input. Once no new splits are found, it yields all results in the agenda. This is an *eager* matching scheme since with "Many" it means "consuming as much resource as possible to accumulate valid parsing splits".

``` python
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
```

For convenience, we can define `Many1` which means repetitive parsing of the `PExpr` should have at least one split set found.

``` python
class Many1(PExpr):
    def __call__(self, inp):
        psr, = self.subs
        m = Many(psr)
        for r, inp1 in psr(inp):
            for rs, inp2 in m(inp1):
                yield [r] + rs, inp2
```

Now we already have all combinators to handle grammars in most cases. You can play with the parsers already:

``` python
>>> ab = Many(Token('a') | Token('b'))
>>> next(ab('babbac'))
(['b', 'a', 'b', 'b', 'a'], 'c')
```

## About the semantics

Simply parsing is always not enough - interpretation is almost always necessary when working with practical data transformation. Now we easily define the `Seman` expression to indicate interpretation:

``` python
class Seman(PExpr):
    def __call__(self, inp):
        psr, func = self.subs
        for r, inp1 in psr(inp):
            yield func(r), inp1
```

It is a binary operation with two operands: a parser `psr` and a function `func`. All it needs is to deliver functioned results based on the results delivered by `psr`. It is also interesting that `Seman` objects can be *nested*. For example, `Seman(Seman(p1, f1), f2)` just *pipelines* the results of `p1` to `f1` then further to `f2`.

Since we already overloaded operator `__truediv__` for interpretation (`/` is chosen since product type `tuple` may get *divided* into unary parts and get handled somehow), we can use it like:

```python
>>> r = Token('a') / str.upper
>>> next(r('ac'))
('A', 'c')
```

In case of handling results from `Many`, the semantic function accepts a list as its argument:

``` python
>>> p = Token('0') | Token('1')
>>> r = Many(p / int) / (lambda xs: [x for x in xs if x > 0])
>>> next(r('101012'))
([1, 1, 1], '2')
```

# How to be lazy?

One big problem of using combinators in practice is that most languages are *strict* when constructing data structures, that is, every argument for a operation must be evaluated before evaluating the operation itself.

For example, given the grammar:
```
a → 'a'
S → a a
  | a S a
```

we can not write the combinator in Python like

``` python
a = Token('a')
S = a * a |\
    a * S * a
```

since `S` needs to be evaluated on the right hand side sub-expression `a * S * a` before it is actually defined. In a lazy language like Haskell, this won't be a problem since the evaluation does not happen when contructing an expression.

But we don't want to lose our concise way of representing the grammar. One approach is to use the `lambda` structure for lazy look-up of `S` in the `globals` environment:

``` python
a = Token('a')
S = lambda inp: \
    (a * a | a * S * a)(inp)
```

which works but it is for sure annoying to repeatly type `lambda inp:` when playing with your funny language.

Here we have several possible tricks.

### A lazy object

First we define the `LazyExpr` class. An lazy expression is defined in a given *context* (aka. *namespace* as well as *environment*), which is prepared in advance.

``` python
class LazyExpr(PExpr):
    def __init__(self, name, context):
        self.name = name
        self.context = context
    def __call__(self, *args):
        return self.context[self.name](*args)
```

Note such expression is logically also a subtype of `PExpr` and is callable. Now the grammar above can be written as

``` python
# Preparing context and construct
ctx = {}
S = LazyExpr('S', ctx)

# Making rules
a = Token('a')
S = ctx['S'] = a * a |\
               a * S * a
```

Now calling the combinator seems neater:
```
>>> list(S('aa'))
[(('a', 'a'), '')]

>>> list(S('aaaaaa'))
[(('a', 'a'), 'aaaa'),
 ((('a', ('a', 'a')), 'a'), 'aa'),
 ((('a', (('a', ('a', 'a')), 'a')), 'a'), '')]
```

## And even simpler?

It still seems annoying to always write the `ctx['S']` stuff. To get rid of this, we finally come to the last resort - overriding built-in operations. The class `Parser` is supposed to be a manager of the context of the lazy expressions, subtyping `dict`:

``` python
class Parser(dict):

    def __getattr__(self, k):
        return LazyExpr(k, self)

    def __setattr__(self, k, v):
        if k not in self:
            self[k] = v
        else:
            self[k] |= v
```

In short, with the overrider `__getattr__`, any access to an non-existing left-hand-side symbol `k` as an attribute leads to the creation of a lazy expression given a symbol `k`. With `__setattr__` a symbol `k` is bound to a right-hand-side expression (or augments the existing expression via an `Or` expression).

Now the `S`-grammar above can be written as

``` python
n = Parser()

a = Token('a')
n.s = a * a | a * n.s * a
```

which although seems not super ideal, but concise enough for practical use. More interesting here is, `Parser` provides extra policy of managing combinators within some namespaces, which can be potentially useful if you think about defining different grammars in separate modules and combining them together for use. Say you have a parser `n` and a parser `m`, you can now define a parser like `my_parser = n.s * m.t * n.w`.


# Utilities

To make ad-hoc things easier, we prepare two functions `fst` and `snd` in order to select useful component of any recognized parsing result yielded by an `And` parser (which is a always an 2-tuple).

``` python
from operator import itemgetter
fst = itemgetter(0)
snd = itemgetter(1)
```

Utility combinators like the following can be useful too:

``` python
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


White = Many(OneOf(' \t\n'))
Digit = OneOf('0123456789')
Alpha = OneOf(map(chr, [*range(65, 91), *range(97, 123)]))
AlphaNum = Alpha | Digit
```

A quite useful operator `**` (see its overloader in `PExpr` class) can help with concatenating a single element with a following list delivered by `Many` or `Many1`. For example, a list of integers seperated by comma can be handled like:

``` python
Digits = Many(Digit) * White / fst / ''.join
int1 = Digits / int
ints = int1 ** Many(Word(',') * int1 / snd)

>>> next(ints('123, 45, 987'))
([123, 45, 987], '')
```

Even more operator sugar can be provided via overloading `__rshift__` and `__lshift__` in `PExpr` based on `fst` and `snd`, meaning *ignoring* a followed or following part:

``` python
class PExpr(object):
    ...
    def __lshift__(self, other):
        return Seman(And(self, other), fst)
    def __rshift__(self, other):
        return Seman(And(self, other), snd)

Digits = (Many(Digit) << White) / ''.join
int1 = Digits / int
ints = int1 ** Many(Word(',') >> int1)

>>> next(ints('123, 45, 987'))
([123, 45, 987], '')
```

# Altogether now

Finally, here is an example grammar for basic arithmetics.

``` python
e = Parser()

def prod(xs):
    p = 1
    for x in xs: p *= x
    return p

e.expr   = e.term ** Many(Word('+') >> e.term) / sum
e.term   = e.factor ** Many(Word('*') >> e.factor) / prod
e.factor = e.number |\
           Word('(') >> e.expr << Word(')')
e.number = (Many1(Digit) << White) / ''.join / int
```

And that's all. The user can extend the parsers with whatever new combinator according to the needs.


# TODO

There are potential improvements considering performance and memory usage. For example

+ Use indices rather than making string slices to represent residual inputs.
+ Use *Graph Structured Stack* or *CONS* to avoid copying lists when parsing with `Many`, `Many1`.
+ Augment the `Result` structure with *Left/Right* structure to report error messages.
+ Make process tracing possible.
+ Make predictive/non-consuming combinators (starting from `<<` and `>>`).
+ Detect left recursion.
