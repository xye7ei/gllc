gllc
===========

A *generalized-LL(1) parser combinator* core can be written within 60 lines.

Based on this core and with some utilities like `Parser` object added, a simple parser for some non-LL(1) grammar `S → a a | a S a` can be written as

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

Things to be noted here

- combinator operations like `*`, `|` are *binary*,
- the *recursive* access of attribute `n.s` is supported by some *lazy* strategy,
- parser namespace is managed by object `n` neatly, but also supports further external composition.

This essay mainly talks about some intrinsic relationship between parsing and *logical inference*, how simple an implementation can be due to that and furthermore, how it is a funny trick to simulate *laziness* for easy usage.

# A tiny combinator core

Parser combinators are handy and useful tool to perform ad-hoc parsing work. Among various approaches, the most famous is the Haskell *Parsec* library. However, for the sake of performance, *Parsec* disables the generalized mode by default (see semantics of the operator `<|>` and function `try` in related documentation).

But it is not difficult to construct the *generalized* core from bottom up. Here *Python* is chosen for a terse implementation due to the interesting features:

* generator
* operator overriding
* possible laziness

and that's all.


## Analogue of logic expressions

Parsing is no much more than logically proving AND/OR expressions. For example given the BNF-style grammar rule

```
S → A B C
  | D
```

it means we can either prove **S** after firstly proving **A**, secondly **B** and thirdly **C**, or going another way by singly proving **D**. Any success from the both ways leads **S** to success. With this analogy, we can treat the grammar like *definite clauses* as

``` python
S <= A & B & C
S <= D
```

where `&`, `|`, `<=` mean logical *and*, *or* and *implied-by*. Symbols like **A**, **B** and etc. are treated as *goal*s of logical inference. By the way, any symbol such as **A**, **B** etc. can be terminal/atomic like a logical *literal*, which can be tested directly if it matches the truth.

## Input as resource for reasoning

Treating an input string as resource, what parsing has extended itself from logical reasoning is that, proving a goal declaims a prefix of such resource (the *recognized* part) thus delivers a *split*. Subsequent provings can only make use of the *residual* resource after splitting.

Note there may be more than one possible ways declaiming split doing a parsing. A generalized parser thus yields all possible versions of splitting. Here a split is exactly a *parse result* as a pair, where recognized part may have possibly functioned type other than string type:

``` haskell
Result : (<recognized-part>, <residual-part>)
```

and each parser, as a function, accepts some input and yields a *sequence* of such results, i.e.

``` haskell
Parser : <input> -> Seq Result
```

and empty sequence when no success.

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

The very first definition is the most fundemantal `PExpr`, i.e. the *parser expression*. It is an *abstract class* supporting *AND* and *OR* operations, which are simulated by operation `*` and `|`. The */* and *\*\** operators for semantic application would be explained later on. The shared constructor with sub-expression arguments is for simplicity.

Each subtype of `PExpr` should support `__call__` method as virtual `def parse(inp): ...` method accepting input string as its argument and yield possible results.

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

The `Token` parser tries to match the prefix of the input and yield nothing when matching failed.

``` python
class Token(PExpr):
    def __call__(self, inp):
        lit, = self.subs
        if inp.startswith(lit):
            yield lit, inp[len(lit):]
```

The `And` expression extending `PExpr` having left and right operands, namely `psr1` and `psr2` and comprises a new parser. It calls parsing by its operands recursively, where the results of `psr2` is derived from results of `psr1`. A detail here is that the Python `*` operator is left-associate (like most oeprators) so that the left sub-expression of `And` may itself be an `And`. Thus the resulted tuple `(r1, r2)` has `r1` as a nested tuple within chained `And` expressions.

``` python
class And(PExpr):
    def __call__(self, inp):
        psr1, psr2 = self.subs
        for r1, inp1 in psr1(inp):
            for r2, inp2 in psr2(inp1):
                yield (r1, r2), inp2
```

For the `Or` expression, parsing yield results from its every operands independently. High potential of parallelism show up here.

``` python
class Or(PExpr):
    def __call__(self, inp):
        for psr in self.subs:
            yield from psr(inp)
```

Then we need the hilarious *Kleene-closure* combinator, which can be implemented in iterative manner.
After starting parsing it bookkeeps an *agenda* of feasible results so far and tries finding updates for the agenda with possible parsings. Once nothing found, it yields all results in the agenda. This is an *eager* matching scheme.

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

Based on this, `Many1` is for a sequence having at least one parsings.

``` python
class Many1(PExpr):
    def __call__(self, inp):
        psr, = self.subs
        m = Many(psr)
        for r, inp1 in psr(inp):
            for rs, inp2 in m(inp1):
                yield [r] + rs, inp2
```

Now all combinators to handle most practical grammars get prepared. Some little playing shows the ease of usage.

``` python
>>> ab = Many(Token('a') | Token('b'))
>>> next(ab('babbac'))
(['b', 'a', 'b', 'b', 'a'], 'c')
```

## And the meanings?

Simply parsing is always not enough - interpretation is necessary in practical work. Based on the concepts above, a semantical combinator is just easy to write:

``` python
class Seman(PExpr):
    def __call__(self, inp):
        psr, func = self.subs
        for r, inp1 in psr(inp):
            yield func(r), inp1
```

It is a binary operation with two operands: a parser `psr` and a function `func`. All it needs is to deliver  functioned results based on the results delivered by `psr`. It is also interesting that `Seman` objects can get *nested*. For example, `Seman(Seman(p1, f1), f2)` just *pipelines* the results of `p1` to `f1` then further to `f2`.

Since we already overloaded operator `/` for semantics, we can use this like

```python
>>> r = Token('a') / str.upper
>>> next(r('ac'))
('A', 'c')
```

Upon any result from `Many`, semantics accepts list as the argument:

``` python
>>> p = Token('0') | Token('1')
>>> r = Many(p / int) / (lambda xs: [x for x in xs if x > 0])
>>> next(r('101012'))
([1, 1, 1], '2')
```

# How lazy?

One severe problem for using combinators pracically is that most language are *strict* when constructing data structures, that is, every argument for a operation must be evaluated before evaluating the operation.

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

since `S` needs to be evaluated in the right hand side sub-expression `a * S * a` before it gets defined. To allow construction by instantiating before evaluation by calling, one approach is to use the `lambda` structure for lazy look-up of `S`:

``` python
a = Token('a')
S = lambda inp: \
    (a * a | a * S * a)(inp)
```

which seems a little cumbersome to repeatly type `lambda` and `inp`. But we have a another trick for somewhat better neatness.

### Some lazy object

Firstly we define the `LazyExpr` class. An lazy expression is defined given a corresponding *context* (aka. *namespace* as well as *environment*), which is prepared in advance.

``` python
class LazyExpr(PExpr):
    def __init__(self, name, context):
        self.name = name
        self.context = context
    def __call__(self, *args):
        return self.context[self.name](*args)
```

Note such expression is logically also subtype of `PExpr` and is callable. Now the grammar above can be written as

``` python
# Preparing context and construct
ctx = {}
S = LazyExpr('S', ctx)

# Making rules
a = Token('a')
S = ctx['S'] = a * a |\
               a * S * a
```

Now calling the combinator is no problem:
```
>>> list(S('aa'))
[(('a', 'a'), '')]

>>> list(S('aaaaaa'))
[(('a', 'a'), 'aaaa'),
 ((('a', ('a', 'a')), 'a'), 'aa'),
 ((('a', (('a', ('a', 'a')), 'a')), 'a'), '')]
```

## And simpler?

It still seems ugly to write `ctx['S']` stuff. To get rid of this, we finally come to the last resort - overriding some core operations. The class `Parser` is supposed to be a manager of context and lazy expressions, subtyping `dict`:

``` python
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
```

In short, with the overrider `__getattr__`, any access to an non-existing LHS `k` as an attribute creates a lazy expression given a symbol `k`. With `__setattr__` a symbol `k` gets bound to a `RHS` expression (or augments the existing expression as an `Or` expression).

Now the `S`-grammar above can be written as

``` python
n = Parser()

a = Token('a')
n.s = a * a | a * n.s * a
```

which although seems not totally ideal, but concise enough for practical use.


# Utilities

To make things easier, we prepare two functions `fst` and `snd` in order to select useful component of any recognized parsing result yielded by an `And` parser (which is a always an 2-tuple).

``` python
from operator import itemgetter
fst = itemgetter(0)
snd = itemgetter(1)
```

Several further utility combinators like these are often useful:

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

A quite useful operator `**` (see its overloader in `PExpr` class) can help to concatenate a single element with following list delivered by `Many` or `Many1`. For example, a list of integers seperated by comma can be handled with:

``` python
Digits = Many(Digit) * White / fst / ''.join
int1 = Digits / int
ints = int1 ** Many(Word(',') * int1 / snd)

>>> next(ints('123, 45, 987'))
([123, 45, 987], '')
```

or even more operator sugar with overloading `__rshift__` and `__lshift__` in `PExpr` for underlied application of `fst` and `snd`, meaning *ignoring* a followed or following part:

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

The rest of work for practical use requires only more combinated utilities based on things above.


# TODO

There is potential optimizations for performance and memory usage. For example

+ Use indices rather than making string slices to represent residual inputs.
+ Use *Graph Structured Stack* or *CONS* to avoid copying lists when parsing with `Many`, `Many1`.
+ Augment the `Result` structure with *Left/Right* structure to report error messages.
+ Make process tracing possible.
+ Make some predictive/non-consuming combinators (starting from `<<` and `>>`).
+ Consider possibilities for these *binary* combinators.
+ Left recursion.

