import gllc as gl
from pprint import pprint

n = gl.Parser()

n.a = gl.Word('a')
n.s = n.a * n.a |\
      n.a * n.s * n.a

pprint(list(n.s('a a a  a a a')))


n = gl.Parser()

# All RHS terms are Lazy
# - Since each term can be incomplete when constructing any combinator.
# - combination of several LazyExpr is a virtually lazy expression
n.s = n.a * n.a
n.a = gl.Word('a')
n.s = n.a * n.s * n.a

pprint(list(n.s('a a a  a a a')))


n = gl.Parser()
a = gl.Word('a')
n.s = a * n.s * a
n.s = a * a

pprint(list(n.s('a a a  a a a')))

