language-webidl-hs
=====

Parser and Pretty-printer for WebIDL in Haskell.

Compared to the out-of-maintenance [webidl](https://hackage.haskell.org/package/webidl)
on hackage, this new one didn't use the lex/yacc infrastructure, but built entirely in
Haskell with parser combinators.

The AST node is parametric, so you can put anything in it. In the parser provided in this
package, the extra field records the adjacent comments and `SourcePos`. But you can `fmap`
on the extra field to customize.
