language-webidl-hs
=====

This project is suspended unless someone is willing to take it over.

## Introduction

Parser and Pretty-printer for WebIDL in Haskell.

Compared to the out-of-maintenance [webidl](https://hackage.haskell.org/package/webidl)
on hackage, this new one doesn't use the lex/yacc infrastructure, but is built entirely in
Haskell with parser combinators.

The AST node is parametric, so you can put anything in it. In the parser provided in this
package, the extra field records the adjacent comments and `SourcePos`. But you can `fmap`
on the extra field to customize.

Also, most AST node parsers are exposed in `Language.WebIDL.Parser` module, so you
can reuse and compose these snippet freely.

Reference: https://www.w3.org/TR/WebIDL-1/

## Contributions

We need your help to keep this repo updated with the official specification!
Please feel free to add an issue or make a PR if you find anything inconsistent
or inconvenient.

Thanks to Mark Roberts for the helpful comments.
