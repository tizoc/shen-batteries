\\ Copyright (c) 2026 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Iterator computation expressions
\\:
\\: Require `(library.use [iter/cexpr])` to define the `iter.do` frontend.
\\: It provides structured comprehensions and generators that produce ordinary
\\: push-based iterators.
\\:
\\: == `iter.do`
\\:
\\: [source,shen]
\\: ----
\\: (iter.to-list
\\:   (iter.do
\\:     (for X [1 2 3])
\\:     (if (> X 1)
\\:         (do (yield X)
\\:             (yield (* X 10))))))
\\: \\ Result: [2 20 3 30]
\\: ----
\\:
\\: An `iter.do` body is delayed until the returned iterator is traversed. Each
\\: traversal evaluates the body again, including source expressions and ordinary
\\: `effect` forms. A short-circuiting consumer can stop before later combined
\\: statements are started.
\\:
\\: == Supported forms
\\:
\\: `bind` flat-maps an existing iterator. `for` converts a list with
\\: `iter.of-list`, then flat-maps it. Use `bind` with `iter.of-vector` for a
\\: vector, or with any other existing iterator. `return` and `yield` produce a
\\: singleton; `return-from` and `yield-from` splice an existing iterator.
\\: Consecutive produced computations are appended, and an empty computation
\\: produces `iter.empty`.
\\:
\\: Ordinary `let`, `effect`, and computation-body `if` forms are supported.
\\: `then` is a discarded iterator bind: the remainder runs once for every value
\\: produced by its input, and does not run when that input is empty. Thus
\\: `(then (iter.of-list [a b])) (yield x)` produces `[x x]`.
\\:
\\: `iter.do` does not support applicative `and` bindings. A portable lockstep zip
\\: of push-based iterators would require buffering or resumable producers, which
\\: would change traversal and effect behavior. Consecutive `bind` forms remain
\\: available for dependent flat-map computations. See the `cexpr` guide for the
\\: shared structured syntax.

(define iter.cexpr-builder
  { (list sexp) --> sexp }
  []                            -> [iter.empty]
  [for            Expr F]       -> [iter.flat-map F [iter.of-list Expr]]
  [bind           Expr F]       -> [iter.flat-map F Expr]
  [return         Expr]         -> [iter.singleton Expr]
  [yield          Expr]         -> [iter.singleton Expr]
  [return-from    Expr]         -> Expr
  [yield-from     Expr]         -> Expr
  [combine        CX1    CX2]   -> [iter.append CX1 CX2]
  [bind-return    Expr   F]     -> [iter.map F Expr]
  [delay          Expr]         -> (let Yield (gensym (protect Yield))
                                     [/. Yield [Expr Yield]])
  [run            Expr]         -> Expr
  Other                         -> (cexpr.default-builder iter Other))

(defcexpr iter.do iter.cexpr-builder)
