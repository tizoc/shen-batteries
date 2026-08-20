\\ Copyright (c) 2026 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Maybe computation expressions
\\:
\\: Require `(library.use [maybe/cexpr])` to define the `maybe.do` frontend.
\\: It provides structured optional-value pipelines: a failed step returns
\\: `(@none)` immediately, while a successful step makes its value available
\\: to the rest of the computation.
\\:
\\: == `maybe.do`
\\:
\\: [source,shen]
\\: ----
\\: (define positive
\\:   { number --> (maybe.t number) }
\\:   X -> (@some X) where (> X 0)
\\:   _ -> (@none))
\\:
\\: (maybe.do
\\:   (bind X (positive 4))
\\:   (let Y (+ X 3))
\\:   (return (* Y 2)))
\\: \\ Result: (@some 14)
\\: ----
\\:
\\: == Supported forms
\\:
\\: `bind` and `then` short-circuit on `@none`. `return` and `yield` wrap a
\\: value in `@some`; `return-from` and `yield-from` use an existing Maybe, so
\\: `(return (@some X))` nests a Maybe while `(return-from (@some X))` does not.
\\: Consecutive lifted computations run from left to right, discard earlier
\\: present results, and stop at the first `@none`; the last present result is
\\: returned. Empty computations produce `@none`.
\\:
\\: Ordinary `let`, `effect`, and computation-body `if` forms are supported.
\\: `effect` discards the result of an ordinary Shen expression; even an
\\: `@none` result from that expression does not short-circuit the computation.
\\: An omitted else arm is `@none`, so a false one-armed conditional does
\\: short-circuit any following statements.
\\:
\\: `maybe.do` does not support `for` or applicative `and` bindings. See the
\\: `cexpr` guide for the shared structured syntax.

(define maybe.cexpr-builder
  { (list sexp) --> sexp }
  []                            -> [@none]
  [bind           Expr F]       -> [maybe.bind Expr F]
  [return         Expr]         -> [@some Expr]
  [yield          Expr]         -> [@some Expr]
  [return-from    Expr]         -> Expr
  [yield-from     Expr]         -> Expr
  [combine        CX1    CX2]   -> (let Ignored (gensym (protect Ignored))
                                     [maybe.bind CX1 [/. Ignored CX2]])
  [bind-return    Expr   F]     -> [maybe.map F Expr]
  [delay          Expr]         -> Expr
  [run            Expr]         -> Expr
  Other                         -> (cexpr.default-builder maybe Other))

(defcexpr maybe.do maybe.cexpr-builder)
