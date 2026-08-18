\\ Copyright (c) 2020 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Sequence computation expressions
\\:
\\: Require `(library.use [seq/cexpr])` to define the `seq.do` frontend.
\\: It provides structured sequence comprehensions and generators.
\\:
\\: [source,shen]
\\: ----
\\: (seq.do
\\:   (for X [1 2 3])
\\:   (if (> X 1)
\\:       (do (yield X)
\\:           (yield (* X 10)))))
\\: ----
\\:
\\: `bind` flat-maps an existing sequence. `for` first converts a list or
\\: vector with `seq.of`, then flat-maps it. `return` and `yield` produce a
\\: singleton; `return-from` and `yield-from` splice an existing sequence.
\\: Consecutive produced computations are appended, and an empty computation
\\: produces `seq.empty`.
\\:
\\: Applicative `(and (bind X XS) (bind Y YS) ...)` bindings zip independent
\\: sources in order and stop at the shortest source. A terminal `return` is
\\: lowered to `seq.map`, `seq.map2`, or `seq.map3` when possible. Ordinary
\\: consecutive `bind` forms remain dependent flat-map operations rather than
\\: zip operations.
\\:
\\: Ordinary `let`, `effect`, and computation-body `if` forms are supported.
\\: `then` is a discarded sequence bind: the remainder runs once for every
\\: value produced by its input, and does not run when that input is empty.
\\: Thus `(then (seq.of-list [a b])) (yield x)` produces `[x x]`. See the
\\: `cexpr` guide for the shared structured syntax.

(define seq.cexpr-builder
  { (list sexp) --> sexp }
  []                            -> [seq.empty]
  [for            Expr F]       -> [seq.flat-map F [seq.of Expr]]
  [bind           Expr F]       -> [seq.flat-map F Expr]
  [return         Expr]         -> [seq.singleton Expr]
  [yield          Expr]         -> [seq.singleton Expr]
  [return-from    Expr]         -> Expr
  [yield-from     Expr]         -> Expr
  [combine        CX1    CX2]   -> [seq.append CX1 CX2]
  [bind-return    Expr   F]     -> (seq.cexpr-builder-bind-return F Expr)
  [merge-sources S1 S2]         -> [seq.zip S1 S2]
  [merge-sources S1 S2 | More]  -> [seq.zip S1 (seq.cexpr-builder [merge-sources S2 | More])]
  Other                         -> (cexpr.default-builder seq Other))

(define seq.cexpr-builder-bind-return
  { sexp --> sexp --> sexp }
  [/. Tuple
      [let V1 [fst Tuple]
        [let Tail [snd Tuple]
          [let V2 [fst Tail]
            [let V3 [snd Tail] Body]]]]]
    [seq.zip S1 [seq.zip S2 S3]]
      -> [seq.map3 [/. V1 V2 V3 Body] S1 S2 S3]
  [/. Tuple
      [let V1 [fst Tuple]
        [let V2 [snd Tuple] Body]]]
    [seq.zip S1 S2]
      -> [seq.map2 [/. V1 V2 Body] S1 S2]
  F Expr -> [seq.map F Expr])

(defcexpr seq.do seq.cexpr-builder)
