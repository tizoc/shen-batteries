\\ Copyright (c) 2020 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define seq.cexpr-builder
  { any.t --> any.t }
  [] -> [seq.empty]
  [for Expr F] -> [seq.flat-map F [seq.of Expr]]
  [bind Expr F] -> [seq.flat-map F Expr]
  [return Expr] -> [seq.singleton Expr]
  [return-from Expr] -> Expr
  [yield Expr] -> [seq.singleton Expr]
  [yield-from Expr] -> Expr
  Other -> (error "seq computation expressions do not support ~R" Other))

(cexpr.register seq seq.cexpr-builder)
