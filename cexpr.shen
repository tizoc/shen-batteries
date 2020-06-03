\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define cexpr.builder
  { symbol --> (any.t --> any.t) }
  seq -> (function seq.cexpr-builder)
  Other -> (error "Unknown cexp: ~A" Other))

\\ TODO:
\\ - handle combination of multiple cexprs
\\ - handle else-less if expression
\\ - handle exceptions
(defmacro cexpr.macro
  [:CX] -> ((cexpr.builder CX) [])
  [:CX do <-- Do | Rest] -> [:CX _ <-- Do | Rest]
  [:CX do Do | Rest] -> [:CX _ Do | Rest]
  [:CX Arr Expr] -> ((cexpr.builder CX) [return Expr]) where (= Arr ->)
  [:CX return Expr] -> ((cexpr.builder CX) [return Expr])
  [:CX return-from Expr] -> ((cexpr.builder CX) [return-from Expr])
  [:CX yield Expr] -> ((cexpr.builder CX) [yield Expr])
  [:CX yield-from Expr] -> ((cexpr.builder CX) [yield-from Expr])
  [:CX Var <-- Expr | Rest] -> ((cexpr.builder CX) [bind Expr [/. Var [:CX | Rest]]])
  [:CX Var <== Expr | Rest] -> ((cexpr.builder CX) [for Expr [/. Var [:CX | Rest]]])
  [:CX Var Expr | Rest] -> [let Var Expr [:CX | Rest]]
  [:CX | Other] -> (error "invalid computation expression ~R" Other))

\\ Computation Expression Builder

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
