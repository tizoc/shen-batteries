\\ Copyright (c) 2026 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define maybe.cexpr-bind
  { (maybe.t A) --> (A --> (maybe.t B)) --> (maybe.t B) }
  M F -> (F (maybe.unsafe-get M)) where (maybe.some? M)
  _ _ -> (@none))

(define maybe.cexpr-builder
  { (list sexp) --> sexp }
  []                            -> [@none]
  [bind           Expr F]       -> [maybe.cexpr-bind Expr F]
  [return         Expr]         -> [@some Expr]
  [yield          Expr]         -> [@some Expr]
  [return-from    Expr]         -> Expr
  [yield-from     Expr]         -> Expr
  [combine        CX1    CX2]   -> (let Ignored (gensym (protect Ignored))
                                     [maybe.cexpr-bind CX1 [/. Ignored CX2]])
  [bind-return    Expr   F]     -> [maybe.map F Expr]
  [delay          Expr]         -> Expr
  [run            Expr]         -> Expr
  Other                         -> (cexpr.default-builder maybe Other))

(defcexpr maybe.do maybe.cexpr-builder)
