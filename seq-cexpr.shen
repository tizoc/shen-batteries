\\ Copyright (c) 2020 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

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
  \\ TODO: higher counts can be handled by combining mapN with seq.zip
  [/. [@p V1 V2 V3] Body] [seq.zip S1 [seq.zip S2 S3]] -> [seq.map3 [/. V1 V2 V3 Body] S1 S2 S3]
  [/. [@p V1 V2] Body] [seq.zip S1 S2] -> [seq.map2 [/. V1 V2 Body] S1 S2]
  F Expr -> [seq.map F Expr])

(cexpr.register seq seq.cexpr-builder)
