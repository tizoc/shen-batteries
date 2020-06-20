\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Computation expressions
\\:
\\: This library (inspired by the F# feature of the same name) provides small
\\: mini-language to express composable computations with custom control flow.
\\:
\\: Computation expressions can express monads, monoids, monad transformers,
\\: and applicative functors.
\\:
\\: == Overview
\\:
\\: TODO
\\:
\\: == Examples
\\:
\\: TODO
\\:
\\: == Implementing new computation expression types
\\:
\\: TODO

(datatype cexpr.t-internal
  ___________________
  (put cexpr.builders N F) : void;

  ___________________
  (unput cexpr.builders N) : void;

  ___________________
  (function (get cexpr.builders N)) : (sexp --> sexp);)

(define cexpr.register
  { symbol --> symbol --> void }
  Name FName -> (put cexpr.builders Name FName))

(define cexpr.unregister
  { symbol --> void }
  Name -> (unput cexpr.builders Name))

(define cexpr.builder
  { symbol --> (sexp --> sexp) }
  Name -> (trap-error
            (function (get cexpr.builders Name))
            (/. _ (error "Unknown cexpr: ~A" Name))))

(define cexpr.default-builder
  { symbol --> sexp --> sexp }
  _ [delay Expr] -> Expr
  _ [run Expr] -> Expr
  Name Expr -> (error "~A computation expressions do not support ~R" Name Expr))

\\ TODO:
\\ - handle exceptions
(defmacro cexpr.macro
  [:CX]                  -> ((cexpr.builder CX) [])
  [:CX do <-- Do | Rest] -> [:CX _ <-- Do | Rest]
  [:CX do Do | Rest]     -> [:CX _ Do | Rest]
  [:CX Arr Expr]         -> ((cexpr.builder CX) [return Expr]) where (= Arr ->)

  \\ As last expression
  [:CX return      Expr]  -> ((cexpr.builder CX) [return Expr])
  [:CX return-from Expr]  -> ((cexpr.builder CX) [return-from Expr])
  [:CX yield       Expr]  -> ((cexpr.builder CX) [yield Expr])
  [:CX yield-from  Expr]  -> ((cexpr.builder CX) [yield-from Expr])
  [:CX [if Test    Then]] -> (let Builder (cexpr.builder CX)
                               [if Test (Builder Then) (Builder [])])
  [:CX [if Test Then Else]] -> (let Builder (cexpr.builder CX)
                                 [if Test (Builder Expr) (Builder Else)])

  \\ As joining expression
  [:CX return Expr      | Rest] -> (let Builder (cexpr.builder CX)
                                     (Builder [combine (Builder [return Expr])
                                                       (Builder [delay [:CX | Rest]])]))
  [:CX return-from Expr | Rest] -> (let Builder (cexpr.builder CX)
                                     (Builder [combine (Builder [return-from Expr])
                                                       (Builder [delay [:CX | Rest]])]))
  [:CX yield Expr       | Rest] -> (let Builder (cexpr.builder CX)
                                     (Builder [combine (Builder [yield Expr])
                                                       (Builder [delay [:CX | Rest]])]))
  [:CX yield-from Expr  | Rest] -> (let Builder (cexpr.builder CX)
                                     (Builder [combine (Builder [yield-from Expr])
                                                       (Builder [delay [:CX | Rest]])]))
  [:CX [if Test Then]   | Rest] -> (let Builder (cexpr.builder CX)
                                     (Builder [combine [if Test (Builder Then) (Builder [])]
                                                       (Builder [delay [:CX | Rest]])]))
  [:CX [if Test Then Else] | Rest] -> (let Builder (cexpr.builder CX)
                                        (Builder [combine [if Test (Builder Then) (Builder Else)]
                                                          (Builder [delay [:CX | Rest]])]))


  [:CX Var <-- Expr | Rest] -> ((cexpr.builder CX) [bind Expr [/. Var [:CX | Rest]]])
  [:CX Var <== Expr | Rest] -> ((cexpr.builder CX) [for Expr [/. Var [:CX | Rest]]])
  [:CX Var Expr     | Rest] -> [let Var Expr [:CX | Rest]]
  [:CX | Other]             -> (error "invalid computation expression ~R" Other))

(preclude [cexpr.t-internal])
