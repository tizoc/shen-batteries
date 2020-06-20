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
(define cexpr.build
  { (sexp --> sexp) --> (list sexp) --> sexp }
  Mk [do <-- Do | Rest] -> (cexpr.build Mk [_ <-- Do | Rest])
  Mk [do Do | Rest]     -> (cexpr.build Mk [_ Do | Rest])
  Mk [Arr Expr]         -> (Mk [return Expr]) where (= Arr ->)

  \\ As last expression
  Mk [return      Expr]  -> (Mk [return Expr])
  Mk [return-from Expr]  -> (Mk [return-from Expr])
  Mk [yield       Expr]  -> (Mk [yield Expr])
  Mk [yield-from  Expr]  -> (Mk [yield-from Expr])
  Mk [[if Test    Then]] -> [if Test (Mk Then) (Mk [])]
  Mk [[if Test Then Else]] -> [if Test (Mk Then) (Mk Else)]

  \\ As joining expression
  Mk [return Expr      | Rest] -> (Mk [combine (Mk [return Expr])
                                               (Mk [delay (cexpr.build Mk Rest)])])
  Mk [return-from Expr | Rest] -> (Mk [combine (Mk [return-from Expr])
                                               (Mk [delay (cexpr.build Mk Rest)])])
  Mk [yield Expr       | Rest] -> (Mk [combine (Mk [yield Expr])
                                               (Mk [delay (cexpr.build Mk Rest)])])
  Mk [yield-from Expr  | Rest] -> (Mk [combine (Mk [yield-from Expr])
                                               (Mk [delay (cexpr.build Mk Rest)])])
  Mk [[if Test Then]   | Rest] -> (Mk [combine [if Test (Mk Then) (Mk [])]
                                               (Mk [delay (cexpr.build Mk Rest)])])
  Mk [[if Test Then Else] | Rest] -> (Mk [combine [if Test (Mk Then) (Mk Else)]
                                                  (Mk [delay (cexpr.build Mk Rest)])])


  Mk [Var <-- Expr | Rest] -> (Mk [bind Expr [/. Var (cexpr.build Mk Rest)]])
  Mk [Var <== Expr | Rest] -> (Mk [for Expr [/. Var (cexpr.build Mk Rest)]])
  Mk [Var Expr     | Rest] -> [let Var Expr (cexpr.build Mk Rest)]
  Mk Other                 -> (error "invalid computation expression ~R" Other))

(defmacro cexpr.macro
  [:CX | CExpr] -> (cexpr.build (cexpr.builder CX) CExpr))

(preclude [cexpr.t-internal])
