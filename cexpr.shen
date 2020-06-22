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
  (function (get cexpr.builders N)) : ((list sexp) --> sexp);)

(define cexpr.register
  { symbol --> symbol --> void }
  Name FName -> (put cexpr.builders Name FName))

(define cexpr.unregister
  { symbol --> void }
  Name -> (unput cexpr.builders Name))

(define cexpr.builder
  { symbol --> ((list sexp) --> sexp) }
  Name -> (trap-error
            (function (get cexpr.builders Name))
            (/. _ (error "Unknown cexpr: ~A" Name))))

(define cexpr.default-builder
  { symbol --> (list sexp) --> sexp }
  _    [delay Expr] -> Expr
  _    [run   Expr] -> Expr
  Name Expr         -> (error "~A computation expressions do not support ~R" Name Expr))

(define cexpr.build-combine
  { ((list sexp) --> sexp) --> sexp --> (list sexp) --> sexp }
  _  CExp [] -> CExp
  Mk CExp Rest -> (Mk [combine CExp (Mk [delay (cexpr.build Mk Rest)])]))

\\ TODO:
\\ - handle exceptions
\\ - allow custom extensions?
(define cexpr.build-monadic
  { ((list sexp) --> sexp) --> (list sexp) --> sexp }
  Mk [do <--      Do       | Rest] -> (cexpr.build Mk [_ <-- Do | Rest])
  Mk [do          Do       | Rest] -> (cexpr.build Mk [_     Do | Rest])
  Mk [Arr         Expr           ] -> (Mk [return Expr]) where (= Arr ->)
  Mk [return      Expr     | Rest] -> (cexpr.build-combine Mk (Mk [return Expr])             Rest)
  Mk [return-from Expr     | Rest] -> (cexpr.build-combine Mk (Mk [return-from Expr])        Rest)
  Mk [yield       Expr     | Rest] -> (cexpr.build-combine Mk (Mk [yield Expr])              Rest)
  Mk [yield-from  Expr     | Rest] -> (cexpr.build-combine Mk (Mk [yield-from Expr])         Rest)
  Mk [[if P [T|Ts]       ] | Rest] -> (cexpr.build-combine Mk [if P (Mk [T|Ts]) (Mk [])]     Rest)
  Mk [[if P [T|Ts] [E|Es]] | Rest] -> (cexpr.build-combine Mk [if P (Mk [T|Ts]) (Mk [E|Es])] Rest)
  Mk [Var <--     Expr     | Rest] -> (Mk [bind Expr [/. Var (cexpr.build Mk Rest)]])
  Mk [Var <==     Expr     | Rest] -> (Mk [for Expr [/. Var (cexpr.build Mk Rest)]])
  Mk [Var         Expr     | Rest] -> [let Var Expr (cexpr.build Mk Rest)]
  Mk Other                         -> (error "invalid computation expression ~R" Other))

(define fail-catch
  { (lazy sexp) --> sexp }
  L -> (trap-error (thaw L) (/. _ (fail))))

(define cexpr.collect-bindings
  { (list sexp) --> (list (sexp * sexp)) --> ((list (sexp * sexp)) * sexp) }
  [Var <-- Expr and | Rest]  Acc -> (cexpr.collect-bindings Rest (append Acc [(@p Var Expr)]))
  [Var <-- Expr return Body] Acc -> (@p (append Acc [(@p Var Expr)]) Body)
  Other                      _   -> (error "invalid applicative computation expression ~R" Other))

\\ TODO: revise and verify all this
(define cexpr.build-applicative
  { ((list sexp) --> sexp) --> (list sexp) --> sexp }
  Mk [Var <-- Expr return Return] <- (fail-catch (freeze (Mk [bind-return Expr [/. Var Return]])))
  Mk [Var <-- Expr and | Rest] -> (let (@p Bindings Body) (cexpr.collect-bindings [Var <-- Expr and | Rest] [])
                                       Vars (map (function fst) Bindings)
                                       Exprs (map (function snd) Bindings)
                                    (Mk [bind-return (Mk [merge-sources | Exprs])
                                                     [/. [@p | Vars] Body]]))
  Mk Expr -> (fail))

(define cexpr.build
  { ((list sexp) --> sexp) --> (list sexp) --> sexp }
  Mk Expr <- (cexpr.build-applicative Mk Expr)
  Mk Expr -> (cexpr.build-monadic Mk Expr))

(defmacro cexpr.macro
  [:CX | CExpr] -> (cexpr.build (cexpr.builder CX) CExpr))

(preclude [cexpr.t-internal])
