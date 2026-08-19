\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Computation expressions
\\:
\\: This library (inspired by the F# feature of the same name) provides a small
\\: mini-language to express composable computations with custom control flow.
\\:
\\: Computation expressions can express monads, monoids, monad transformers,
\\: and applicative functors.
\\:
\\: == Overview
\\:
\\: A computation module declares a statically named frontend with
\\: `(defcexpr Frontend Builder)`. For example,
\\: `(defcexpr seq.do seq.cexpr-builder)` defines `seq.do` as the frontend for
\\: the ordinary `seq.cexpr-builder` function. The declaration creates only a
\\: shallow outer macro; the ordinary, typed `cexpr` functions parse and lower
\\: the complete body.
\\:
\\: Frontends are namespaced and fixed by their defining source. There is no
\\: runtime builder registry or generic `:` dispatcher.
\\:
\\: A body is a sequence of parenthesized statements:
\\:
\\: * `(bind X Computation)` binds the result of a computation.
\\: * `(for X Source)` invokes the builder's `for` operation.
\\: * `(let X Expression)` makes an ordinary local Shen binding.
\\: * `(then Computation)` runs a computation and discards its result.
\\: * `(effect Expression)` evaluates an ordinary Shen expression and discards
\\:   its result.
\\: * `(return X)` and `(yield X)` lift a value; `(return-from M)` and
\\:   `(yield-from M)` use an existing computation.
\\: * `(if P Then Else)` selects between computation bodies. The else arm may
\\:   be omitted. Use Shen `do` with parenthesized statements for a
\\:   multi-statement arm.
\\: * `(and (bind X MX) (bind Y MY) ...)` combines independent computations
\\:   applicatively and binds all of their results for the remaining body.
\\:
\\: Lifted values and conditional results may be followed by more statements;
\\: the builder combines them with the delayed remainder of the body. At the
\\: outer boundary, `cexpr.expand` invokes `Run(Delay(Body))` exactly once.
\\:
\\: A declared frontend is guaranteed to be available to following source
\\: files and dependent modules, not to the source file containing its
\\: declaration. In a package, list the frontend name and any non-core syntax
\\: words used by the body among the package's external symbols. `bind`,
\\: `let`, `return`, `if`, `do`, and `and` are already Shen external symbols;
\\: `for`, `then`, `effect`, `return-from`, `yield`, and `yield-from` are not.
\\:
\\: == Examples
\\:
\\: The `maybe/cexpr` module provides dependent optional computations without
\\: nested lambdas:
\\:
\\: [source,shen]
\\: ----
\\: (maybe.do
\\:   (bind User (find-user UserId))
\\:   (bind Address (primary-address User))
\\:   (return (city Address)))
\\: ----
\\:
\\: The `seq/cexpr` module exposes both monadic binding and generator forms:
\\:
\\: [source,shen]
\\: ----
\\: (seq.do
\\:   (for X [1 2 3])
\\:   (if (> X 1)
\\:       (do (yield X)
\\:           (yield (* X 10))))
\\:   (yield done))
\\: ----
\\:
\\: The meaning of each form belongs to the builder. For example, the `seq`
\\: builder interprets applicative sources as a shortest-sequence zip, while
\\: ordinary consecutive `bind` statements are dependent `flat-map`
\\: operations.
\\:
\\: == Implementing new computation expression types
\\:
\\: A builder is an ordinary checked Shen function with type
\\: `(list sexp) --> sexp`. It receives an operation represented as source
\\: data and returns the Shen source tree that implements it. Declare the
\\: public frontend in the builder's source with, for example,
\\: `(defcexpr result.do result.cexpr-builder)`.
\\:
\\: A builder declared inside a Shen package uses a qualified frontend name,
\\: such as `result.do`, and lists `defcexpr`, any referenced `cexpr.*`
\\: helpers, and the non-core operation tags it pattern-matches as external
\\: symbols. This keeps its source-data patterns identical to the operations
\\: emitted by the common expander. A bare `do` remains Shen's core external
\\: symbol and is therefore not package-qualified.
\\:
\\: The common expander can send these operations:
\\:
\\: * `[]` for an empty computation (Zero).
\\: * `[bind M F]` and `[for Source F]`.
\\: * `[return X]`, `[return-from M]`, `[yield X]`, and `[yield-from M]`.
\\: * `[combine Left DelayedRight]`.
\\: * `[bind-return M F]`, an optional optimized Bind followed by Return.
\\: * `[merge-sources M1 M2 ...]` for applicative `and` bindings. The merged
\\:   result is attached to the remaining body with Bind, or with BindReturn
\\:   when the remainder is a single Return and the optimization is supported.
\\: * `[delay M]` and `[run M]`.
\\:
\\: Delegate unknown operations to `cexpr.default-builder`. It supplies
\\: identity implementations of Delay and Run. It reports `bind-return` as
\\: unsupported with Shen's `fail`, allowing the expander to use ordinary Bind
\\: and Return instead for both single and applicative bindings; an actual
\\: error raised by a builder is never swallowed. Other unsupported operations
\\: produce a descriptive error.
\\:
\\: `merge-sources` must preserve source order. Two results are represented as
\\: `(@p A B)`; three or more are right-nested, as in `(@p A (@p B C))`. The
\\: expander generates a hygienic one-argument lambda which destructures that
\\: representation, so applicative builders work with any number of sources. A
\\: builder may still recognize the generated BindReturn shape to provide
\\: specialized lowering.
\\:
\\: Delay is also applied to the remainder supplied to Combine. The single
\\: outer Run/Delay pair is separate: it lets a builder suspend, start, or
\\: finalize the complete computation without finalizing nested bodies.

(defmacro cexpr.defcexpr-macro
  [defcexpr Frontend Builder]
    -> (let Internal (intern (@s (str Frontend) ".cexpr-macro"))
         [defmacro Internal
           [cons Frontend (protect Body)]
             -> [cexpr.expand [fn Builder] (protect Body)]])
       where (and (and (symbol? Frontend) (not (variable? Frontend)))
                  (and (symbol? Builder) (not (variable? Builder))))
  [defcexpr | Declaration]
    -> (error "invalid defcexpr declaration ~R"
              [defcexpr | Declaration]))

(define cexpr.default-builder
  { symbol --> (list sexp) --> sexp }
  _    [bind-return _ _] -> (fail)
  _    [delay Expr] -> Expr
  _    [run   Expr] -> Expr
  Name Expr         -> (error "~A computation expressions do not support ~R" Name Expr))

(define cexpr.build-combine
  { ((list sexp) --> sexp) --> sexp --> (list sexp) --> sexp }
  _  CExp [] -> CExp
  Mk CExp Rest -> (Mk [combine CExp (Mk [delay (cexpr.build Mk Rest)])]))

(define cexpr.build-branch
  { ((list sexp) --> sexp) --> sexp --> sexp }
  Mk Branch -> (cexpr.build Mk (cexpr.body-statements Branch)))

(define cexpr.body-statements
  { sexp --> (list sexp) }
  [do First Rest]
    -> [First | (cexpr.body-statements Rest)]
  Statement -> [Statement])

\\ TODO:
\\ - handle exceptions
\\ - allow custom extensions?
(define cexpr.build-monadic
  { ((list sexp) --> sexp) --> (list sexp) --> sexp }
  Mk []                              -> (Mk [])
  Mk [[then Do]                | Rest] -> (let X (gensym (protect X))
                                            (Mk [bind Do [/. X (cexpr.build Mk Rest)]]))
  Mk [[effect Effect]          | Rest] -> (let X (gensym (protect X))
                                            [let X Effect (cexpr.build Mk Rest)])
  Mk [[return      Expr]       | Rest] -> (cexpr.build-combine Mk (Mk [return Expr])      Rest)
  Mk [[return-from Expr]       | Rest] -> (cexpr.build-combine Mk (Mk [return-from Expr]) Rest)
  Mk [[yield       Expr]       | Rest] -> (cexpr.build-combine Mk (Mk [yield Expr])       Rest)
  Mk [[yield-from  Expr]       | Rest] -> (cexpr.build-combine Mk (Mk [yield-from Expr])  Rest)
  Mk [[if P Then]              | Rest] -> (cexpr.build-combine Mk [if P (cexpr.build-branch Mk Then)
                                                                        (cexpr.build Mk [])]
                                                                   Rest)
  Mk [[if P Then Else]         | Rest] -> (cexpr.build-combine Mk [if P (cexpr.build-branch Mk Then)
                                                                        (cexpr.build-branch Mk Else)]
                                                                   Rest)
  Mk [[bind Var Expr]          | Rest] -> (Mk [bind Expr [/. Var (cexpr.build Mk Rest)]])
    where (variable? Var)
  Mk [[for Var Expr]           | Rest] -> (Mk [for Expr [/. Var (cexpr.build Mk Rest)]])
    where (variable? Var)
  Mk [[let Var Expr]           | Rest] -> [let Var Expr (cexpr.build Mk Rest)]
    where (variable? Var)
  Mk Other                              -> (error "invalid computation expression ~R" Other))

(define cexpr.collect-bindings
  { sexp --> (list (sexp * sexp)) }
  [and Left Right] -> (append (cexpr.collect-bindings Left)
                              (cexpr.collect-bindings Right))
  [bind Var Expr]  -> [(@p Var Expr)]
    where (variable? Var)
  Other            -> (error "invalid applicative binding ~R" Other))

(define cexpr.merge-lambda
  { (list sexp) --> sexp --> sexp }
  Vars Body -> (let Tuple (gensym (protect Tuple))
                 [/. Tuple (cexpr.bind-merged Vars Tuple Body)]))

(define cexpr.bind-merged
  { (list sexp) --> sexp --> sexp --> sexp }
  [V1 V2] Expr Body -> [let V1 [fst Expr]
                         [let V2 [snd Expr]
                           Body]]
  [V | Vs] Expr Body -> (let Tail (gensym (protect Tail))
                          [let V [fst Expr]
                            [let Tail [snd Expr]
                              (cexpr.bind-merged Vs Tail Body)]])
  Vars _ _ -> (error "cannot bind an applicative pattern from ~R" Vars))

(define cexpr.build-bind-return
  { ((list sexp) --> sexp) --> sexp --> sexp --> sexp --> sexp }
  Mk Var Expr Return -> (Mk [bind-return Expr [/. Var Return]])
    where (variable? Var)
  _ _ _ _ -> (fail))

(define cexpr.build-merged
  { ((list sexp) --> sexp) --> sexp --> (list sexp) --> (list sexp) --> sexp }
  Mk Merged Vars [[return Body]]
    <- (Mk [bind-return Merged (cexpr.merge-lambda Vars Body)])
  Mk Merged Vars Rest
    -> (Mk [bind Merged
                  (cexpr.merge-lambda Vars (cexpr.build Mk Rest))]))

(define cexpr.build-applicative
  { ((list sexp) --> sexp) --> (list sexp) --> sexp }
  Mk [[bind Var Expr] [return Return]]
    <- (cexpr.build-bind-return Mk Var Expr Return)
  Mk [[and Left Right] | Rest]
    -> (let Bindings (cexpr.collect-bindings [and Left Right])
            Vars (map (fn fst) Bindings)
            Exprs (map (fn snd) Bindings)
            Merged (Mk [merge-sources | Exprs])
         (cexpr.build-merged Mk Merged Vars Rest))
  Mk Expr -> (fail))

(define cexpr.build
  { ((list sexp) --> sexp) --> (list sexp) --> sexp }
  Mk Expr <- (cexpr.build-applicative Mk Expr)
  Mk Expr -> (cexpr.build-monadic Mk Expr))

\\ Apply the builder's root boundary exactly once. Recursive computation
\\ expression translation continues to use cexpr.build directly.
(define cexpr.expand
  { ((list sexp) --> sexp) --> (list sexp) --> sexp }
  Mk CExpr -> (Mk [run (Mk [delay (cexpr.build Mk CExpr)])]))
