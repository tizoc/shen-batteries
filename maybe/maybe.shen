\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Optional values
\\:
\\: `(maybe.t A)` represents either a present value, constructed with
\\: `(@some Value)`, or absence, represented by `(@none)`. Unlike `nullable`,
\\: `@some` is tagged, so nested Maybe values remain distinct.
\\:
\\: Require the module with `(library.use [maybe])`. The constructors also act
\\: as programmable patterns:
\\:
\\: [source,shen]
\\: ----
\\: (define maybe.default
\\:   Default (@none) -> Default
\\:   _ (@some Value) -> Value)
\\: ----

(package maybe [@some @none void defpattern]

(datatype t-internal
  ______________
  @none_value_ : (- (t A));

  ______________
  (absvector 2) : (- (t A));

  MaybeX : (t A);
  ______________
  (address-> MaybeX 0 #tag#some) : (t A);

  MaybeX : (t A);
  X : A;
  ______________
  (address-> MaybeX 1 X) : (t A);

  (absvector? X) : verified;
  ______________
  (= (<-address X 0) #tag#some) : boolean;

  MaybeX : (t A);
  ______________
  (<-address MaybeX 1) : A;)

(datatype t
  \\ For pattern matching
  ______________
  (@none) : (t A);

  X : A;
  ==============
  (@some X) : (t A);)

\\: == Construction

\\: `(@none)` returns the shared value representing absence.
(define @none
  { --> (t A) }
  -> @none_value_)

(define init-@some
  { (t A) --> A --> (t A) }
  M X -> (address-> (address-> M 0 #tag#some) 1 X))

\\: `(@some Value)` returns a tagged Maybe containing `Value`. The tag means
\\: `(@some (@none))` remains different from `(@none)`.
(define @some
  { A --> (t A) }
  X -> (init-@some (absvector 2) X))

\\: == Predicates

\\: `(maybe.none? Maybe)` returns `true` exactly when `Maybe` is `(@none)`.
(define none?
  { (t A) --> boolean }
  X -> (= X @none_value_))

\\: `(maybe.some? Maybe)` returns `true` exactly when the typed Maybe argument
\\: is an `@some` value. Like `maybe.none?`, it is a predicate over
\\: `(maybe.t A)`, not a run-time type test for arbitrary Shen values.
(define maybe.some?
  { (t A) --> boolean }
  X -> (not (none? X)))

\\ `some-representation?` safely rejects unrelated Shen values. Programmable
\\ patterns can appear in polymorphic definitions, so their run-time
\\ discriminator must not assume that the scrutinee already has a `maybe.t`
\\ type. Keep the public `maybe.some?` predicate specialized to typed Maybe
\\ values.
(define some-representation?
  { A --> boolean }
  X -> (trap-error (= (<-address X 0) #tag#some) (/. Error false))
    where (absvector? X)
  _ -> false)

\\: == Extraction

\\: `(maybe.get Maybe)` returns the value inside `(@some Value)` and raises an
\\: error for `(@none)`.
(define maybe.get
  { (t A) --> A }
  X -> (<-address X 1) where (maybe.some? X)
  _ -> (error "Not a @some value"))

\\: `(maybe.unsafe-get Maybe)` returns the value inside an `@some` without
\\: checking its tag. Call it only after `maybe.some?` has established that the
\\: value is present; applying it to `@none` is invalid.
(define unsafe-get
  { (t A) --> A }
  X -> (<-address X 1))

\\: `(maybe.get/or Maybe FrozenDefault)` returns the present value, or thaws
\\: `FrozenDefault` when `Maybe` is `@none`. The default is not evaluated for an
\\: `@some`, as in `(maybe.get/or Maybe (freeze (make-default)))`.
(define get/or
  { (t A) --> (lazy A) --> A }
  X _ -> (<-address X 1) where (maybe.some? X)
  _ F -> (thaw F))

\\: == Transformation

\\: `(maybe.map F Maybe)` returns `(@some (F Value))` for an `@some` and
\\: returns `@none` without calling `F` otherwise.
(define maybe.map
  { (A --> B) --> (t A) --> (t B) }
  F X -> (@some (F (maybe.get X))) where (maybe.some? X)
  _ X -> (@none))

\\: `(maybe.bind Maybe F)` calls `F` with the value inside `Maybe` when it is
\\: `(@some Value)`, and returns `(@none)` without calling `F` otherwise.
\\: `F` itself returns a Maybe, so `bind` does not add another `@some` layer.
\\: The value-first argument order follows the monadic bind convention; use
\\: `maybe.map` for a function-first non-monadic transformation.
(define maybe.bind
  { (t A) --> (A --> (t B)) --> (t B) }
  Maybe F -> (F (unsafe-get Maybe)) where (maybe.some? Maybe)
  _ _ -> (@none))

\\: == Effects

\\: `(maybe.for-each F Maybe)` calls `F` once with the present value and
\\: discards its result. It does not call `F` for `@none`; both cases return
\\: `void`.
(define for-each
  { (A --> B) --> (t A) --> void }
  F X -> (do (F (maybe.get X))
             (void))
      where (maybe.some? X)
  _ X -> (void))

(define #tag#some
  { (t A) --> string }
  X -> (make-string "(@some ~S)" (unsafe-get X)))

\\: == Pattern matching
\\:
\\: `(@none)` matches absence. `(@some Pattern)` matches a present value and
\\: continues matching `Pattern` against its contents. These patterns work in
\\: definitions and other contexts supported by Shen's programmable pattern
\\: matching. Unlike the typed predicates above, the `@some` pattern safely
\\: declines an unrelated scrutinee so a later fallback clause can match it.

(defpattern maybe.pattern-handler
  Self Is? Assign [@none]   -> (do (Is? [none? Self])
                                   handled)
  Self Is? Assign [@some X] -> (do (Is? [some-representation? Self])
                                   (Assign X [unsafe-get Self])
                                   handled))

(preclude [t-internal])

)
