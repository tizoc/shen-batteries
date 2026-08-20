\\ Copyright (c) 2026 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Result values
\\:
\\: `(result.t E A)` represents either success, constructed with
\\: `(@ok Value)`, or failure, constructed with `(@err Error)`. The error type
\\: comes first so `map` and `bind` preserve the leading error parameter while
\\: changing the success type.
\\:
\\: Require the module with `(library.use [result])`. Both constructors also
\\: act as programmable patterns:
\\:
\\: [source,shen]
\\: ----
\\: (define result.message
\\:   (@ok Value) -> (make-string "value: ~S" Value)
\\:   (@err Error) -> (make-string "error: ~S" Error))
\\: ----

(package result [@ok @err defpattern]

(datatype t-internal
  ______________
  (absvector 3) : (- (t E A));

  Result : (t E A);
  ______________
  (address-> Result 0 #tag#ok) : (t E A);

  Result : (t E A);
  ______________
  (address-> Result 0 #tag#err) : (t E A);

  Result : (t E A);
  X : A;
  ______________
  (address-> Result 1 X) : (t E A);

  Result : (t E A);
  X : E;
  ______________
  (address-> Result 2 X) : (t E A);

  (absvector? X) : verified;
  ______________
  (= (<-address X 0) #tag#ok) : boolean;

  (absvector? X) : verified;
  ______________
  (= (<-address X 0) #tag#err) : boolean;

  Result : (t E A);
  ______________
  (<-address Result 1) : A;

  Result : (t E A);
  ______________
  (<-address Result 2) : E;)

(datatype t
  X : A;
  ==============
  (@ok X) : (t E A);

  X : E;
  ==============
  (@err X) : (t E A);)

\\: == Construction

(define init-@ok
  { (t E A) --> A --> (t E A) }
  Result X -> (address-> (address-> Result 0 #tag#ok) 1 X))

\\: `(@ok Value)` returns a successful Result containing `Value`.
(define @ok
  { A --> (t E A) }
  X -> (init-@ok (absvector 3) X))

(define init-@err
  { (t E A) --> E --> (t E A) }
  Result X -> (address-> (address-> Result 0 #tag#err) 2 X))

\\: `(@err Error)` returns a failed Result containing `Error`.
(define @err
  { E --> (t E A) }
  X -> (init-@err (absvector 3) X))

(define ok-representation?
  { A --> boolean }
  X -> (trap-error (= (<-address X 0) #tag#ok) (/. Error false))
    where (absvector? X)
  _ -> false)

(define err-representation?
  { A --> boolean }
  X -> (trap-error (= (<-address X 0) #tag#err) (/. Error false))
    where (absvector? X)
  _ -> false)

\\: == Predicates

\\: `(result.ok? Result)` returns `true` exactly when the typed Result is an
\\: `@ok` value. It is a predicate over `(result.t E A)`, not a run-time type
\\: test for arbitrary Shen values.
(define result.ok?
  { (t E A) --> boolean }
  Result -> (ok-representation? Result))

\\: `(result.err? Result)` returns `true` exactly when the typed Result is an
\\: `@err` value. It is a predicate over `(result.t E A)`, not a run-time type
\\: test for arbitrary Shen values.
(define result.err?
  { (t E A) --> boolean }
  Result -> (err-representation? Result))

(define ok-value
  { (t E A) --> A }
  Result -> (<-address Result 1))

(define err-value
  { (t E A) --> E }
  Result -> (<-address Result 2))

\\: == Elimination

\\: `(result.fold OnErr OnOk Result)` calls `OnErr` with the error from an
\\: `@err`, or `OnOk` with the value from an `@ok`. Exactly one handler is
\\: called, making `fold` the total way to extract a value from either case.
(define result.fold
  { (E --> B) --> (A --> B) --> (t E A) --> B }
  _ OnOk Result -> (OnOk (ok-value Result))
    where (result.ok? Result)
  OnErr _ Result -> (OnErr (err-value Result))
    where (result.err? Result))

\\: == Transformation

\\: `(result.map F Result)` applies `F` to an `@ok` value and leaves an
\\: `@err` error unchanged. `F` is not called for an `@err`.
(define result.map
  { (A --> B) --> (t E A) --> (t E B) }
  F Result -> (@ok (F (ok-value Result)))
    where (result.ok? Result)
  _ Result -> (@err (err-value Result))
    where (result.err? Result))

\\: `(result.map-error F Result)` applies `F` to an `@err` error and leaves an
\\: `@ok` value unchanged. `F` is not called for an `@ok`.
(define result.map-error
  { (E --> F) --> (t E A) --> (t F A) }
  _ Result -> (@ok (ok-value Result))
    where (result.ok? Result)
  F Result -> (@err (F (err-value Result)))
    where (result.err? Result))

\\: `(result.bind Result F)` calls `F` with the value inside an `@ok` and
\\: returns its Result directly. For an `@err`, it preserves the error without
\\: calling `F`. The value-first argument order follows the monadic bind
\\: convention used by `maybe.bind`.
(define result.bind
  { (t E A) --> (A --> (t E B)) --> (t E B) }
  Result F -> (F (ok-value Result))
    where (result.ok? Result)
  Result _ -> (@err (err-value Result))
    where (result.err? Result))

(define #tag#ok
  { (t E A) --> string }
  Result -> (make-string "(@ok ~S)" (ok-value Result)))

(define #tag#err
  { (t E A) --> string }
  Result -> (make-string "(@err ~S)" (err-value Result)))

\\: == Pattern matching
\\:
\\: `(@ok Pattern)` matches a successful value and continues matching
\\: `Pattern` against its contents. `(@err Pattern)` does the same for a failed
\\: value. Both patterns safely decline unrelated Shen values so a later
\\: fallback clause can match them.

(defpattern result.pattern-handler
  Self Is? Assign [@ok X] -> (do (Is? [ok-representation? Self])
                                  (Assign X [ok-value Self])
                                  handled)
  Self Is? Assign [@err X] -> (do (Is? [err-representation? Self])
                                   (Assign X [err-value Self])
                                   handled))

(preclude [t-internal])

)
