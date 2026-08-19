\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Nullable values
\\:
\\: A `(nullable.t A)` is either `(@null)` or `(@just Value)`. The null case
\\: uses a private sentinel, while `@just` represents `Value` directly without
\\: allocating a wrapper. Consequently nested nullable layers collapse: this
\\: type is intended for APIs where one distinguished absence value is enough.
\\:
\\: Require the module with `(library.use [nullable])`. Present values retain
\\: their ordinary representation, so they need no extraction operation. Use
\\: the programmable `@null` and `@just` patterns when both cases must be
\\: handled explicitly:
\\:
\\: [source,shen]
\\: ----
\\: (define nullable.default
\\:   Default (@null) -> Default
\\:   _ (@just Value) -> Value)
\\: ----
\\:
\\: == API

(package nullable [@just @null null? defpattern]

(datatype t-internal
  ________________
  @null_value_ : (- (t A));)

(datatype t
  X : A;
  ________________
  X : (- (t A));

  X : (t A);
  ________________
  (not (null? X)) : verified >> X : A;

  \\ Pattern matching
  ______________
  (@null) : (t A);

  X : A;
  ==============
  (@just X) : (t A);)

\\: `(@null)` returns the distinguished null value.
(define @null
  { --> (t A) }
  -> @null_value_)

\\: `(@just Value)` injects `Value` into the nullable representation. The value
\\: is represented directly, so construction does not allocate a wrapper; the
\\: nesting caveat described above still applies to the private null sentinel:
\\: `(@just (@null))` is the same value as `(@null)`.
(define @just
  { A --> (t A) }
  X -> X)

\\: `(null? Value)` returns `true` exactly when `Value` is `(@null)`.
(define null?
  { (t A) --> boolean }
  X -> (= X @null_value_))

\\: == Pattern matching
\\:
\\: `(@null)` matches the null sentinel. `(@just Pattern)` matches any
\\: non-null value and continues matching `Pattern` against that value.
\\:
\\: [source,shen]
\\: ----
\\: (nullable.default "unknown" (@just "Ada"))
\\: \\ Result: "Ada"
\\: ----

(defpattern nullable.pattern-handler
  Self Is? Assign [@null]   -> (do (Is? [null? Self])
                                   handled)
  Self Is? Assign [@just X] -> (do (Is? [not [null? Self]])
                                   (Assign X Self)
                                   handled))

(preclude [t-internal])

)
