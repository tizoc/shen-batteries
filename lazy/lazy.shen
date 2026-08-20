\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Utilities for lazy computations
\\:
\\: Shen's `freeze` creates a delayed computation and `thaw` runs it.
\\: `lazy.memo` adds successful-result caching when the same computation may
\\: be thawed more than once. Require it with `(library.use [lazy])`.
\\:
\\: == API

(package lazy [box.t box.make box.unbox box.put]

(datatype memo-state-internal
  L : (lazy A);
  ==============================
  [pending L] : (memo-state A);

  X : A;
  ============================
  [ready X] : (memo-state A);)

\\: `(lazy.memo Frozen)` returns a new lazy computation. Its first successful
\\: thaw evaluates `Frozen` and caches the result; later thaws return that
\\: result without evaluating `Frozen` again. If evaluation raises an error,
\\: nothing is cached and a later thaw retries it.
\\:
\\: [source,shen]
\\: ----
\\: (let Count (box.make 0)
\\:      Delayed (lazy.memo (freeze (do (box.incr Count) 42)))
\\:   (do (thaw Delayed)
\\:       (thaw Delayed)
\\:       (box.unbox Count)))
\\: \\ Result: 1
\\: ----
(define memo
  { (lazy A) --> (lazy A) }
  L -> (let Result (box.make [pending L])
         (freeze (memo-h Result (box.unbox Result)))))

(define memo-h
  { (box.t (memo-state A)) --> (memo-state A) --> A }
  Result [pending L] -> (let X (thaw L)
                             Update (box.put Result [ready X])
                          X)
  _ [ready X] -> X)

(preclude [memo-state-internal])

)
