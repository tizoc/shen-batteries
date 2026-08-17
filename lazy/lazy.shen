\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Utilities for lazy computations

(package lazy [box.t box.make box.unbox box.put]

(datatype memo-state-internal
  L : (lazy A);
  ==============================
  [pending L] : (memo-state A);

  X : A;
  ============================
  [ready X] : (memo-state A);)

\\: `(lazy.memo Frozen)` returns a memoized version of `Frozen`. The first successful
\\: thaw evaluates `Frozen` and caches its result; later thaws reuse that result. If
\\: evaluating `Frozen` raises an error, a later thaw retries the evaluation.
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
