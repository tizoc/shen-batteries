\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Utilities for lazy computations

(package lazy [box.make box.unbox box.put]

\\: `(lazy.memo Frozen)` returns a memoized version of `Frozen` that will produce the same
\\: result as `(thaw Frozen)` when thawed but performing the computation only once
\\: the first time it is thawed, and reusing the initial result every other time.
(define memo
  { (lazy A) --> (lazy A) }
  L -> (let Result (box.make L)
         (freeze
          (let X (thaw (box.unbox Result))
               Update (box.put Result (freeze X))
            X))))

)
