\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\** {1 Utilities for lazy computations} *\

(package lazy [box.make box.unbox box.put]

(datatype t-internal
  _____________
  #not-thawed#7907# : A;)

\** [(lazy.memo Frozen)] returns a memoized version of [Frozen] that will produce the same
    result as [(thaw Frozen)] when thawed but performing the computation only once
    the first time it is thawed, and reusing the initial result every other time. *\
(define memo
  { (lazy A) --> (lazy A) }
  L -> (let Result (box.make #not-thawed#7907#)
         (freeze
          (let X (box.unbox Result)
            (if (= #not-thawed#7907# X)
                (let Thawed (thaw L)
                     Update (box.put Result Thawed)
                  Thawed)
                X)))))

(preclude [t-internal])

)