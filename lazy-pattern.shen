\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package lazy (external lazy)

(datatype pattern-matching
  X : A >> P;
  _________________
  (freeze X) : (lazy A) >> P;)

(define freeze-pattern
  Self Is? Assign [freeze X] -> (Assign X [thaw Self])
  _ _ _ _ -> (fail))

(shen.x.programmable-pattern-matching.register-handler freeze-pattern)

)