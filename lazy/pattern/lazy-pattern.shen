\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package lazy-pattern [sexp void | (external lazy)]

(datatype t
  X : A >> P;
  _________________
  (freeze X) : (lazy A) >> P;)

(define lazy-pattern.pattern-handler
  { sexp --> (sexp --> void) --> (sexp --> sexp --> void) --> sexp --> void }
  Self Is? Assign [freeze X] -> (Assign X [thaw Self])
  _ _ _ _ -> (fail))

(shen.x.programmable-pattern-matching.register-handler lazy-pattern.pattern-handler)

)
