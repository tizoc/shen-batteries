\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package lazy-pattern [defpattern | (external lazy)]

(datatype t
  X : A;
  =================================================
  (freeze X) : (lazy A);)

(defpattern lazy-pattern.pattern-handler
  Self Is? Assign [freeze X] -> (do (Assign X [thaw Self])
                                    handled))

)
