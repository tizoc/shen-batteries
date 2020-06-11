\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package lazy (external lazy)

(datatype t
  X : A >> P;
  _________________
  (freeze X) : (lazy A) >> P;)

(defpattern lazy.pattern-handler
  Self Is? Assign [freeze X] -> (Assign X [thaw Self]))

)