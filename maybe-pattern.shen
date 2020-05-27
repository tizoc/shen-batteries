\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package maybe (external maybe)

(datatype pattern-matching
  ______________
  (@none) : (maybe A);

  X : A;
  ==============
  (@some X) : (maybe A);)

(define maybe-pattern
  Self Is? Assign [@none]   -> (Is? [maybe.none? Self])
  Self Is? Assign [@some X] -> (do (Is? [maybe.some? Self])
                                   (Assign X [maybe.get-unsafe Self]))
  _ _ _ _ -> (fail))

(shen.x.programmable-pattern-matching.register-handler maybe-pattern)

)