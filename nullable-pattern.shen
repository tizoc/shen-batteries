\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package nullable (external nullable)

(datatype pattern-matching
  ______________
  (@null) : (nullable A);

  X : A;
  ==============
  (@just X) : (nullable A);)

(define nullable-pattern
  Self Is? Assign [@null]   -> (Is? [null? Self])
  Self Is? Assign [@just X] -> (do (Is? [not [null? Self]])
                                   (Assign X Self))
  _ _ _ _ -> (fail))

(shen.x.programmable-pattern-matching.register-handler nullable-pattern)

)