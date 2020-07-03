\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package nullable [@just @null null? void sexp defpattern]

(datatype t-internal
  ________________
  @null_value_ : (mode (t A) -);)

(datatype t
  X : A;
  ________________
  X : (mode (t A) -);

  X : (t A);
  ________________
  (not (null? X)) : verified >> X : A;

  \\ Pattern matching
  ______________
  (@null) : (nullable A);

  X : A;
  ==============
  (@just X) : (nullable A);)

(define @null
  { --> (t A) }
  -> @null_value_)

(define @just
  { A --> (t A) }
  X -> X)

(define null?
  { (t A) --> boolean }
  X -> (= X @null_value_))

(defpattern nullable.pattern-handler
  Self Is? Assign [@null]   -> (Is? [null? Self])
  Self Is? Assign [@just X] -> (do (Is? [not [null? Self]])
                                   (Assign X Self)))

(preclude [t-internal])

)