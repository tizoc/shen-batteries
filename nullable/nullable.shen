\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package nullable [@just @null null? defpattern]

(datatype t-internal
  ________________
  @null_value_ : (- (t A));)

(datatype t
  X : A;
  ________________
  X : (- (t A));

  X : (t A);
  ________________
  (not (null? X)) : verified >> X : A;

  \\ Pattern matching
  ______________
  (@p shen.custom-pattern (@null)) : (t A);

  X : A;
  ==============
  (@p shen.custom-pattern (@just X)) : (t A);)

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
  Self Is? Assign [@null]   -> (do (Is? [null? Self])
                                   handled)
  Self Is? Assign [@just X] -> (do (Is? [not [null? Self]])
                                   (Assign X Self)
                                   handled))

(preclude [t-internal])

)
