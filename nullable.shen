\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package nullable [@just @null null?]

(datatype internal-type
  ________________
  @null_value_ : (mode (t A) -);)

(datatype t
  X : A;
  ________________
  X : (mode (t A) -);

  X : (t A);
  ________________
  (not (null? X)) : verified >> X : A;)

(define @null
  { --> (t A) }
  -> @null_value_)

(define @just
  { A --> (t A) }
  X -> X)

(define null?
  { (t A) --> boolean }
  X -> (= X @null_value_))

(preclude [nullable-internal])

)