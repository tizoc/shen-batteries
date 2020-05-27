\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package nullable [nullable @just @null null?]

(datatype nullable-internal
  ________________
  @null_value_ : (mode (nullable A) -);)

(datatype nullable
  X : A;
  ________________
  X : (mode (nullable A) -);

  X : (nullable A);
  ________________
  (not (null? X)) : verified >> X : A;)

(define @null
  { --> (nullable A) }
  -> @null_value_)

(define @just
  { A --> (nullable A) }
  X -> X)

(define null?
  { (nullable A) --> boolean }
  X -> (= X @null_value_))

(preclude [nullable-internal])

)