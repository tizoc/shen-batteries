\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package maybe [maybe]

(datatype maybe-internal
  ________________
  @nothing : (mode (maybe A) -);)

(datatype maybe
  X : A;
  ________________
  X : (mode (maybe A) -);

  X : (maybe A);
  ________________
  (some? X) : verified >> X : A;)

(define nothing
  { --> (maybe A) }
  -> @nothing)

(define nothing?
  { (maybe A) --> boolean }
  X -> (= X @nothing))

(define some?
  { (maybe A) --> boolean }
  X -> (not (nothing? X)))

(preclude [maybe-internal])

)