\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package maybe [maybe @some @none unit]

(datatype internal-type
  ________________
  @none_value_ : (mode (maybe A) -);

  ______________
  (absvector 2) : (mode (maybe A) -);

  MaybeX : (maybe A);
  ______________
  (address-> MaybeX 0 @some-tag) : (maybe A);

  MaybeX : (maybe A);
  X : A;
  ______________
  (address-> MaybeX 1 X) : (maybe A);

  (absvector? X) : verified;
  ______________
  (= (<-address X 0) @some-tag) : boolean;

  MaybeX : (maybe A);
  ______________
  (<-address MaybeX 1) : A;)

(define @none
  { --> (maybe A) }
  -> @none_value_)

(define init-@some
  { (maybe A) --> A --> (maybe A) }
  M X -> (address-> (address-> M 0 @some-tag) 1 X))

(define @some
  { A --> (maybe A) }
  X -> (init-@some (absvector 2) X))

(define none?
  { (maybe A) --> boolean }
  X -> (= X @none_value_))

(define some?
  { (maybe A) --> boolean }
  X -> (not (none? X)))

(define maybe.get
  { (maybe A) --> A }
  X -> (<-address X 1) where (some? X)
  _ -> (error "Not a @some value"))

(define maybe.get-unsafe
  { (maybe A) --> A }
  X -> (<-address X 1))

(define maybe.get/or
  { (maybe A) --> (lazy A) --> A }
  X _ -> (<-address X 1) where (some? X)
  _ F -> (thaw F))

(define maybe.map
  { (A --> B) --> (maybe A) --> (maybe B) }
  F X -> (@some (F (maybe.get X))) where (some? X)
  _ X -> (@none))

(define maybe.for-each
  { (A --> B) --> (maybe A) --> unit }
  F X -> (do (F (maybe.get X))
             unit)
      where (some? X)
  _ X -> unit)

(preclude [internal-type])

)