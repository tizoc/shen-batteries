\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package maybe [@some @none unit]

(datatype internal-type
  ________________
  @none_value_ : (mode (t A) -);

  ______________
  (absvector 2) : (mode (t A) -);

  MaybeX : (t A);
  ______________
  (address-> MaybeX 0 #tag#some) : (t A);

  MaybeX : (t A);
  X : A;
  ______________
  (address-> MaybeX 1 X) : (t A);

  (absvector? X) : verified;
  ______________
  (= (<-address X 0) #tag#some) : boolean;

  MaybeX : (t A);
  ______________
  (<-address MaybeX 1) : A;)

(define @none
  { --> (t A) }
  -> @none_value_)

(define init-@some
  { (t A) --> A --> (t A) }
  M X -> (address-> (address-> M 0 #tag#some) 1 X))

(define @some
  { A --> (t A) }
  X -> (init-@some (absvector 2) X))

(define none?
  { (t A) --> boolean }
  X -> (= X @none_value_))

(define some?
  { (t A) --> boolean }
  X -> (not (none? X)))

(define maybe.get
  { (t A) --> A }
  X -> (<-address X 1) where (some? X)
  _ -> (error "Not a @some value"))

(define get-unsafe
  { (t A) --> A }
  X -> (<-address X 1))

(define get/or
  { (t A) --> (lazy A) --> A }
  X _ -> (<-address X 1) where (some? X)
  _ F -> (thaw F))

(define maybe.map
  { (A --> B) --> (t A) --> (t B) }
  F X -> (@some (F (maybe.get X))) where (some? X)
  _ X -> (@none))

(define for-each
  { (A --> B) --> (t A) --> unit }
  F X -> (do (F (maybe.get X))
             unit)
      where (some? X)
  _ X -> unit)

(define #tag#some
  { (t A) --> string }
  X -> (make-string "(@some ~S)" (get-unsafe X)))

(preclude [internal-type])

)