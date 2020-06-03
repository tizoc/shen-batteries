\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package box []

(datatype t-internal
  ______________
  (absvector 2) : (mode (t A) -);

  Box : (t A);
  ______________
  (address-> Box 0 #tag) : (t A);

  Box : (t A);
  X : A;
  ______________
  (address-> Box 1 X) : (t A);

  (absvector? X) : verified;
  ______________
  (= (<-address X 0) #tag) : boolean;

  Box : (t A);
  ______________
  (<-address Box 1) : A;)

(define make
  { A --> (t A) }
  X -> (init-box (absvector 2) X))

(define init-box
  { (t A) --> A --> (t A) }
  Box X -> (box.put (address-> Box 0 #tag) X))

(define box?
  { A --> boolean}
  X -> (trap-error (= (<-address X 0) #tag) (/. _ false))
    where (absvector? X)
  _ -> false)

(define unbox
  { (t A) --> A }
  Box -> (<-address Box 1))

(define box.put
  { (t A) --> A --> (t A) }
  Box X -> (address-> Box 1 X))

(define modify
  { (A --> A) --> (t A) --> (t A) }
  F Box -> (box.put Box (F (unbox Box))))

(define #tag
  { (t A) --> string }
  B -> (make-string "(box ~S)" (unbox B)))

(preclude [t-internal])

)