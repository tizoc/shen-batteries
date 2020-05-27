\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package box [box]

(datatype internal-type
  ______________
  (absvector 2) : (mode (box A) -);

  Box : (box A);
  ______________
  (address-> Box 0 @box) : (box A);

  Box : (box A);
  X : A;
  ______________
  (address-> Box 1 X) : (box A);

  (absvector? X) : verified;
  ______________
  (= (<-address X 0) @box) : boolean;

  Box : (box A);
  ______________
  (<-address Box 1) : A;)

(define make
  { A --> (box A) }
  X -> (init-box (absvector 2) X))

(define init-box
  { (box A) --> A --> (box A) }
  Box X -> (box.put (address-> Box 0 @box) X))

(define box?
  { A --> boolean}
  X -> (trap-error (= (<-address X 0) @box) (/. _ false))
    where (absvector? X)
  _ -> false)

(define unbox
  { (box A) --> A }
  Box -> (<-address Box 1))

(define box.put
  { (box A) --> A --> (box A) }
  Box X -> (address-> Box 1 X))

(define modify
  { (A --> A) --> (box A) --> (box A) }
  F Box -> (box.put Box (F (unbox Box))))

(preclude [internal-type])

)