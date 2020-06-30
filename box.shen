\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Boxes
\\:
\\:  Boxes are mutable references to a value.


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

\\: == API

\\: === Construction

\\: `(box.make Value)` returns a new box containing `Value`.
(define make
  { A --> (t A) }
  X -> (init-box (absvector 2) X))

(define init-box
  { (t A) --> A --> (t A) }
  Box X -> (box.put (address-> Box 0 #tag) X))

\\: === Predicates

\\: `(box.box? X)` returns `true` if `X` is a box, `false` otherwise.
(define box?
  { A --> boolean}
  X -> (trap-error (= (<-address X 0) #tag) (/. _ false))
    where (absvector? X)
  _ -> false)

\\: === Access

\\: `(box.unbox Box)` returns the value inside `Box`.
(define unbox
  { (t A) --> A }
  Box -> (<-address Box 1))

\\: === Modification

\\: `(box.put Box Value)` stores `Value` inside `Box`.
(define box.put
  { (t A) --> A --> (t A) }
  Box X -> (address-> Box 1 X))

\\: `(box.modify F Box)` stores the result of `(box.put (F (box.unbox Box)))` inside `Box`.
(define modify
  { (A --> A) --> (t A) --> (t A) }
  F Box -> (box.put Box (F (unbox Box))))

\\: `(box.incr Box)` increments the number stored in `Box` by 1.
(define box.incr
  { (t number) --> (t number) }
  Box -> (box.put Box (+ (unbox Box) 1)))

\\: `(box.decr Box)` decrements the number stored in `Box` by 1.
(define box.decr
  { (t number) --> (t number) }
  Box -> (box.put Box (- (unbox Box) 1)))

\\: `(box.toggle Box)` replaces the contents of `Box` with `true` if it contains `false` or with `false` if it contains `true`.
(define box.toggle
  { (t boolean) --> (t boolean) }
  Box -> (box.put Box (not (unbox Box))))

(define #tag
  { (t A) --> string }
  B -> (make-string "(box ~S)" (unbox B)))

(preclude [t-internal])

)