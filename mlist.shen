\\ Copyright (c) 2020 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package mlist [box.t box.make box.put box.modify box.incr box.unbox iter.t void]

(synonyms (iter.t A) ((A --> void) --> void))

(datatype t-internal
  Vec : (vector A);
  N : (box.t number);
  Next : (box.t (mlist.t A));
  ========================
  (@p Vec N Next) : (mlist.t A);

  _______________________
  #empty : (mlist.t A);)

(define empty
  { --> (mlist.t A) }
  -> #empty)

(define mlist.empty?
  { A --> boolean }
  X -> (== X #empty))

(define of-iter
  { (iter.t A) --> (mlist.t A) }
  Iter -> (of-iter-with Iter (/. _ (void))))

(define of-iter-with
  { (iter.t A) --> (A --> void) --> (mlist.t A) }
  Iter Yield -> (let Start     (box.make (mlist.empty))
                     ChunkSize (box.make 8)
                     Prev      (box.make Start)
                     Cur       (box.make (mlist.empty))
                  (do (Iter (/. X (let _ (Yield X)
                                       CurVal (box.unbox Cur)
                                    (append-value ChunkSize CurVal Prev Cur X))))
                      (box.put (box.unbox Prev) (box.unbox Cur))
                      (box.unbox Start))))

(define append-value
  { (box.t number) --> (mlist.t A) --> (box.t (box.t (mlist.t A))) --> (box.t (mlist.t A)) --> A --> void }
  ChunkSize MList _ Cur X -> (let N (box.unbox ChunkSize)
                                  _ (if (< N 4096)
                                        (box.modify (* 2) ChunkSize)
                                        (void))
                                  Vec (vector N)
                                  _ (vector-> Vec 1 X)
                                (box.put Cur (@p Vec (box.make 2) (box.make (mlist.empty)))))
      where (mlist.empty? MList)

  _ (@p V N Next) Prev Cur X -> (do (vector-> V (box.unbox N) X)
                                    (box.incr N)
                                    (if (> (box.unbox N) (limit V))
                                        (do (box.put (box.unbox Prev) (box.unbox Cur))
                                            (box.put Prev Next)
                                            (box.put Cur (mlist.empty)))
                                        (void))))

(define vector-for-each
  { (A --> void) --> (vector A) --> number --> number --> void }
  _ _ N N -> (void)
  F V N Stop -> (do (F (<-vector V N))
                    (vector-for-each F V (+ N 1) Stop)))

(define vector-for-each-reverse
  { (A --> void) --> (vector A) --> number --> void }
  _ _ 0 -> (void)
  F V N -> (do (F (<-vector V N))
               (vector-for-each-reverse F V (- N 1))))

(define for-each
  { (A --> void) --> (mlist.t A) --> void }
  _ MList -> (void) where (mlist.empty? MList)
  Yield (@p V N Next) -> (do (vector-for-each Yield V 1 (box.unbox N))
                             (for-each Yield (box.unbox Next))))

(define for-each-reverse
  { (A --> void) --> (mlist.t A) --> void }
  _ MList -> (void) where (mlist.empty? MList)
  Yield (@p V N Next) -> (do (for-each-reverse Yield (box.unbox Next))
                             (vector-for-each-reverse Yield V (box.unbox N))))

(define to-iter
  { (mlist.t A) --> (iter.t A) }
  MList Yield -> (for-each Yield MList))

(preclude [t-internal])

)