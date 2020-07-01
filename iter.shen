\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Iter
\\:
\\: The type `(iter.t A)` represents an iterator that produces values of type `A`.
\\: When applied to a function of type `A --> void`, the function will be applied
\\: to every value produced by the iterator until it is consumed or an exception
\\: is raised.
\\:
\\: Instances of `(iter.t A)` are push-based iterators, which means that the iteration
\\: is controlled by the producer. For a pull-based iterator see the `seq` library.

(package iter [maybe.t maybe.some? maybe.unsafe-get maybe.for-each @some @none void box.make box.unbox box.put box.modify box.incr box.toggle box.t with-return with-break]

(synonyms (iter.t A) ((A --> void) --> void))

\\: == Creation

\\: `(iter.from-lazy Frozen)`
(define from-lazy
  { (lazy (maybe.t A)) --> (iter.t A) }
  F Yield -> (from-lazy-h (thaw F) F Yield))

(define from-lazy-h
  { (maybe.t A) --> (lazy (maybe.t A)) --> (iter.t A) }
  (@none) _ _ -> (void)
  (@some X) F Yield -> (do (Yield X)
                           (from-lazy-h (thaw F) F Yield)))

\\: `(iter.empty)`
(define empty
  { --> (iter.t A) }
  -> (/. _ (void)))

\\: `(iter.singleton X)`
(define singleton
  { A --> (iter.t A) }
  X F -> (F X))

\\: `(iter.cons X Iter)`
(define iter.cons
  { A --> (iter.t A) --> (iter.t A)}
  X Iter Yield -> (do (Yield X)
                      (Iter Yield)))

\\: `(iter.snoc Iter X)`
(define snoc
  { (iter.t A) --> A --> (iter.t A)}
  Iter X Yield -> (do (Iter Yield)
                      (Yield X)))

\\: `(iter.repeat X)`
(define repeat
  { A --> (iter.t A) }
  X Yield -> (do (Yield X)
                 (repeat X Yield)))

\\: `(iter.init F)`
(define init
  { (number --> A) --> (iter.t A) }
  FN Yield -> (init-h 0 FN Yield))

(define init-h
  { number --> (number --> A) --> (iter.t A) }
  0 _ _ -> (void)
  N FN Yield -> (do (Yield (FN N))
                    (init-h (- N 1) FN Yield)))

\\: `(iterate F X)`
(define iterate
  { (A --> A) --> A --> (iter.t A) }
  F X Yield -> (do (Yield X)
                   (iterate F (F X) Yield)))

\\: `(iter.forever Frozen)`
(define forever
  { (lazy A) --> (iter.t A) }
  X Yield -> (do (Yield (thaw X))
                 (forever X Yield)))

\\: `(iter.cycle Iter)`
(define cycle
  { (iter.t A) --> (iter.t A) }
  Iter Yield -> (do (Iter Yield)
                    (cycle Iter Yield)))

\\: `(iter.unfoldr X F Init)`
(define unfoldr
  { (B --> (maybe.t (A * B))) --> B --> (iter.t A) }
  F X Yield -> (unfoldr-h F (F X) Yield))

(define unfoldr-h
  { (B --> (maybe.t (A * B))) --> (maybe.t (A * B)) --> (iter.t A) }
  _ (@none) _ -> (void)
  F (@some (@p X Rest)) Yield -> (do (Yield X)
                                     (unfoldr-h F (F Rest) Yield)))

\\: `(iter.scan F Init Iter)`
(define scan
  { (B --> A --> B) --> B --> (iter.t A) --> (iter.t B) }
  F Acc Iter Yield -> (let _ (Yield Acc)
                           Acc (box.make Acc)
                        (Iter (/. Elt
                               (let Acc' (F (box.unbox Acc) Elt)
                                    _ (Yield Acc')
                                 (box.put Acc Acc'))))))


\\: == Consumption

\\: `(iter.for-each F Iter)`
(define for-each
  { (A --> void) --> (iter.t A) --> void }
  F Iter -> (Iter F))

\\: `(iter.for-eachi F Iter)`
(define for-eachi
  { (number --> A --> void) --> (iter.t A) --> void }
  F Iter -> (let Index (box.make 0)
              (Iter (/. X (let I (box.unbox Index)
                              _ (F I X)
                            (box.put Index (+ 1 I)))))))

\\: `(iter.fold F Init Iter)`
(define fold
  { (A --> B --> A) --> A --> (iter.t B) --> A }
  F Init Iter -> (let Acc (box.make Init)
                      _ (Iter (/. Elt (box.put Acc (F (box.unbox Acc) Elt))))
                   (box.unbox Acc)))

\\: `(iter.foldi F Init Iter)`
(define foldi
  { (A --> number --> B --> A) --> A --> (iter.t B) --> A }
  F Init Iter -> (let Index (box.make 0)
                      Acc (box.make Init)
                      _ (Iter (/. Elt (let I (box.unbox Index)
                                          _ (box.modify (/. Acc (F Acc I Elt)) Acc)
                                       (box.put Index (+ 1 I)))))
                   (box.unbox Acc)))

\\: `(iter.fold-map F Init Iter)`
(define fold-map
  { (Acc --> A --> (Acc * B)) --> Acc --> (iter.t A) --> (iter.t B) }
  F Init Iter Yield -> (let Acc (box.make Init)
                         (Iter (/. X (let Acc*Y (F (box.unbox Acc) X)
                                          _ (box.put Acc (fst Acc*Y))
                                       (Yield (snd Acc*Y)))))))

\\: `(iter.fold-filter-map F Init Iter)`
(define fold-filter-map
  { (Acc --> A --> (Acc * (maybe.t B))) --> Acc --> (iter.t A) --> (iter.t B) }
  F Init Iter Yield -> (let Acc (box.make Init)
                         (Iter (/. X (let Acc*Y (F (box.unbox Acc) X)
                                          _ (box.put Acc (fst Acc*Y))
                                          Y (snd Acc*Y)
                                       (maybe.for-each Yield Y))))))

\\: `(iter.map F Iter)`
(define iter.map
  { (A --> B) --> (iter.t A) --> (iter.t B) }
  F Iter Yield -> (Iter (/. X (Yield (F X)))))

\\: `(iter.mapi F Iter)`
(define mapi
  { (number --> A --> B) --> (iter.t A) --> (iter.t B) }
  F Iter Yield -> (let Index (box.make 0)
                    (Iter (/. X (do (Yield (F (box.unbox Index) X))
                                    (box.incr Index))))))

\\: `(iter.map-by-2 F Iter)`
\\(define map-by-2
\\  { (A --> A --> A) --> (iter.t A) --> (iter.t A) }
\\  F Iter Yield -> )

\\: `(iter.for-all? Test Iter)`
(define for-all?
  { (A --> boolean) --> (iter.t A) --> boolean }
  Test Iter -> (with-return Return
                 (do (Iter (/. X (if (not (Test X))
                                     (Return false)
                                     (void))))
                     true)))

\\: `(iter.exists? Test Iter)`
(define exists?
  { (A --> boolean) --> (iter.t A) --> boolean }
  Test Iter -> (with-return Return
                 (do (Iter (/. X (if (Test X)
                                     (Return true)
                                     (void))))
                     false)))

\\: `(iter.element? X Iter)`
(define iter.element?
  { A --> (iter.t A) --> boolean }
  Elt Iter -> (exists? (= Elt) Iter))

\\: `(iter.element-eq? EqF X Iter)`
(define element-eq?
  { (A --> A --> boolean) --> A --> (iter.t A) --> boolean }
  Eq Elt Iter -> (exists? (Eq Elt) Iter))

\\: `(iter.find-map F Iter)`
(define find-map
  { (A --> (maybe.t B)) --> (iter.t A) --> (maybe.t B) }
  F Iter -> (with-return Return
              (do (Iter (/. X (let Maybe (F X)
                                (if (maybe.some? Maybe)
                                    (Return Maybe)
                                    (void)))))
                  (@none))))

\\: `(iter.find-mapi F Iter)`
(define find-mapi { (number --> A --> (maybe.t B)) --> (iter.t A) --> (maybe.t B) }
  F Iter -> (let Index (box.make 0)
              (with-return Return
                (do (Iter (/. X (let Maybe (F (box.unbox Index) X)
                                  (if (maybe.some? Maybe)
                                      (Return Maybe)
                                      (box.incr Index)))))
                    (@none)))))

\\: `(iter.find Test Iter)`
(define find
  { (A --> boolean) --> (iter.t A) --> (maybe.t A) }
  Test Iter -> (find-map (/. X (if (Test X) (@some X) (@none))) Iter))

\\: `(iter.find-exn Test Iter)`
(define find-exn
  { (A --> boolean) --> (iter.t A) --> A }
  Test Iter -> (let R (find Test Iter)
                 (if (maybe.some? R)
                     (maybe.unsafe-get R)
                     (error "find-exn: value not found"))))

\\: `(iter.length Iter)`
(define iter.length { (iter.t A) --> number }
  Iter -> (let R (box.make 0)
               _ (Iter (/. _ (box.incr R)))
            (box.unbox R)))

\\: `(iter.empty? Iter)`
(define iter.empty?
  { (iter.t A) --> boolean }
  Iter -> (with-return Return
            (do (Iter (/. _ (Return false)))
                true)))

\\: == Transformation

\\: `(iter.filter F Iter)`
(define filter
  { (A --> boolean) --> (iter.t A) --> (iter.t A) }
  F Iter Yield -> (Iter (/. X (if (F X)
                                  (Yield X)
                                  (void)))))

\\: `(iter.append IterL IterR)`
(define iter.append
  { (iter.t A) --> (iter.t A) --> (iter.t A) }
  IterL IterR Yield -> (do (IterL Yield)
                           (IterR Yield)))

\\: `(iter.concat IterList)`
(define iter.concat
  { (list (iter.t A)) --> (iter.t A) }
  [] _ -> (void)
  [Iter | Rest] Yield -> (do (Iter Yield)
                             (iter.concat Rest Yield)))

\\: `(iter.flatten ItersIter)`
(define flatten
  { (iter.t (iter.t A)) --> (iter.t A) }
  Iters Yield -> (Iters (/. Iter (Iter Yield))))

\\: `(iter.flat-map F Iter)`
(define flat-map
  { (A --> (iter.t B)) --> (iter.t A) --> (iter.t B) }
  F Iter Yield -> (Iter (/. X (F X Yield))))

\\: `(iter.flat-map-l F Iter)`
(define flat-map-l
  { (A --> (list B)) --> (iter.t A) --> (iter.t B) }
  F Iter Yield -> (Iter (/. X (list-for-each Yield (F X)))))

(define list-for-each
  { (A --> void) --> (list A) --> void }
  F [] -> (void)
  F [X | Rest] -> (do (F X)
                      (list-for-each F Rest)))

\\: `(iter.filter-map F Iter)`
(define filter-map
  { (A --> (maybe.t B)) --> (iter.t A) --> (iter.t B) }
  F Iter Yield -> (Iter (/. X (maybe.for-each Yield (F X)))))

\\: `(iter.filter-mapi F Iter)`
(define filter-mapi
  { (number --> A --> (maybe.t B)) --> (iter.t A) --> (iter.t B) }
  F Iter Yield -> (let Index (box.make 0)
                    (Iter (/. X (let Res (F (box.unbox Index) X)
                                     _ (box.incr Index)
                                  (maybe.for-each Yield Res))))))

\\: `(iter.filter-count F Iter)`
(define filter-count
  { (A --> boolean) --> (iter.t A) --> number }
  F Iter -> (let Count (box.make 0)
                 _ (Iter (/. X (if (F X)
                                   (box.incr Count)
                                   (void))))
              (box.unbox Count)))

\\: `(iter.intersperse Elt Iter)`
(define intersperse
  { A --> (iter.t A) --> (iter.t A) }
  Elt Iter Yield -> (let First (box.make true)
                      (Iter (/. X (do (if (box.unbox First)
                                          (box.put First false)
                                          (Yield Elt))
                                      (Yield X))))))

\\: `(iter.keep-some Iter)`
(define keep-some
  { (iter.t (maybe.t A)) --> (iter.t A) }
  Iter Yield -> (Iter (/. X (maybe.for-each Yield X))))

\\: == Caching

\\ (define persistent { (iter.t A) --> (iter.t A) } )
\\ (define persistent-lazy { (iter.t A) --> (iter.t A) } )

\\: == Misc

\\: === List-like

\\: `(iter.head Iter)`
(define iter.head
  { (iter.t A) --> (maybe.t A) }
  Iter -> (with-return Return
            (do (Iter (/. X (Return (@some X))))
                (@none))))

\\: `(iter.head-exn)`
(define head-exn
  { (iter.t A) --> A }
  Iter -> (let R (iter.head Iter)
            (if (maybe.some? R)
                (maybe.unsafe-get R)
                (error "iter.head-exn called on emtpy iter"))))

\\: `(iter.take N Iter)`
(define take
  { number --> (iter.t A) --> (iter.t A) }
  N Iter Yield -> (let Count (box.make 0)
                    (with-break Break
                      (Iter (/. X (if (= (box.unbox Count) N)
                                      (Break)
                                      (do (box.incr Count)
                                          (Yield X))))))))

\\: `(iter.take-while P Iter)`
(define take-while
  { (A --> boolean) --> (iter.t A) --> (iter.t A) }
  P Iter Yield -> (with-break Break
                    (Iter (/. X (if (P X)
                                    (Yield X)
                                    (Break))))))

\\: `(iter.fold-while F Init Iter)`
(define fold-while
  { (A --> B --> (A * boolean)) --> A --> (iter.t B) --> A }
  F Init Iter -> (let State (box.make Init)
                   (do (with-break Break
                         (Iter (/. X (let (@p Acc Cont) (F (box.unbox State) X)
                                          _ (box.put State Acc)
                                       (if Cont
                                           (void)
                                           (Break))))))
                       (box.unbox State))))

\\: `(iter.drop N Iter)`
(define drop
  { number --> (iter.t A) --> (iter.t A) }
  N Iter Yield -> (let Count (box.make 0)
                    (Iter (/. X (if (>= (box.unbox Count) N)
                                    (Yield X)
                                    (box.incr Count))))))

\\: `(iter.drop-while P Iter)`
(define drop-while
  { (A --> boolean) --> (iter.t A) --> (iter.t A) }
  P Iter Yield -> (let Drop (box.make true)
                    (Iter (/. X (if (box.unbox Drop)
                                    (if (P X)
                                        (void)
                                        (do (box.toggle Drop)
                                            (Yield X)))
                                    (Yield X))))))
\\ `(iter.reverse Iter)`
\\ (define iter.reverse { (iter.t A) --> (iter.t A) } )

\\: `(iter.enumerate Iter)`
(define enumerate
  { (iter.t A) --> (iter.t (number * A)) }
  Iter Yield -> (let R (box.make 0)
                  (Iter (/. X (let N (box.unbox R)
                                   _ (box.incr R)
                                (Yield (@p N X)))))))

)


