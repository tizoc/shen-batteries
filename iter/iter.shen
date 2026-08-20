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
\\:
\\: Applying an iterator starts a traversal. Applying it again normally starts the
\\: producer again, so any effects in the producer are repeated. Transformations such
\\: as `iter.map` and `iter.filter` preserve this behavior. Use `iter.persistent` or
\\: `iter.persistent-lazy` when subsequent traversals should replay cached values.
\\:
\\: Consumers such as `iter.fold`, `iter.length`, and `iter.to-list` traverse the
\\: complete iterator and therefore return only for finite inputs. Short-circuiting
\\: consumers such as `iter.head`, `iter.find`, and `iter.exists?` can consume an
\\: infinite iterator when they reach a result.

(package iter [
  maybe.t maybe.some? maybe.unsafe-get maybe.for-each @some @none
  void with-return with-break
  box.make box.unbox box.put box.modify box.incr box.toggle box.t
  mlist.length mlist.of-iter mlist.of-iter-with mlist.to-iter mlist.for-each-reverse mlist.for-each-enumerated
  mlist.vector-for-each mlist.vector-for-each-enumerated
]

(synonyms (iter.t A) ((A --> void) --> void))

\\: == Creation

\\: `(iter.from-lazy Frozen)` repeatedly thaws `Frozen`. Each `@some X` produces
\\: `X`; the first `@none` ends the traversal. The frozen computation is responsible
\\: for maintaining any state needed to produce successive values.
(define from-lazy
  { (lazy (maybe.t A)) --> (iter.t A) }
  F Yield -> (from-lazy-h (thaw F) F Yield))

(define from-lazy-h
  { (maybe.t A) --> (lazy (maybe.t A)) --> (iter.t A) }
  (@none) _ _ -> (void)
  (@some X) F Yield -> (do (Yield X)
                           (from-lazy-h (thaw F) F Yield)))

\\: `(iter.empty)` produces no values.
(define empty
  { --> (iter.t A) }
  -> (/. X (void)))

\\: `(iter.singleton X)` produces exactly `X`.
(define singleton
  { A --> (iter.t A) }
  X F -> (F X))

\\: `(iter.cons X Iter)` produces `X` followed by every value from `Iter`.
(define iter.cons
  { A --> (iter.t A) --> (iter.t A)}
  X Iter Yield -> (do (Yield X)
                      (Iter Yield)))

\\: `(iter.snoc Iter X)` produces every value from `Iter` followed by `X`.
(define snoc
  { (iter.t A) --> A --> (iter.t A)}
  Iter X Yield -> (do (Iter Yield)
                      (Yield X)))

\\: `(iter.repeat X)` produces `X` forever.
(define repeat
  { A --> (iter.t A) }
  X Yield -> (do (Yield X)
                 (repeat X Yield)))

\\: `(iter.init F)` produces an infinite iterator containing `F 0`, `F 1`, and so on.
(define init
  { (number --> A) --> (iter.t A) }
  FN Yield -> (init-h 0 FN Yield))

(define init-h
  { number --> (number --> A) --> (iter.t A) }
  N FN Yield -> (do (Yield (FN N))
                    (init-h (+ N 1) FN Yield)))

\\: `(iter.iterate F X)` produces the infinite iterator `X`, `F X`, `F (F X)`,
\\: and so on.
(define iterate
  { (A --> A) --> A --> (iter.t A) }
  F X Yield -> (do (Yield X)
                   (iterate F (F X) Yield)))

\\: `(iter.forever Frozen)` repeatedly thaws `Frozen` and produces its value forever.
(define forever
  { (lazy A) --> (iter.t A) }
  X Yield -> (do (Yield (thaw X))
                 (forever X Yield)))

\\: `(iter.cycle Iter)` repeats every value produced by `Iter`. If `Iter` is
\\: empty, the resulting iterator is empty. Every cycle traverses `Iter` again, so
\\: effects in `Iter` are repeated unless it is persistent. A repeatable finite
\\: iterator that produces at least one value on every traversal produces an
\\: infinite result.
(define cycle
  { (iter.t A) --> (iter.t A) }
  Iter Yield -> (let Produced (box.make false)
                  (do (Iter (/. X (do (box.put Produced true)
                                      (Yield X))))
                      (if (box.unbox Produced)
                          (cycle Iter Yield)
                          (void)))))

\\: `(iter.unfoldr F State)` repeatedly applies `F` to the current state. A result
\\: `(@some (@p Value Next))` produces `Value` and continues with `Next`; `@none`
\\: ends the traversal.
(define unfoldr
  { (B --> (maybe.t (A * B))) --> B --> (iter.t A) }
  F X Yield -> (unfoldr-h F (F X) Yield))

(define unfoldr-h
  { (B --> (maybe.t (A * B))) --> (maybe.t (A * B)) --> (iter.t A) }
  _ (@none) _ -> (void)
  F (@some (@p X Rest)) Yield -> (do (Yield X)
                                     (unfoldr-h F (F Rest) Yield)))

\\: `(iter.scan F Init Iter)` produces `Init`, then the successive accumulators from
\\: folding `F` over `Iter` from left to right. It therefore produces one more value
\\: than `Iter` when `Iter` is finite.
(define scan
  { (B --> A --> B) --> B --> (iter.t A) --> (iter.t B) }
  F Acc Iter Yield -> (let _ (Yield Acc)
                           Acc (box.make Acc)
                        (Iter (/. Elt
                               (let Acc' (F (box.unbox Acc) Elt)
                                    _ (Yield Acc')
                                 (box.put Acc Acc'))))))


\\: == Consumption

\\: `(iter.for-each F Iter)` applies `F` to every value. `Iter` must be finite for
\\: this call to return.
(define for-each
  { (A --> void) --> (iter.t A) --> void }
  F Iter -> (Iter F))

\\: `(iter.for-eachi F Iter)` applies `F` to every zero-based index and value.
\\: `Iter` must be finite for this call to return.
(define for-eachi
  { (number --> A --> void) --> (iter.t A) --> void }
  F Iter -> (let Index (box.make 0)
              (Iter (/. X (let I (box.unbox Index)
                              _ (F I X)
                            (box.put Index (+ 1 I)))))))

\\: `(iter.fold F Init Iter)` folds every value from left to right. `Iter` must be
\\: finite for this call to return.
(define fold
  { (A --> B --> A) --> A --> (iter.t B) --> A }
  F Init Iter -> (let Acc (box.make Init)
                      _ (Iter (/. Elt (box.put Acc (F (box.unbox Acc) Elt))))
                   (box.unbox Acc)))

\\: `(iter.foldi F Init Iter)` folds every zero-based index and value from left to
\\: right. `Iter` must be finite for this call to return.
(define foldi
  { (A --> number --> B --> A) --> A --> (iter.t B) --> A }
  F Init Iter -> (let Index (box.make 0)
                      Acc (box.make Init)
                      _ (Iter (/. Elt (let I (box.unbox Index)
                                          _ (box.modify (/. Acc (F Acc I Elt)) Acc)
                                       (box.put Index (+ 1 I)))))
                   (box.unbox Acc)))

\\: `(iter.fold-map F Init Iter)` threads an accumulator through `Iter`. For each
\\: input, `F` receives the current accumulator and returns
\\: `(@p Next-Accumulator Output)`; the accumulator is updated and `Output` is
\\: produced. `Init` is restored for each traversal of the resulting iterator.
(define fold-map
  { (Acc --> A --> (Acc * B)) --> Acc --> (iter.t A) --> (iter.t B) }
  F Init Iter Yield -> (let Acc (box.make Init)
                         (Iter (/. X (let Acc*Y (F (box.unbox Acc) X)
                                          _ (box.put Acc (fst Acc*Y))
                                       (Yield (snd Acc*Y)))))))

\\: `(iter.fold-filter-map F Init Iter)` is a stateful filter-map. For every input,
\\: `F` returns `(@p Next-Accumulator Maybe-Output)`. The accumulator is updated even
\\: when `Maybe-Output` is `@none`; an `@some` value is unwrapped and produced.
\\: `Init` is restored for each traversal of the resulting iterator.
(define fold-filter-map
  { (Acc --> A --> (Acc * (maybe.t B))) --> Acc --> (iter.t A) --> (iter.t B) }
  F Init Iter Yield -> (let Acc (box.make Init)
                         (Iter (/. X (let Acc*Y (F (box.unbox Acc) X)
                                          _ (box.put Acc (fst Acc*Y))
                                          Y (snd Acc*Y)
                                       (maybe.for-each Yield Y))))))

\\: `(iter.map F Iter)` applies `F` to every value from `Iter`, preserving order.
(define iter.map
  { (A --> B) --> (iter.t A) --> (iter.t B) }
  F Iter Yield -> (Iter (/. X (Yield (F X)))))

\\: `(iter.mapi F Iter)` applies `F` to each zero-based index and value, preserving
\\: order. Indexing restarts at zero for every traversal.
(define mapi
  { (number --> A --> B) --> (iter.t A) --> (iter.t B) }
  F Iter Yield -> (let Index (box.make 0)
                    (Iter (/. X (do (Yield (F (box.unbox Index) X))
                                    (box.incr Index))))))

\\: `(iter.for-all? Test Iter)` returns whether every value satisfies `Test`, and is
\\: true for an empty iterator. It stops at the first failure, so it can consume an
\\: infinite iterator when such a value is reached.
(define for-all?
  { (A --> boolean) --> (iter.t A) --> boolean }
  Test Iter -> (with-return Return
                 (do (Iter (/. X (if (not (Test X))
                                     (Return false)
                                     (void))))
                     true)))

\\: `(iter.exists? Test Iter)` returns whether any value satisfies `Test`, and is
\\: false for an empty iterator. It stops at the first match, so it can consume an
\\: infinite iterator when such a value is reached.
(define exists?
  { (A --> boolean) --> (iter.t A) --> boolean }
  Test Iter -> (with-return Return
                 (do (Iter (/. X (if (Test X)
                                     (Return true)
                                     (void))))
                     false)))

\\: `(iter.element? X Iter)` returns whether `Iter` contains a value equal to `X`
\\: according to Shen's `=`. It stops at the first match.
(define iter.element?
  { A --> (iter.t A) --> boolean }
  Elt Iter -> (exists? (= Elt) Iter))

\\: `(iter.element-eq? EqF X Iter)` returns whether any value satisfies
\\: `(EqF X Value)`. It stops at the first match.
(define element-eq?
  { (A --> A --> boolean) --> A --> (iter.t A) --> boolean }
  Eq Elt Iter -> (exists? (Eq Elt) Iter))

\\: `(iter.find-map F Iter)` returns the first `@some` result from applying `F`, or
\\: `@none` when there is no such result. It stops at the first `@some`.
(define find-map
  { (A --> (maybe.t B)) --> (iter.t A) --> (maybe.t B) }
  F Iter -> (with-return Return
              (do (Iter (/. X (let Maybe (F X)
                                (if (maybe.some? Maybe)
                                    (Return Maybe)
                                    (void)))))
                  (@none))))

\\: `(iter.find-mapi F Iter)` applies `F` to each zero-based index and value and
\\: returns the first `@some` result, or `@none` when there is no such result. It
\\: stops at the first `@some`.
(define find-mapi { (number --> A --> (maybe.t B)) --> (iter.t A) --> (maybe.t B) }
  F Iter -> (let Index (box.make 0)
              (with-return Return
                (do (Iter (/. X (let Maybe (F (box.unbox Index) X)
                                  (if (maybe.some? Maybe)
                                      (Return Maybe)
                                      (box.incr Index)))))
                    (@none)))))

\\: `(iter.find Test Iter)` returns the first value that satisfies `Test` and stops
\\: traversing immediately.
(define iter.find
  { (A --> boolean) --> (iter.t A) --> (maybe.t A) }
  Test Iter -> (find-map (/. X (if (Test X) (@some X) (@none))) Iter))

\\: `(iter.find-exn Test Iter)` returns the first value that satisfies `Test` and
\\: stops immediately. It raises an error when no value matches.
(define find-exn
  { (A --> boolean) --> (iter.t A) --> A }
  Test Iter -> (let R (iter.find Test Iter)
                 (if (maybe.some? R)
                     (maybe.unsafe-get R)
                     (error "find-exn: value not found"))))

\\: `(iter.length Iter)` returns the number of values produced. It consumes the
\\: complete iterator, which must be finite.
(define iter.length { (iter.t A) --> number }
  Iter -> (let R (box.make 0)
               _ (Iter (/. X (box.incr R)))
            (box.unbox R)))

\\: `(iter.empty? Iter)` returns whether `Iter` produces no values. It stops after
\\: the first value, if there is one.
(define iter.empty?
  { (iter.t A) --> boolean }
  Iter -> (with-return Return
            (do (Iter (/. X (Return false)))
                true)))

\\: == Transformation

\\: `(iter.filter Test Iter)` produces only the values that satisfy `Test`, preserving
\\: their order.
(define iter.filter
  { (A --> boolean) --> (iter.t A) --> (iter.t A) }
  F Iter Yield -> (Iter (/. X (if (F X)
                                  (Yield X)
                                  (void)))))

\\: `(iter.append IterL IterR)` produces every value from `IterL`, then every value
\\: from `IterR`.
(define iter.append
  { (iter.t A) --> (iter.t A) --> (iter.t A) }
  IterL IterR Yield -> (do (IterL Yield)
                           (IterR Yield)))

\\: `(iter.concat IterList)` concatenates a list of iterators in list order.
(define iter.concat
  { (list (iter.t A)) --> (iter.t A) }
  [] _ -> (void)
  [Iter | Rest] Yield -> (do (Iter Yield)
                             (iter.concat Rest Yield)))

\\: `(iter.flatten ItersIter)` produces every value from each inner iterator in the
\\: order that `ItersIter` produces those iterators.
(define flatten
  { (iter.t (iter.t A)) --> (iter.t A) }
  Iters Yield -> (Iters (/. Iter (Iter Yield))))

\\: `(iter.flat-map F Iter)` replaces each value with the iterator returned by `F`
\\: and concatenates those iterators in source order.
(define flat-map
  { (A --> (iter.t B)) --> (iter.t A) --> (iter.t B) }
  F Iter Yield -> (Iter (/. X (F X Yield))))

\\: `(iter.flat-map-l F Iter)` replaces each value with the list returned by `F` and
\\: produces the lists' values in source order.
(define flat-map-l
  { (A --> (list B)) --> (iter.t A) --> (iter.t B) }
  F Iter Yield -> (Iter (/. X (list-for-each Yield (F X)))))

(define list-for-each
  { (A --> void) --> (list A) --> void }
  F [] -> (void)
  F [X | Rest] -> (do (F X)
                      (list-for-each F Rest)))

\\: `(iter.filter-map F Iter)` applies `F` to every value, unwraps and produces each
\\: `@some` result, and skips each `@none`.
(define filter-map
  { (A --> (maybe.t B)) --> (iter.t A) --> (iter.t B) }
  F Iter Yield -> (Iter (/. X (maybe.for-each Yield (F X)))))

\\: `(iter.filter-mapi F Iter)` applies `F` to each zero-based input index and value,
\\: unwraps and produces each `@some` result, and skips each `@none`. Indices count
\\: all input values, including skipped ones, and restart for every traversal.
(define filter-mapi
  { (number --> A --> (maybe.t B)) --> (iter.t A) --> (iter.t B) }
  F Iter Yield -> (let Index (box.make 0)
                    (Iter (/. X (let Res (F (box.unbox Index) X)
                                     _ (box.incr Index)
                                  (maybe.for-each Yield Res))))))

\\: `(iter.filter-count Test Iter)` returns the number of values that satisfy `Test`.
\\: It consumes the complete iterator, which must be finite.
(define filter-count
  { (A --> boolean) --> (iter.t A) --> number }
  F Iter -> (let Count (box.make 0)
                 _ (Iter (/. X (if (F X)
                                   (box.incr Count)
                                   (void))))
              (box.unbox Count)))

\\: `(iter.intersperse Separator Iter)` inserts `Separator` between adjacent values.
\\: It adds nothing before the first value or after the last, and an empty iterator
\\: remains empty.
(define intersperse
  { A --> (iter.t A) --> (iter.t A) }
  Elt Iter Yield -> (let First (box.make true)
                      (Iter (/. X (do (if (box.unbox First)
                                          (box.put First false)
                                          (Yield Elt))
                                      (Yield X))))))

\\: `(iter.keep-some Iter)` unwraps and produces every `@some` value and skips every
\\: `@none`.
(define keep-some
  { (iter.t (maybe.t A)) --> (iter.t A) }
  Iter Yield -> (Iter (/. X (maybe.for-each Yield X))))

\\: == Caching

\\: `(iter.persistent Iter)` immediately consumes all of `Iter` and returns an
\\: iterator over the cached values. Effects in `Iter` happen during this call and
\\: only once; later traversals replay the cache. `Iter` must be finite.
(define persistent
  { (iter.t A) --> (iter.t A) }
  Iter -> (let Cached (mlist.of-iter Iter)
            (/. Yield (mlist.to-iter Cached Yield))))

\\: `(iter.persistent-lazy Iter)` defers traversal until the result is first consumed.
\\: The first traversal produces values while building a cache; after that traversal
\\: completes, later traversals replay the cache without running `Iter` again.
\\:
\\: A traversal stopped early or terminated by an error is not cached. The next
\\: traversal invokes `Iter` again; a replayable source starts over, while a custom
\\: stateful source may produce something different. Consequently an infinite source
\\: may be short-circuited through this iterator, but no traversal of it can complete
\\: and establish the cache.
(define persistent-lazy
  { (iter.t A) --> (iter.t A) }
  Iter -> (let R (box.make (@none))
            (/. Yield
              (let MaybeIter (box.unbox R)
                (if (maybe.some? MaybeIter)
                    (let CachedIter (maybe.unsafe-get MaybeIter)
                      (CachedIter Yield))
                    (let NewIter (mlist.of-iter-with Iter Yield)
                      (box.put R (@some (mlist.to-iter NewIter)))))))))

\\: == Misc

\\: === List-like

\\: `(iter.head Iter)` returns `(@some X)` for the first value, or `@none` when the
\\: iterator is empty. It consumes at most one value and stops immediately.
(define iter.head
  { (iter.t A) --> (maybe.t A) }
  Iter -> (with-return Return
            (do (Iter (/. X (Return (@some X))))
                (@none))))

\\: `(iter.head-exn Iter)` returns the first value and stops immediately. It raises
\\: an error when the iterator is empty.
(define head-exn
  { (iter.t A) --> A }
  Iter -> (let R (iter.head Iter)
            (if (maybe.some? R)
                (maybe.unsafe-get R)
                (error "iter.head-exn called on empty iter"))))

\\: `(iter.take N Iter)` produces at most the first `N` values from `Iter`.
\\: For positive `N`, it stops immediately after producing the `N`th value and does
\\: not consume an extra value from `Iter`. Zero does not start `Iter`. Negative
\\: values of `N` are rejected. This can bound an infinite iterator.
(define iter.take
  { number --> (iter.t A) --> (iter.t A) }
  N _ _ -> (error "cannot take a negative amount from an iter") where (< N 0)
  0 _ _ -> (void)
  N Iter Yield -> (let Count (box.make 0)
                    (with-break Break
                      (Iter (/. X (do (box.incr Count)
                                     (Yield X)
                                     (if (= (box.unbox Count) N)
                                         (Break)
                                         (void))))))))

\\: `(iter.take-while P Iter)` produces values while `P` is true. It consumes but does
\\: not produce the first value for which `P` is false, then stops immediately.
(define take-while
  { (A --> boolean) --> (iter.t A) --> (iter.t A) }
  P Iter Yield -> (with-break Break
                    (Iter (/. X (if (P X)
                                    (Yield X)
                                    (Break))))))

\\: `(iter.fold-while F Init Iter)` calls `F` with the accumulator and each value.
\\: `F` returns the next accumulator paired with a continuation flag. The next
\\: accumulator is retained even when the flag is false, and traversal then stops
\\: immediately.
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

\\: `(iter.drop N Iter)` skips the first `N` values from `Iter`.
\\: Negative values of `N` are rejected.
(define iter.drop
  { number --> (iter.t A) --> (iter.t A) }
  N _ _ -> (error "cannot drop a negative amount from an iter") where (< N 0)
  N Iter Yield -> (let Count (box.make 0)
                    (Iter (/. X (if (>= (box.unbox Count) N)
                                    (Yield X)
                                    (box.incr Count))))))

\\: `(iter.drop-while P Iter)` skips the longest prefix whose values satisfy `P`,
\\: then produces the first failing value and everything after it. Once `P` fails it
\\: is not applied to later values.
(define drop-while
  { (A --> boolean) --> (iter.t A) --> (iter.t A) }
  P Iter Yield -> (let Drop (box.make true)
                    (Iter (/. X (if (box.unbox Drop)
                                    (if (P X)
                                        (void)
                                        (do (box.toggle Drop)
                                            (Yield X)))
                                    (Yield X))))))
\\: `(iter.reverse Iter)` immediately consumes all of `Iter` and returns an iterator
\\: over the cached values in reverse order. `Iter` must be finite.
(define iter.reverse
  { (iter.t A) --> (iter.t A) }
  Iter -> (let MList (mlist.of-iter Iter)
            (/. Yield (mlist.for-each-reverse Yield MList))))

\\: `(iter.enumerate Iter)` pairs every value with its zero-based index as
\\: `(@p Index Value)`. Indexing restarts at zero for every traversal.
(define enumerate
  { (iter.t A) --> (iter.t (number * A)) }
  Iter Yield -> (let R (box.make 0)
                  (Iter (/. X (let N (box.unbox R)
                                   _ (box.incr R)
                                (Yield (@p N X)))))))

\\: == Converters

\\: `(iter.to-list Iter)` returns the values in production order. It consumes the
\\: complete iterator, which must be finite.
(define to-list
  { (iter.t A) --> (list A) }
  Iter -> (reverse (to-list-reverse Iter)))

\\: `(iter.to-list-reverse Iter)` returns the values in reverse production order. It
\\: consumes the complete iterator, which must be finite.
(define to-list-reverse
  { (iter.t A) --> (list A) }
  Iter -> (fold (/. Acc X [X | Acc]) [] Iter))

\\: `(iter.of-list List)` produces the elements of `List` in list order.
(define of-list
  { (list A) --> (iter.t A) }
  [] Yield -> (void)
  [X | Rest] Yield -> (do (Yield X)
                          (of-list Rest Yield)))

\\: `(iter.convert-list F List)` converts `List` to an iterator, applies iterator
\\: transformation `F`, and collects the result back into a list in production
\\: order. The transformed iterator must be finite.
(define convert-list
  { ((iter.t A) --> (iter.t B)) --> (list A) --> (list B) }
  F List -> (to-list (F (of-list List))))

\\: `(iter.to-vector Iter)` returns a vector containing the values in production
\\: order. It consumes the complete iterator, which must be finite.
(define to-vector
  { (iter.t A) --> (vector A) }
  Iter -> (let MList (mlist.of-iter Iter)
               Limit (mlist.length MList)
            (let Vector (vector Limit)
                 _ (mlist.for-each-enumerated
                    (/. Pair (vector-> Vector (fst Pair) (snd Pair)))
                    MList)
              Vector)))

\\: `(iter.of-vector Vector)` produces the values at vector positions `1` through
\\: `(limit Vector)` in ascending order. An empty vector produces no values.
(define of-vector
  { (vector A) --> (iter.t A) }
  Vector _ -> (void) where (= 0 (limit Vector))
  Vector Yield -> (of-vector-range Vector 1 (limit Vector) Yield))

\\: `(iter.of-vector-enumerated Vector)` produces `(@p Position Value)` for vector
\\: positions `1` through `(limit Vector)` in ascending order. These positions are
\\: one-based, unlike the zero-based indices produced by `iter.enumerate`.
(define of-vector-enumerated
  { (vector A) --> (iter.t (number * A)) }
  Vector Yield -> (mlist.vector-for-each-enumerated Yield Vector 1 (+ 1 (limit Vector))))

\\: `(iter.of-vector-range Vector From To)` iterates over the inclusive range of
\\: vector positions from `From` to `To`. Positions increase when `From <= To`
\\: and decrease otherwise. Both endpoints must be valid positions in `Vector`.
(define of-vector-range
  { (vector A) --> number --> number --> (iter.t A) }
  Vector From To _ -> (error "iter.of-vector-range: Invalid range for vector with limit ~A: From=~A To=~A" (limit Vector) From To)
      where (or (< From 1) (> From (limit Vector))
                (< To 1) (> To (limit Vector)))
  Vector From To Yield -> (mlist.vector-for-each Yield Vector From (+ 1 To))
      where (<= From To)
  Vector From To Yield -> (of-vector-range-descending-h Vector From To Yield))

(define of-vector-range-descending-h
  { (vector A) --> number --> number --> (A --> void) --> void }
  Vector To To Yield -> (Yield (<-vector Vector To))
  Vector Position To Yield -> (do (Yield (<-vector Vector Position))
                                  (of-vector-range-descending-h Vector (- Position 1) To Yield)))

)
