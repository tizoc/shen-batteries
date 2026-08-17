(define test.list-iter
  [] _ -> (void)
  [X | Xs] Yield -> (do (Yield X)
                        (test.list-iter Xs Yield)))

(define test.collect-mlist
  MList -> (let Values (box.make [])
                 _ (mlist.for-each-enumerated
                    (/. X (box.put Values [X | (box.unbox Values)]))
                    MList)
              (reverse (box.unbox Values))))

(define test.collect-mlist-reverse
  MList -> (let Values (box.make [])
                 _ (mlist.for-each-reverse
                    (/. X (box.put Values [X | (box.unbox Values)]))
                    MList)
              (reverse (box.unbox Values))))

(define test.mlist
  -> (mlist.of-iter (test.list-iter [1 2 3 4 5 6 7 8 9 10])))

(define test.counted-iter
  [] _ _ -> (void)
  [X | Xs] Count Yield -> (do (box.incr Count)
                              (Yield X)
                              (test.counted-iter Xs Count Yield)))

(test.assert-true
  "feature list is nonempty"
  (cons? (shen.x.features.current)))

(test.assert-equal
  "void composes with do"
  ok
  (do (void) ok))

(test.assert-equal
  "box updates"
  2
  (let Box (box.make 1)
    (do (box.incr Box)
        (box.unbox Box))))

(test.assert-equal
  "lazy.memo evaluates once"
  1
  (let Count (box.make 0)
       Lazy (lazy.memo (freeze (do (box.incr Count) 42)))
    (do (thaw Lazy)
        (thaw Lazy)
        (box.unbox Count))))

(test.assert-equal
  "lazy.memo caches every result value"
  1
  (let Count (box.make 0)
       Lazy (lazy.memo
              (freeze (do (box.incr Count) lazy.#not-thawed#7907#)))
    (do (thaw Lazy)
        (thaw Lazy)
        (box.unbox Count))))

(test.assert-equal
  "dict round trip"
  42
  (let Dict (dict.make 4)
    (do (dict.set Dict answer 42)
        (dict.get Dict answer))))

(test.assert-equal
  "dict delete returns and removes its key"
  [answer missing]
  (let Dict (dict.make 4)
    (do (dict.set Dict answer 42)
        [(dict.delete Dict answer)
         (trap-error (dict.get Dict answer) (/. X missing))])))

(test.assert-equal
  "dict traversal returns lists"
  [2 true true 2 true true]
  (let Dict (dict.make 4)
       SetFirst (dict.set Dict 1 10)
       SetSecond (dict.set Dict 2 20)
       Keys (dict.keys Dict)
       Values (dict.values Dict)
    [(length Keys) (element? 1 Keys) (element? 2 Keys)
     (length Values) (element? 10 Values) (element? 20 Values)]))

(test.assert-equal
  "dict fold threads its accumulator"
  330
  (let Dict (dict.make 4)
    (do (dict.set Dict 1 10)
        (dict.set Dict 2 20)
        (dict.fold (/. K V Acc (+ Acc (+ (* K 100) V))) Dict 0))))

(test.assert-equal
  "pipe-first macro"
  20
  (=> 2 (+ 3) (* 4)))

(test.assert-equal
  "doto returns the updated value"
  3
  (let Box (doto (box.make 1) (box.incr) (box.incr))
    (box.unbox Box)))

(test.assert-equal
  "doto accepts no operations"
  7
  (doto 7))

(shen.x.features.cond-expand
  shen/scheme
    (do (test.assert-equal
          "with-return typechecks"
          7
          (test-with-exit.return))
        (test.assert-equal
          "with-break typechecks"
          ok
          (do (test-with-exit.break) ok))
        (test.assert-equal
          "with-return exits early"
          7
          (with-return Return (+ 1 (Return 7))))
        (test.assert-equal
          "with-break exits early"
          1
          (let Count (box.make 0)
            (do (with-break Break
                  (do (box.incr Count)
                      (Break)
                      (box.incr Count)))
                (box.unbox Count))))
        (test.assert-equal
          "iter loads through its descriptor"
          [1 2 3]
          (iter.to-list (iter.of-list [1 2 3])))
        (test.assert-equal
          "iter Shen 41 compatibility APIs"
          [(@some 3) [3 4]]
          (let Iter (iter.of-list [1 2 3 4 5])
            [(iter.find (= 3) Iter)
             (iter.to-list
               (iter.take 2
                 (iter.drop 1
                   (iter.filter (/. X (> X 1)) Iter))))]))
        (test.assert-equal
          "iter.to-vector spans cache chunks"
          [1 2 3 4 5 6 7 8 9 10]
          (iter.to-list
            (iter.of-vector
              (iter.to-vector
                (iter.of-list [1 2 3 4 5 6 7 8 9 10])))))
        (test.assert-equal
          "iter.init is infinite and zero-based"
          [0 2 4 6]
          (iter.to-list
            (iter.take 4 (iter.init (/. N (* N 2))))))
        (test.assert-equal
          "cycling an empty iterator stays empty"
          []
          (iter.to-list (iter.cycle (iter.empty))))
        (test.assert-equal
          "iter.cycle repeats a nonempty iterator"
          [1 2 1 2 1]
          (iter.to-list
            (iter.take 5 (iter.cycle (iter.of-list [1 2])))))
        (test.assert-equal
          "iter.take zero consumes no upstream values"
          [0 []]
          (let Count (box.make 0)
               Values (iter.to-list
                        (iter.take 0
                          (test.counted-iter [1 2 3] Count)))
            [(box.unbox Count) Values]))
        (test.assert-equal
          "iter.take consumes exactly its result"
          [2 [1 2]]
          (let Count (box.make 0)
               Values (iter.to-list
                        (iter.take 2
                          (test.counted-iter [1 2 3] Count)))
            [(box.unbox Count) Values]))
        (test.assert-equal
          "iter.take rejects negative counts"
          rejected
          (trap-error
            (iter.to-list (iter.take -1 (iter.of-list [1 2 3])))
            (/. X rejected)))
        (test.assert-equal
          "iter.drop rejects negative counts"
          rejected
          (trap-error
            (iter.to-list (iter.drop -1 (iter.of-list [1 2 3])))
            (/. X rejected)))
        (test.assert-equal
          "iter.of-vector accepts an empty vector"
          []
          (iter.to-list (iter.of-vector (vector 0)))))
  true skip)

(test.assert-equal
  "let list destructuring"
  3
  (let [A | B] [1 2]
    (+ A (hd B))))

(test.assert-equal
  "defpattern registers before following definitions"
  (@p 1 2)
  (defpattern-fixture.match (@p 1 2)))

(test.assert-equal
  "maybe patterns use defpattern"
  1
  (test.match-maybe (@some 1)))

(test.assert-equal
  "nullable patterns use defpattern"
  2
  (test.match-nullable (@just 2)))

(test.assert-equal
  "lazy patterns use defpattern"
  3
  (test.match-lazy (freeze (+ 1 2))))

(test.assert-equal
  "loader API is present"
  1
  (arity library.use))

(test.assert-equal
  "loader restores the home directory"
  (value *test-home-directory*)
  (value *home-directory*))

(test.assert-equal
  "loader restores typechecking"
  (value *test-typechecking*)
  (tc?))

(test.assert-equal
  "mlist length spans chunks"
  10
  (mlist.length (test.mlist)))

(test.assert-equal
  "mlist enumeration spans chunks"
  [(@p 1 1) (@p 2 2) (@p 3 3) (@p 4 4) (@p 5 5)
   (@p 6 6) (@p 7 7) (@p 8 8) (@p 9 9) (@p 10 10)]
  (test.collect-mlist (test.mlist)))

(test.assert-equal
  "mlist reverse traversal skips unused slots"
  [10 9 8 7 6 5 4 3 2 1]
  (test.collect-mlist-reverse (test.mlist)))

(test.assert-equal
  "shendoc preserves comments, source forms, and packages"
  "= Fixture

Intro.

== API

==== `sample.make` : `(sample.t A) --> external`

Documents a private typed function.

==== `public`

Documents an external untyped function.

==== `sample.<token>`

Documents a grammar rule.

A detached note.

A final note.

"
  (shendoc.generate "tests/fixtures/shendoc-input.shen"))

(test.assert-equal
  "seq loads through its descriptor"
  [1 2 3]
  (seq.to-list (seq.range 1 3)))

(test.assert-equal
  "seq Shen 41 compatibility APIs"
  [3 4]
  (seq.to-list
    (seq.take 2
      (seq.drop 1
        (seq.filter (/. X (> X 1)) (seq.range 1 5))))))

(test.assert-equal
  "seq find uses its package API"
  (@some 3)
  (seq.find (/. X (= X 3)) (seq.range 1 5)))

(test.assert-equal
  "seq cexpr loads its runtime dependency"
  [1 2]
  (seq.to-list (:seq yield 1 yield 2)))

(test.assert-equal
  "seq.make rejects negative counts"
  rejected
  (trap-error (seq.make -1 x) (/. X rejected)))

(test.assert-true
  "cycling an empty sequence stays empty"
  (seq.empty? (seq.cycle (seq.empty))))

(test.assert-equal
  "seq.cycle repeats a nonempty sequence"
  [1 2 1 2 1]
  (seq.to-list
    (seq.take 5
      (seq.cycle (seq.of-list [1 2])))))

(test.assert-equal
  "seq.cycle stays lazy until traversal"
  [0 1 1]
  (let Count (box.make 0)
       Seq (seq.map (/. X (do (box.incr Count) X)) (seq.singleton 1))
       Cycled (seq.cycle Seq)
       Before (box.unbox Count)
       Head (seq.head Cycled)
    [Before Head (box.unbox Count)]))

(test.assert-equal
  "seq.flatten consumes outer sequences lazily"
  [0 [1] 1]
  (let Count (box.make 0)
       Outer (seq.map (/. X (do (box.incr Count) (seq.singleton X)))
                      (seq.of-list [1 2 3]))
       Flattened (seq.flatten Outer)
       Before (box.unbox Count)
       Values (seq.to-list (seq.take 1 Flattened))
    [Before Values (box.unbox Count)]))

(test.assert-equal
  "seq.take zero consumes no source values"
  [0 []]
  (let Count (box.make 0)
       Source (seq.map (/. X (do (box.incr Count) X))
                       (seq.forever (freeze 1)))
       Values (seq.to-list (seq.take 0 Source))
    [(box.unbox Count) Values]))

(test.assert-equal
  "seq.take consumes exactly its result"
  [2 [1 1]]
  (let Count (box.make 0)
       Source (seq.map (/. X (do (box.incr Count) X))
                       (seq.forever (freeze 1)))
       Values (seq.to-list (seq.take 2 Source))
    [(box.unbox Count) Values]))

(test.assert-equal
  "seq.truncate zero consumes no source values"
  [0 []]
  (let Count (box.make 0)
       Source (seq.map (/. X (do (box.incr Count) X))
                       (seq.forever (freeze 1)))
       Values (seq.to-list (seq.truncate 0 Source))
    [(box.unbox Count) Values]))

(test.assert-equal
  "seq.truncate consumes exactly its result"
  [2 [1 1]]
  (let Count (box.make 0)
       Source (seq.map (/. X (do (box.incr Count) X))
                       (seq.forever (freeze 1)))
       Values (seq.to-list (seq.truncate 2 Source))
    [(box.unbox Count) Values]))

(test.assert-equal
  "seq.take rejects negative counts"
  rejected
  (trap-error (seq.take -1 (seq.empty)) (/. X rejected)))

(test.assert-equal
  "seq.truncate rejects negative counts"
  rejected
  (trap-error (seq.truncate -1 (seq.empty)) (/. X rejected)))

(test.assert-equal
  "seq.drop rejects negative counts"
  rejected
  (trap-error (seq.drop -1 (seq.empty)) (/. X rejected)))
