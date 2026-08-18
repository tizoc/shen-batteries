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
  (cons? (features.current)))

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
  "dict set returns and stores its value"
  [42 42]
  (let Dict (dict.make 4)
       Result (dict.set Dict answer 42)
    [Result (dict.get Dict answer)]))

(test.assert-equal
  "dict delete returns and removes its key"
  [answer missing]
  (let Dict (dict.make 4)
    (do (dict.set Dict answer 42)
        [(dict.delete Dict answer)
         (trap-error (dict.get Dict answer) (/. X missing))])))

(test.assert-equal
  "dict delete returns an absent key without changing the dictionary"
  [missing 1 42]
  (let Dict (dict.make 4)
    (do (dict.set Dict keep 42)
        [(dict.delete Dict missing)
         (length (dict.keys Dict))
         (dict.get Dict keep)])))

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
  "dict fold returns its initial accumulator for an empty dictionary"
  7
  (dict.fold (/. K V Acc (+ Acc (+ K V))) (dict.make 1) 7))

(test.assert-equal
  "dict count tracks insertions, replacements, and deletions"
  [0 1 1 0]
  (let Dict (dict.make 2)
       Empty (dict.count Dict)
       Insert (do (dict.set Dict key 1) (dict.count Dict))
       Replace (do (dict.set Dict key 2) (dict.count Dict))
       Delete (do (dict.delete Dict key) (dict.count Dict))
    [Empty Insert Replace Delete]))

(test.assert-equal
  "maybe bind transforms present values"
  (@some 42)
  (maybe.bind (@some 41) (/. X (@some (+ X 1)))))

(test.assert-equal
  "maybe bind does not evaluate its function for an absent value"
  [true 0]
  (let Calls (box.make 0)
       Result (maybe.bind (@none)
                          (/. X (do (box.incr Calls) (@some X))))
    [(maybe.none? Result) (box.unbox Calls)]))

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
          "nested with-break respects shadowing"
          1
          (test-with-exit.nested-break))
        (test.assert-equal
          "nested exit forms keep separate binders"
          1
          (test-with-exit.mixed-shadowing))
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
          (iter.to-list (iter.of-vector (vector 0))))
        (test.assert-equal
          "iter.of-vector-range includes ascending endpoints"
          [two three four]
          (iter.to-list
            (iter.of-vector-range
              (iter.to-vector (iter.of-list [one two three four]))
              2 4)))
        (test.assert-equal
          "iter.of-vector-range includes descending endpoints"
          [four three two]
          (iter.to-list
            (iter.of-vector-range
              (iter.to-vector (iter.of-list [one two three four]))
              4 2)))
        (test.assert-equal
          "iter.of-vector-range rejects out-of-bounds endpoints"
          rejected
          (trap-error
            (iter.to-list
              (iter.of-vector-range
                (iter.to-vector (iter.of-list [one two]))
                2 0))
            (/. X rejected))))
  true skip)

(test.assert-equal
  "let list destructuring"
  3
  (let [A | B] [1 2]
    (+ A (hd B))))

(test.assert-equal
  "defpattern registers for following files"
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
  "module source paths are portable and relative"
  [true false false false false]
  [(library.relative-source? "dir/file.shen")
   (library.relative-source? "/file.shen")
   (library.relative-source? "c#92;file.shen")
   (library.relative-source? (@s "dir" (@s "c#92;" "file.shen")))
   (library.relative-source? "C:/file.shen")])

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
  "shendoc rejects computed package external declarations"
  (@s "unsupported package external declaration: (external lazy)"
      (n->string 10))
  (trap-error
    (shendoc.generate "tests/fixtures/shendoc-inherited-input.shen")
    (/. E (error-to-string E))))

(set test.*shendoc-package-probe* false)

(test.assert-equal
  "shendoc does not evaluate package external declarations"
  [rejected false]
  [(trap-error
    (shendoc.generate "tests/fixtures/shendoc-unsafe-package.shen")
    (/. X rejected))
   (value test.*shendoc-package-probe*)])

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
  "seq length consumes a finite sequence"
  5
  (seq.length (seq.range 1 5)))

(test.assert-equal
  "seq.to-vector preserves order and traverses its source once"
  [3 [one two three]]
  (let Count (box.make 0)
       Vector (seq.to-vector
                (seq.map (/. X (do (box.incr Count) X))
                         (seq.of-list [one two three])))
    [(box.unbox Count) (seq.to-list (seq.of-vector Vector))]))

(test.assert-equal
  "seq.to-vector accepts an empty sequence"
  0
  (limit (seq.to-vector (seq.empty))))

(test.assert-equal
  "seq dictionary sources snapshot associations"
  [1 true true true true true]
  (let Dict (dict.make 3)
       _ (dict.set Dict first 10)
       Entries (seq.of-dict-entries Dict)
       Keys (seq.of-dict-keys Dict)
       Values (seq.of-dict-values Dict)
       _ (dict.set Dict second 20)
       EntryList (seq.to-list Entries)
       KeyList (seq.to-list Keys)
       ValueList (seq.to-list Values)
    [(length EntryList)
     (element? (@p first 10) EntryList)
     (element? first KeyList)
     (element? 10 ValueList)
     (not (element? second KeyList))
     (not (element? 20 ValueList))]))

(test.assert-equal
  "seq.to-dict keeps the last value for duplicate keys"
  [2 20 30]
  (let Dict (seq.to-dict
              (seq.of-list [(@p key 10) (@p other 30) (@p key 20)]))
    [(dict.count Dict) (dict.get Dict key) (dict.get Dict other)]))

(test.assert-equal
  "seq cexpr loads its runtime dependency"
  [1 2]
  (seq.to-list (seq.do (yield 1) (yield 2))))

(test.assert-equal
  "cexpr applies root delay and run once and delays combined tails"
  [root-run [delayed [combined [yielded 1] [delayed [yielded 2]]]]]
  (test.cexpr-trace.do (yield 1) (yield 2)))

(test.assert-equal
  "cexpr sends an empty body through the builder zero operation"
  [root-run [delayed zero]]
  (test.cexpr-trace.do))

(test.assert-equal
  "seq cexpr supports dependent monadic binds"
  [11 21 12 22]
  (seq.to-list
    (seq.do
      (bind X (seq.of-list [1 2]))
      (bind Y (seq.of-list [10 20]))
      (return (+ X Y)))))

(test.assert-equal
  "seq cexpr distinguishes return and return-from"
  [[one] [one two]]
  [(seq.to-list (seq.do (return one)))
   (seq.to-list (seq.do (return-from (seq.of-list [one two]))))])

(test.assert-equal
  "seq cexpr combines yield and yield-from"
  [one two three four]
  (seq.to-list
    (seq.do
      (yield one)
      (yield-from (seq.of-list [two three]))
      (yield four))))

(test.assert-equal
  "seq cexpr converts sources for for-bindings"
  [10 20]
  (seq.to-list
    (seq.do
      (for X [1 2])
      (return (* X 10)))))

(test.assert-equal
  "seq cexpr supports discarded monadic binds"
  [kept]
  (seq.to-list
    (seq.do
      (then (seq.singleton ignored))
      (yield kept))))

(test.assert-equal
  "cexpr supports ordinary local bindings"
  [42]
  (seq.to-list
    (seq.do
      (let X 41)
      (return (+ X 1)))))

(test.assert-equal
  "cexpr supports ordinary host effects"
  [1 [kept]]
  (let Count (box.make 0)
       Values (seq.to-list
                (seq.do
                  (effect (box.incr Count))
                  (yield kept)))
    [(box.unbox Count) Values]))

(test.assert-equal
  "cexpr supports an else-less conditional followed by more statements"
  [[after] [inside after]]
  [(seq.to-list
     (seq.do
       (if false (yield inside))
       (yield after)))
   (seq.to-list
     (seq.do
       (if true (yield inside))
       (yield after)))])

(test.assert-equal
  "cexpr supports conditional branches followed by more statements"
  [else after]
  (seq.to-list
    (seq.do
      (if false (yield then) (yield else))
      (yield after))))

(test.assert-equal
  "cexpr supports full conditional bodies grouped with do"
  [1 10 2 20]
  (seq.to-list
    (seq.do
      (if true
          (do (bind X (seq.of-list [1 2]))
              (yield X)
              (yield (* X 10)))
          (yield 0)))))

(test.assert-equal
  "grouped conditional bodies preserve discarded monadic binds"
  []
  (seq.to-list
    (seq.do
      (if true
          (do (then (seq.empty))
              (yield kept))
          (yield other)))))

(test.assert-equal
  "cexpr uses bind-return for a terminal bind and return"
  [root-run [delayed [bind-return source source]]]
  (test.cexpr-trace.do
    (bind X source)
    (return X)))

(test.assert-equal
  "cexpr falls back to bind and return when bind-return is unsupported"
  42
  (test.cexpr-monadic.do
    (bind X 41)
    (return (+ X 1))))

(test.assert-equal
  "applicative cexpr falls back to bind and return"
  42
  (test.cexpr-monadic.do
    (and (bind X 40)
         (bind Y 2))
    (return (+ X Y))))

(test.assert-equal
  "cexpr propagates errors from a supported bind-return operation"
  rejected
  (trap-error
    (cexpr.build (fn test.cexpr-broken-builder)
                 [[bind X source] [return X]])
    (/. E rejected)))

(test.assert-equal
  "applicative cexpr propagates bind-return errors"
  rejected
  (trap-error
    (cexpr.build (fn test.cexpr-broken-builder)
                 [[and [bind X 1] [bind Y 2]] [return (+ X Y)]])
    (/. E rejected)))

(test.assert-equal
  "seq cexpr supports two applicative bindings"
  [11 22]
  (seq.to-list
    (seq.do
      (and (bind X (seq.of-list [1 2]))
           (bind Y (seq.of-list [10 20])))
      (return (+ X Y)))))

(test.assert-equal
  "applicative bindings scope over the remaining body"
  [22 44]
  (seq.to-list
    (seq.do
      (and (bind X (seq.of-list [1 2]))
           (bind Y (seq.of-list [10 20])))
      (let Sum (+ X Y))
      (return (* Sum 2)))))

(test.assert-equal
  "seq cexpr supports three applicative bindings"
  [111 222]
  (seq.to-list
    (seq.do
      (and (bind X (seq.of-list [1 2]))
           (bind Y (seq.of-list [10 20]))
           (bind Z (seq.of-list [100 200])))
      (return (+ X (+ Y Z))))))

(test.assert-equal
  "seq cexpr supports four or more applicative bindings"
  [1111 2222]
  (seq.to-list
    (seq.do
      (and (bind A (seq.of-list [1 2]))
           (bind B (seq.of-list [10 20]))
           (bind C (seq.of-list [100 200]))
           (bind D (seq.of-list [1000 2000])))
      (return (+ A (+ B (+ C D)))))))

(test.assert-equal
  "generic cexpr applicative lowering supports four sources"
  10
  (test.cexpr-applicative4 1 2 3 4))

(test.assert-equal
  "maybe cexpr binds present values"
  (@some 5)
  (test.cexpr-maybe-map (@some 4)))

(test.assert-true
  "maybe cexpr short-circuits absent values"
  (maybe.none?
    (maybe.do
      (bind X (@none))
      (return (+ X 1)))))

(test.assert-equal
  "maybe cexpr supports return-from"
  (@some existing)
  (maybe.do (return-from (@some existing))))

(test.assert-equal
  "seq cexpr expansion typechecks in a definition"
  [2 3]
  (seq.to-list (test.cexpr-seq-map (seq.of-list [1 2]))))

(test.assert-equal
  "cexpr generator syntax works inside packages"
  [10 20]
  (seq.to-list (test-cexpr-package.generate)))

(test.assert-equal
  "defcexpr declares builders inside packages"
  42
  (test.cexpr-packaged-builder))

(test.assert-equal
  "cexpr local bindings typecheck in a definition"
  [4 6]
  (seq.to-list (test.cexpr-seq-let (seq.of-list [1 2]))))

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
