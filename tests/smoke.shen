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
                (box.unbox Count)))))
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
