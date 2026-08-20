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

(define test.counted-seq
  Values Count -> (seq.map (/. X (do (box.incr Count) X))
                           (seq.of-list Values)))

(define test.delayed-error-seq
  Count -> (seq.map (/. X (do (box.incr Count)
                              (simple-error "later sequence was forced")))
                   (seq.singleton ignored)))

(test.assert-true
  "feature list is nonempty"
  (cons? (features.current)))

(test.assert-true
  "features.add is idempotent"
  (let Feature shen-batteries/test-idempotent-feature
       First (do (features.add Feature) (features.current))
       Second (do (features.add Feature) (features.current))
    (and (element? Feature First) (= First Second))))

\\ Make a fixture-owned feature visible before the following source is expanded.
(features.add shen-batteries/documented-examples)

(let Typechecking (if (tc?) + -)
  (do (tc +)
      (load "tests/fixtures/documented-examples.shen")
      (tc Typechecking)))

(test.assert-equal
  "documented compound feature conditions select in a following source"
  [or-selected and-selected]
  (test.documented-feature-selection))

(test.assert-equal
  "documented verified-and predicate typechecks and evaluates"
  [true false false]
  [(test.documented-positive-number? 1)
   (test.documented-positive-number? -1)
   (test.documented-positive-number? not-a-number)])

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
  "box predicates and modification helpers"
  [true false 5 false]
  (let Number (box.make 3)
       Flag (box.make true)
    (do (box.modify (/. X (* X 2)) Number)
        (box.decr Number)
        (box.toggle Flag)
        [(box.box? Number)
         (box.box? 3)
         (box.unbox Number)
         (box.unbox Flag)])))

(test.assert-equal
  "lazy.memo evaluates once"
  1
  (let Count (box.make 0)
       Lazy (lazy.memo (freeze (do (box.incr Count) 42)))
    (do (thaw Lazy)
        (thaw Lazy)
        (box.unbox Count))))

(test.assert-equal
  "lazy.memo retries after an error and caches the successful result"
  ["first attempt failed" 1 42 2 42 2]
  (let Attempts (box.make 0)
       Lazy (lazy.memo
              (freeze
                (do (box.incr Attempts)
                    (if (= 1 (box.unbox Attempts))
                        (simple-error "first attempt failed")
                        42))))
       First (trap-error (thaw Lazy) (/. Error (error-to-string Error)))
       AfterFirst (box.unbox Attempts)
       Second (thaw Lazy)
       AfterSecond (box.unbox Attempts)
       Third (thaw Lazy)
    [First AfterFirst Second AfterSecond Third (box.unbox Attempts)]))

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
  "dict.is? distinguishes dictionaries"
  [true false]
  [(dict.is? (dict.make 1)) (dict.is? not-a-dictionary)])

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

(test.assert-error-contains
  "dict rejects a zero size hint"
  "dict.make requires a positive size hint"
  (freeze (dict.make 0)))

(test.assert-error-contains
  "dict rejects a negative size hint"
  "dict.make requires a positive size hint"
  (freeze (dict.make -1)))

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
  "documented maybe map and bind preserve or join optional layers"
  [(@some (@some 4)) (@some 4) true]
  [(maybe.map (fn test.documented-below-ten) (@some 4))
   (maybe.bind (@some 4) (fn test.documented-below-ten))
   (maybe.none?
     (maybe.bind (@some 12) (fn test.documented-below-ten)))])

(test.assert-equal
  "maybe predicates preserve nested optional values"
  [true false false true false true]
  [(maybe.none? (@none))
   (maybe.some? (@none))
   (maybe.none? (@some value))
   (maybe.some? (@some value))
   (maybe.none? (@some (@none)))
   (maybe.some? (@some (@none)))])

(test.assert-equal
  "maybe checked and unchecked extraction agree for present values"
  [42 42]
  [(maybe.get (@some 42))
   (maybe.unsafe-get (@some 42))])

(test.assert-error-contains
  "maybe.get rejects an absent value"
  "Not a @some value"
  (freeze (maybe.get (@none))))

(test.assert-equal
  "maybe.get/or evaluates its default only for absence"
  [present 0 fallback 1]
  (let Calls (box.make 0)
       Present (maybe.get/or
                 (@some present)
                 (freeze (do (box.incr Calls) unused)))
       AfterPresent (box.unbox Calls)
       Absent (maybe.get/or
                (@none)
                (freeze (do (box.incr Calls) fallback)))
    [Present AfterPresent Absent (box.unbox Calls)]))

(test.assert-equal
  "maybe.map calls its function only for a present value"
  [(@some 2) true 1]
  (let Calls (box.make 0)
       F (/. X (do (box.incr Calls) (+ X 1)))
       Present (maybe.map F (@some 1))
       Absent (maybe.map F (@none))
    [Present (maybe.none? Absent) (box.unbox Calls)]))

(test.assert-equal
  "maybe.for-each performs only the present effect and returns void"
  [(void) (void) [value]]
  (let Seen (box.make [])
       Present (maybe.for-each
                 (/. X (box.put Seen [X | (box.unbox Seen)]))
                 (@some value))
       Absent (maybe.for-each
                (/. X (box.put Seen [unexpected | (box.unbox Seen)]))
                (@none))
    [Present Absent (box.unbox Seen)]))

(test.assert-equal
  "nullable values use an identity representation and collapse null nesting"
  [value true true false]
  [(@just value)
   (= (@just value) value)
   (null? (@just (@null)))
   (null? (@just value))])

(test.assert-equal
  "pipe-first macro"
  20
  (=> 2 (+ 3) (* 4)))

(test.assert-equal
  "pipe-last supports parenthesized and bare stages"
  [6 4 2]
  (=>> [1 2 3] (map (/. X (* X 2))) reverse))

(test.assert-equal
  "pipe bare stages are one-place calls"
  [[3 2 1] [3 2 1]]
  [(=> [1 2 3] reverse)
   (=>> [1 2 3] reverse)])

(test.assert-equal
  "zero-stage pipes are identities"
  [value value]
  [(=> value) (=>> value)])

(test.assert-equal
  "pipe macros remain external inside packages"
  [20 [6 4 2] 2]
  [(test-pipe-package.scaled 2)
   (test-pipe-package.doubled-reversed [1 2 3])
   (test-pipe-package.bumped)])

(test.assert-equal
  "doto returns the updated value"
  3
  (let Box (doto (box.make 1) (box.incr) (box.incr))
    (box.unbox Box)))

(test.assert-equal
  "doto evaluates its target once, orders operations, and returns the target"
  [1 [first second] 99]
  (let Builds (box.make 0)
       Seen (box.make [])
       Target (box.make 0)
       Result (doto (do (box.incr Builds) Target)
                (test.pipe-record-step Seen first)
                (test.pipe-record-step Seen second))
    (do (box.put Result 99)
        [(box.unbox Builds)
         (box.unbox Seen)
         (box.unbox Target)])))

(test.assert-equal
  "doto accepts no operations"
  7
  (doto 7))

(do (test.assert-equal
          "with-return typechecks"
          7
          (test-with-exit.return))
        (test.assert-equal
          "with-return completes with its body result"
          42
          (with-return Return (+ 20 22)))
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
          "documented finite seq and iter pipelines agree"
          [[30 40 50] [30 40 50]]
          [(seq.to-list
             (seq.map (/. X (* X 10))
               (seq.filter (/. X (> X 2))
                 (seq.range 1 5))))
           (iter.to-list
             (iter.map (/. X (* X 10))
               (iter.filter (/. X (> X 2))
                 (iter.of-list [1 2 3 4 5]))))])
        (test.assert-equal
          "documented infinite seq and iter pipelines terminate at a prefix"
          [[0 2 4 6 8] [0 2 4 6 8]]
          [(seq.to-list
             (seq.truncate 5
               (seq.map (/. N (* N 2))
                 (seq.unfold
                   (/. N (@some (@p N (+ N 1))))
                   0))))
           (iter.to-list
             (iter.take 5
               (iter.map (/. N (* N 2))
                 (iter.init (/. N N)))))])
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
          "ordinary iterators rerun their producer on every traversal"
          [6 [1 2 3] [1 2 3]]
          (let Count (box.make 0)
               Iter (test.counted-iter [1 2 3] Count)
               First (iter.to-list Iter)
               Second (iter.to-list Iter)
            [(box.unbox Count) First Second]))
        (test.assert-equal
          "iter.persistent eagerly consumes once and replays its cache"
          [3 3 3 [1 2 3] [1 2 3]]
          (let Count (box.make 0)
               Iter (iter.persistent
                      (test.counted-iter [1 2 3] Count))
               AfterCreation (box.unbox Count)
               First (iter.to-list Iter)
               AfterFirst (box.unbox Count)
               Second (iter.to-list Iter)
            [AfterCreation AfterFirst (box.unbox Count) First Second]))
        (test.assert-equal
          "iter.persistent-lazy waits, then caches a completed traversal"
          [0 3 3 [1 2 3] [1 2 3]]
          (let Count (box.make 0)
               Iter (iter.persistent-lazy
                      (test.counted-iter [1 2 3] Count))
               Before (box.unbox Count)
               First (iter.to-list Iter)
               AfterFirst (box.unbox Count)
               Second (iter.to-list Iter)
            [Before AfterFirst (box.unbox Count) First Second]))
        (test.assert-equal
          "iter.persistent-lazy retries after an interrupted traversal"
          [(@some 1) 1 [1 2 3] 4 [1 2 3] 4]
          (let Count (box.make 0)
               Iter (iter.persistent-lazy
                      (test.counted-iter [1 2 3] Count))
               Head (iter.head Iter)
               AfterHead (box.unbox Count)
               Complete (iter.to-list Iter)
               AfterComplete (box.unbox Count)
               Replay (iter.to-list Iter)
            [Head AfterHead Complete AfterComplete Replay (box.unbox Count)]))
        (test.assert-equal
          "iter.persistent-lazy retries after a failed traversal"
          [failed 1 [1 2] 2 [1 2] 2]
          (let Attempts (box.make 0)
               Source (/. Yield
                        (do (box.incr Attempts)
                            (if (= 1 (box.unbox Attempts))
                                (do (Yield 1)
                                    (simple-error "expected iterator failure"))
                                (test.list-iter [1 2] Yield))))
               Iter (iter.persistent-lazy Source)
               Failed (trap-error (iter.to-list Iter) (/. Error failed))
               AfterFailure (box.unbox Attempts)
               Complete (iter.to-list Iter)
               AfterComplete (box.unbox Attempts)
               Replay (iter.to-list Iter)
            [Failed AfterFailure Complete AfterComplete Replay
             (box.unbox Attempts)]))
        (test.assert-equal
          "iter.head consumes only its result"
          [(@some 1) 1]
          (let Count (box.make 0)
               Result (iter.head (test.counted-iter [1 2 3] Count))
            [Result (box.unbox Count)]))
        (test.assert-equal
          "iter.find stops at its first match"
          [(@some 3) 3]
          (let Count (box.make 0)
               Result (iter.find (= 3)
                        (test.counted-iter [1 2 3 4] Count))
            [Result (box.unbox Count)]))
        (test.assert-equal
          "iter.exists? stops at its first match"
          [true 2]
          (let Count (box.make 0)
               Result (iter.exists? (= 2)
                        (test.counted-iter [1 2 3 4] Count))
            [Result (box.unbox Count)]))
        (test.assert-equal
          "iter.unfoldr produces values until @none"
          [[0 1 2] []]
          [(iter.to-list
             (iter.unfoldr
               (/. N (if (< N 3)
                         (@some (@p N (+ N 1)))
                         (@none)))
               0))
           (iter.to-list (iter.unfoldr (/. Ignored (@none)) ignored))])
        (test.assert-equal
          "iter.for-all? handles empty, complete, and short-circuited traversals"
          [true true false 3]
          (let Count (box.make 0)
               Failed (iter.for-all? (/. X (< X 3))
                        (test.counted-iter [1 2 3 4] Count))
            [(iter.for-all? (/. Ignored false) (iter.empty))
             (iter.for-all? (/. X (< X 3)) (iter.of-list [1 2]))
             Failed
             (box.unbox Count)]))
        (test.assert-equal
          "iter.find-map stops at its first @some and reports no match"
          [(@some 30) 3 true]
          (let Count (box.make 0)
               Present
                (iter.find-map
                  (/. X (if (> X 2) (@some (* X 10)) (@none)))
                  (test.counted-iter [1 2 3 4] Count))
               Absent
                (iter.find-map (/. Ignored (@none)) (iter.of-list [1 2]))
            [Present (box.unbox Count) (maybe.none? Absent)]))
        (test.assert-equal
          "iter.intersperse handles empty, singleton, and longer iterators"
          [[] [a] [a separator b separator c]]
          [(iter.to-list (iter.intersperse separator (iter.empty)))
           (iter.to-list
             (iter.intersperse separator (iter.singleton a)))
           (iter.to-list
             (iter.intersperse separator (iter.of-list [a b c])))])
        (test.assert-equal
          "iter.reverse eagerly consumes once and replays its cache"
          [3 [3 2 1] [3 2 1] 3]
          (let Count (box.make 0)
               Reversed
                (iter.reverse (test.counted-iter [1 2 3] Count))
               AfterCreation (box.unbox Count)
               First (iter.to-list Reversed)
               Second (iter.to-list Reversed)
            [AfterCreation First Second (box.unbox Count)]))
        (test.assert-equal
          "iter.take-while consumes its first rejected value"
          [3 [1 2]]
          (let Count (box.make 0)
               Values (iter.to-list
                        (iter.take-while (/. X (< X 3))
                          (test.counted-iter [1 2 3 4] Count)))
            [(box.unbox Count) Values]))
        (test.assert-equal
          "iter.fold-while retains the accumulator from its stopping value"
          [6 3]
          (let Count (box.make 0)
               Result (iter.fold-while
                        (/. Acc X (@p (+ Acc X) (< X 3)))
                        0
                        (test.counted-iter [1 2 3 4] Count))
            [Result (box.unbox Count)]))
        (test.assert-error-contains
          "iter.take rejects negative counts"
          "cannot take a negative amount from an iter"
          (freeze
            (iter.to-list (iter.take -1 (iter.of-list [1 2 3])))))
        (test.assert-error-contains
          "iter.drop rejects negative counts"
          "cannot drop a negative amount from an iter"
          (freeze
            (iter.to-list (iter.drop -1 (iter.of-list [1 2 3])))))
        (test.assert-equal
          "iter.from-lazy thaws until @none"
          [1 2 3]
          (let State (box.make [1 2 3])
            (iter.to-list
              (iter.from-lazy
                (freeze
                  (let Values (box.unbox State)
                    (if (= Values [])
                        (@none)
                        (do (box.put State (tl Values))
                            (@some (hd Values))))))))))
        (test.assert-equal
          "iter.scan includes its initial accumulator"
          [0 1 3 6]
          (iter.to-list
            (iter.scan (/. Acc X (+ Acc X))
                       0
                       (iter.of-list [1 2 3]))))
        (test.assert-equal
          "iter stateful maps reset and advance through skipped outputs"
          [[1 3] [1 3] [3]]
          (let FoldMapped
                (iter.fold-map
                  (/. Acc X (let Next (+ Acc X) (@p Next Next)))
                  0
                  (iter.of-list [1 2]))
               First (iter.to-list FoldMapped)
               Second (iter.to-list FoldMapped)
               FilterMapped
                (iter.to-list
                  (iter.fold-filter-map
                    (/. Acc X
                      (let Next (+ Acc X)
                        (@p Next
                            (if (= X 2) (@some Next) (@none)))))
                    0
                    (iter.of-list [1 2 3])))
            [First Second FilterMapped]))
        (test.assert-equal
          "iter input indices and vector positions use their documented bases"
          [[(@p 0 a) (@p 1 b)] [(@p 1 a) (@p 2 b)] [2]]
          (let Vector (iter.to-vector (iter.of-list [a b]))
            [(iter.to-list (iter.enumerate (iter.of-list [a b])))
             (iter.to-list (iter.of-vector-enumerated Vector))
             (iter.to-list
               (iter.filter-mapi
                 (/. Index X (if (= X c) (@some Index) (@none)))
                 (iter.of-list [a b c])))]))
        (test.assert-equal
          "iter.drop-while stops testing after the prefix"
          [3 [3 1]]
          (let Tests (box.make 0)
               Values
                (iter.to-list
                  (iter.drop-while
                    (/. X (do (box.incr Tests) (< X 3)))
                    (iter.of-list [1 2 3 1])))
            [(box.unbox Tests) Values]))
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
        (test.assert-error-contains
          "iter.of-vector-range rejects out-of-bounds endpoints"
          "iter.of-vector-range: Invalid range for vector with limit 2"
          (freeze
            (iter.to-list
              (iter.of-vector-range
                (iter.to-vector (iter.of-list [one two]))
                2 0)))))

(test.assert-equal
  "let list destructuring"
  3
  (let [A | B] [1 2]
    (+ A (hd B))))

(test.assert-equal
  "let tuple destructuring"
  42
  (let (@p A B) (@p 20 22)
    (+ A B)))

(test.assert-equal
  "let destructuring supports discarded components"
  [1 2]
  [(let (@p A _) (@p 1 ignored) A)
   (let [_ | Tail] [ignored 2] (hd Tail))])

(test.assert-equal
  "discarded destructuring components evaluate non-variable inputs once"
  [[1 1] [2 2] 2]
  (let Count (box.make 0)
    [(let [Head | _] (do (box.incr Count) [1 2])
       [Head (box.unbox Count)])
     (let (@p _ Right) (do (box.incr Count) (@p ignored 2))
       [Right (box.unbox Count)])
     (box.unbox Count)]))

(test.assert-equal
  "let destructuring evaluates its input once"
  [1 42]
  (let Count (box.make 0)
    (let (@p A B) (do (box.incr Count) (@p 20 22))
      [(box.unbox Count) (+ A B)])))

(test.assert-equal
  "lambdas destructure through an inner let"
  42
  ((/. Pair
     (let (@p A B) Pair
       (+ A B)))
   (@p 20 22)))

(test.assert-equal
  "malformed destructuring reports an accessor error"
  rejected
  (trap-error
    (let (@p A B) not-a-tuple
      (+ A B))
    (/. Error rejected)))

(test.assert-equal
  "defpattern decodes a polyadic bit pattern in a following file"
  [1 2 1 5 8]
  (defpattern-guide.read-small-frame
    (defpattern-guide.bits [130 133 255])))

(test.assert-equal
  "defpattern macro supports another polyadic bit-pattern arity"
  [1 2 4]
  (defpattern-guide.read-short-prefix
    (defpattern-guide.bits [160])))

(test.assert-equal
  "defpattern nested literal rejects reserved WebSocket bits"
  []
  (defpattern-guide.read-small-frame
    (defpattern-guide.bits [146 133 255])))

(test.assert-equal
  "defpattern bounds discriminator rejects a truncated bit cursor"
  []
  (defpattern-guide.read-small-frame
    (defpattern-guide.bits [130])))

(test.assert-equal
  "defpattern type discriminator safely rejects an unrelated value"
  []
  (defpattern-guide.read-small-frame ordinary-value))

(test.assert-equal
  "maybe patterns use defpattern"
  1
  (test.match-maybe (@some 1)))

(test.assert-equal
  "maybe patterns discriminate polymorphic values before extraction"
  [present absent ordinary ordinary ordinary]
  [(test.classify-maybe-pattern (@some value))
   (test.classify-maybe-pattern (@none))
   (test.classify-maybe-pattern value)
   (test.classify-maybe-pattern (absvector 0))
   (test.classify-maybe-pattern (@v value <>))])

(test.assert-equal
  "nullable patterns use defpattern"
  2
  (test.match-nullable (@just 2)))

(test.assert-equal
  "nullable patterns distinguish present and absent values"
  [present absent]
  [(test.classify-nullable-pattern (@just value))
   (test.classify-nullable-pattern (@null))])

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
  "// Generated by Shendoc; edit the source documentation comments instead.

= Fixture

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

(do (test.assert-error-contains
      "shendoc rejects an unsafe package external declaration"
      "unsupported package external declaration"
      (freeze
        (shendoc.generate "tests/fixtures/shendoc-unsafe-package.shen")))
    (test.assert-equal
      "shendoc does not evaluate package external declarations"
      false
      (value test.*shendoc-package-probe*)))

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
  "seq.map2 does not force its second source when the first is empty"
  [[] 0]
  (let Pulls (box.make 0)
       Result (trap-error
                (seq.to-list
                  (seq.map2 (/. X Y [X Y])
                            (seq.empty)
                            (test.delayed-error-seq Pulls)))
                (/. Error later-source-forced))
    [Result (box.unbox Pulls)]))

(test.assert-equal
  "seq.map2 stops pulling from the right when the left source ends"
  [[11] 1 1]
  (let LeftPulls (box.make 0)
       RightPulls (box.make 0)
       Values (seq.to-list
                (seq.map2 (fn +)
                          (test.counted-seq [1] LeftPulls)
                          (test.counted-seq [10 20] RightPulls)))
    [Values (box.unbox LeftPulls) (box.unbox RightPulls)]))

(test.assert-equal
  "seq.map3 stops pulling later sources when the first source ends"
  [[111] 1 1 1]
  (let FirstPulls (box.make 0)
       SecondPulls (box.make 0)
       ThirdPulls (box.make 0)
       Values (seq.to-list
                (seq.map3 (/. X Y Z (+ X (+ Y Z)))
                          (test.counted-seq [1] FirstPulls)
                          (test.counted-seq [10 20] SecondPulls)
                          (test.counted-seq [100 200] ThirdPulls)))
    [Values
     (box.unbox FirstPulls)
     (box.unbox SecondPulls)
     (box.unbox ThirdPulls)]))

(test.assert-equal
  "seq.map3 checks its sources from left to right at the shortest boundary"
  [[111] 2 1 1]
  (let FirstPulls (box.make 0)
       SecondPulls (box.make 0)
       ThirdPulls (box.make 0)
       Values (seq.to-list
                (seq.map3 (/. X Y Z (+ X (+ Y Z)))
                          (test.counted-seq [1 2] FirstPulls)
                          (test.counted-seq [10] SecondPulls)
                          (test.counted-seq [100 200] ThirdPulls)))
    [Values
     (box.unbox FirstPulls)
     (box.unbox SecondPulls)
     (box.unbox ThirdPulls)]))

(test.assert-equal
  "seq.zip-with does not force its right source when the left is empty"
  [[] 0]
  (let Pulls (box.make 0)
       Result (trap-error
                (seq.to-list
                  (seq.zip-with (/. X Y [X Y])
                                (seq.empty)
                                (test.delayed-error-seq Pulls)))
                (/. Error later-source-forced))
    [Result (box.unbox Pulls)]))

(test.assert-equal
  "seq.zip does not force its right source when the left is empty"
  [[] 0]
  (let Pulls (box.make 0)
       Result (trap-error
                (seq.to-list
                  (seq.zip (seq.empty)
                           (test.delayed-error-seq Pulls)))
                (/. Error later-source-forced))
    [Result (box.unbox Pulls)]))

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

(test.assert-error-contains
  "cexpr propagates errors from a supported bind-return operation"
  "broken bind-return optimizer"
  (freeze
    (cexpr.build (fn test.cexpr-broken-builder)
                 [[bind X source] [return X]])))

(test.assert-error-contains
  "applicative cexpr propagates bind-return errors"
  "broken bind-return optimizer"
  (freeze
    (cexpr.build (fn test.cexpr-broken-builder)
                 [[and [bind X 1] [bind Y 2]] [return result]])))

(test.assert-equal
  "seq cexpr supports two applicative bindings"
  [11 22]
  (seq.to-list
    (seq.do
      (and (bind X (seq.of-list [1 2]))
           (bind Y (seq.of-list [10 20])))
      (return (+ X Y)))))

(test.assert-equal
  "seq applicative bindings do not force a later source after an empty first source"
  [[] 0]
  (let Pulls (box.make 0)
       Result (trap-error
                (seq.to-list
                  (seq.do
                    (and (bind X (seq.empty))
                         (bind Y (test.delayed-error-seq Pulls)))
                    (return [X Y])))
                (/. Error later-source-forced))
    [Result (box.unbox Pulls)]))

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
  "seq applicative merging leaves all later sources untouched after an empty first source"
  [[] 0]
  (let Pulls (box.make 0)
       Result (trap-error
                (seq.to-list
                  (seq.do
                    (and (bind A (seq.empty))
                         (bind B (test.delayed-error-seq Pulls))
                         (bind C (test.delayed-error-seq Pulls))
                         (bind D (test.delayed-error-seq Pulls)))
                    (return [A B C D])))
                (/. Error later-source-forced))
    [Result (box.unbox Pulls)]))

(test.assert-equal
  "generic cexpr applicative lowering supports four sources"
  10
  (test.cexpr-applicative4 1 2 3 4))

(test.assert-equal
  "maybe cexpr binds present values"
  (@some 5)
  (test.cexpr-maybe-map (@some 4)))

(test.assert-equal
  "maybe cexpr typed pipelines bind, let, return, and short-circuit"
  [(@some 14) true]
  [(test.cexpr-maybe-pipeline 4)
   (maybe.none? (test.cexpr-maybe-pipeline -1))])

(test.assert-equal
  "documented maybe cexpr pipeline supports dependent binds"
  [(@some 14) true]
  [(test.documented-maybe-pipeline 4)
   (maybe.none? (test.documented-maybe-pipeline 8))])

(test.assert-true
  "empty maybe cexpr produces absence"
  (maybe.none? (maybe.do)))

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
  "maybe cexpr return distinguishes lifting from forwarding"
  [(@some (@some value)) (@some value)]
  [(maybe.do (return (@some value)))
   (maybe.do (return-from (@some value)))])

(test.assert-equal
  "maybe cexpr yield distinguishes lifting from forwarding"
  [(@some (@some value)) (@some value)]
  [(maybe.do (yield (@some value)))
   (maybe.do (yield-from (@some value)))])

(test.assert-equal
  "maybe cexpr evaluates lifted computations from left to right"
  [(@some second) [first second]]
  (let Seen (box.make [])
       Result (maybe.do
                (yield (do (box.put Seen [first | (box.unbox Seen)]) first))
                (yield (do (box.put Seen [second | (box.unbox Seen)]) second)))
    [Result (reverse (box.unbox Seen))]))

(test.assert-equal
  "maybe cexpr then short-circuits effects after absence"
  [(@some done) 1 true 1]
  (let Calls (box.make 0)
       Present (maybe.do
                 (then (@some ignored))
                 (effect (box.incr Calls))
                 (return done))
       AfterPresent (box.unbox Calls)
       Absent (maybe.do
                (then (@none))
                (effect (box.incr Calls))
                (return unreachable))
    [Present AfterPresent (maybe.none? Absent) (box.unbox Calls)]))

(test.assert-equal
  "maybe cexpr effects discard ordinary results without short-circuiting"
  (@some continued)
  (maybe.do
    (effect (@none))
    (return continued)))

(test.assert-equal
  "maybe cexpr one-armed false conditionals stop the remainder"
  [true 0]
  (let Calls (box.make 0)
       Result (maybe.do
                (if false (return unreachable))
                (effect (box.incr Calls))
                (return also-unreachable))
    [(maybe.none? Result) (box.unbox Calls)]))

(test.assert-equal
  "maybe cexpr selects a complete conditional branch"
  (@some right)
  (maybe.do
    (if false
        (return left)
        (return right))))

(test.assert-equal
  "maybe cexpr present one-armed conditionals continue into the remainder"
  (@some trailing)
  (maybe.do
    (if true (return branch))
    (return trailing)))

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

(test.assert-error-contains
  "seq.make rejects negative counts"
  "cannot make a negative amount of elements"
  (freeze (seq.make -1 x)))

(test.assert-equal
  "seq.range-step handles both directions and incompatible bounds"
  [[1 3 5] [6 4 2] [] []]
  [(seq.to-list (seq.range-step 2 1 6))
   (seq.to-list (seq.range-step -2 6 1))
   (seq.to-list (seq.range-step 2 6 1))
   (seq.to-list (seq.range-step -2 1 6))])

(test.assert-error-contains
  "seq.range-step rejects a zero step"
  "seq.range-step called with Step=0"
  (freeze (seq.range-step 0 1 6)))

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
  "seq traversal repeats effects without memoization"
  [[1 2] [1 2] 4]
  (let Count (box.make 0)
       Seq (seq.map (/. X (do (box.incr Count) X))
                    (seq.of-list [1 2]))
       First (seq.to-list Seq)
       Second (seq.to-list Seq)
    [First Second (box.unbox Count)]))

(test.assert-equal
  "seq.memo shares successful nodes between traversals"
  [[1 2] [1 2] 2]
  (let Count (box.make 0)
       Seq (seq.memo
             (seq.map (/. X (do (box.incr Count) X))
                      (seq.of-list [1 2])))
       First (seq.to-list Seq)
       Second (seq.to-list Seq)
    [First Second (box.unbox Count)]))

(test.assert-equal
  "seq.memo preserves an already consumed prefix"
  [[1] 1 [1 2 3] 3]
  (let Count (box.make 0)
       Seq (seq.memo
             (seq.map (/. X (do (box.incr Count) X))
                      (seq.of-list [1 2 3])))
       Prefix (seq.to-list (seq.take 1 Seq))
       AfterPrefix (box.unbox Count)
       All (seq.to-list Seq)
    [Prefix AfterPrefix All (box.unbox Count)]))

(test.assert-equal
  "seq.memo preserves its prefix and retries only a failed node"
  [failed 2 [1 2 3] 4 [1 2 3] 4]
  (let Count (box.make 0)
       Failed (box.make false)
       Seq (seq.memo
             (seq.map
               (/. X
                 (do (box.incr Count)
                     (if (and (= X 2) (= false (box.unbox Failed)))
                         (do (box.put Failed true)
                             (simple-error "transient sequence failure"))
                         X)))
               (seq.of-list [1 2 3])))
       First (trap-error (seq.to-list Seq) (/. Error failed))
       AfterFailure (box.unbox Count)
       Second (seq.to-list Seq)
       AfterRetry (box.unbox Count)
       Third (seq.to-list Seq)
    [First AfterFailure Second AfterRetry Third (box.unbox Count)]))

(test.assert-error-contains
  "seq transformation errors are delayed until traversal"
  "delayed sequence failure"
  (let Seq (seq.map (/. X (simple-error "delayed sequence failure"))
                     (seq.singleton 1))
    (freeze (seq.head Seq))))

(test.assert-equal
  "seq.head evaluates only its result node"
  [1 1]
  (let Count (box.make 0)
       Seq (seq.map (/. X (do (box.incr Count) X))
                    (seq.of-list [1 2 3]))
       Head (seq.head Seq)
    [Head (box.unbox Count)]))

(test.assert-error-contains
  "seq.head rejects an empty sequence"
  "seq.head called on empty seq"
  (freeze (seq.head (seq.empty))))

(test.assert-error-contains
  "seq.tail rejects an empty sequence"
  "seq.tail called on empty seq"
  (freeze (seq.tail (seq.empty))))

(test.assert-equal
  "seq.into-vector returns the unconsumed remainder"
  [a b [c] 0]
  (let Vector (vector 2)
       Result (seq.into-vector 1 2 Vector (seq.of-list [a b c]))
    [(<-vector Vector 1)
     (<-vector Vector 2)
     (seq.to-list (fst Result))
     (snd Result)]))

(test.assert-equal
  "seq.into-vector fills downward for a negative count"
  [b a [c] 0]
  (let Vector (vector 3)
       Result (seq.into-vector 3 -2 Vector (seq.of-list [a b c]))
    [(<-vector Vector 2)
     (<-vector Vector 3)
     (seq.to-list (fst Result))
     (snd Result)]))

(test.assert-equal
  "seq.into-vector reports premature source exhaustion"
  [a b [] 1]
  (let Vector (vector 3)
       Result (seq.into-vector 1 3 Vector (seq.of-list [a b]))
    [(<-vector Vector 1)
     (<-vector Vector 2)
     (seq.to-list (fst Result))
     (snd Result)]))

(let Count (box.make 0)
     Source (test.counted-seq [a b] Count)
  (do (test.assert-error-contains
        "seq.into-vector rejects an invalid span"
        "count exceeds vector limits"
        (freeze (seq.into-vector 2 2 (vector 2) Source)))
      (test.assert-equal
        "seq.into-vector validates its span before consuming its source"
        0
        (box.unbox Count))))

(test.assert-equal
  "seq.find stops at its first match"
  [(@some 3) 3]
  (let Count (box.make 0)
       Seq (seq.map (/. X (do (box.incr Count) X))
                    (seq.of-list [1 2 3 4]))
       Found (seq.find (= 3) Seq)
    [Found (box.unbox Count)]))

(test.assert-equal
  "seq.find-map stops at its first @some and reports no match"
  [(@some 30) 3 true]
  (let Count (box.make 0)
       Present
        (seq.find-map
          (/. X (if (> X 2) (@some (* X 10)) (@none)))
          (test.counted-seq [1 2 3 4] Count))
       Absent
        (seq.find-map (/. Ignored (@none)) (seq.of-list [1 2]))
    [Present (box.unbox Count) (maybe.none? Absent)]))

(test.assert-equal
  "seq.equal-cmp? uses its comparator and requires equal lengths"
  [true false false]
  [(seq.equal-cmp? (/. X Y (= (* X 10) Y))
                   (seq.of-list [1 2])
                   (seq.of-list [10 20]))
   (seq.equal-cmp? (/. X Y (= (* X 10) Y))
                   (seq.of-list [1 2])
                   (seq.of-list [10 99]))
   (seq.equal-cmp? (/. X Y (= (* X 10) Y))
                   (seq.of-list [1 2])
                   (seq.of-list [10]))])

(test.assert-equal
  "seq.unzip projections independently traverse their source"
  [[1 2] 2 [a b] 4]
  (let Count (box.make 0)
       Source
        (seq.map
          (/. Pair (do (box.incr Count) Pair))
          (seq.of-list [(@p 1 a) (@p 2 b)]))
       Unzipped (seq.unzip Source)
       Firsts (seq.to-list (fst Unzipped))
       AfterFirsts (box.unbox Count)
       Seconds (seq.to-list (snd Unzipped))
    [Firsts AfterFirsts Seconds (box.unbox Count)]))

(test.assert-equal
  "seq.exists? stops at the first match"
  [true 2]
  (let Count (box.make 0)
       Seq (seq.map (/. X (do (box.incr Count) X))
                    (seq.of-list [1 2 3 4]))
       Found (seq.exists? (= 2) Seq)
    [Found (box.unbox Count)]))

(test.assert-equal
  "seq.take-while evaluates its first rejected value"
  [3 [1 2]]
  (let Count (box.make 0)
       Seq (seq.map (/. X (do (box.incr Count) X))
                    (seq.of-list [1 2 3 4]))
       Values (seq.to-list (seq.take-while (/. X (< X 3)) Seq))
    [(box.unbox Count) Values]))

(test.assert-error-contains
  "seq.take rejects a sequence that ends early"
  "failure to take from sequence that ended abruptly"
  (freeze (seq.to-list (seq.take 3 (seq.of-list [1 2])))))

(test.assert-equal
  "seq.truncate permits a sequence that ends early"
  [1 2]
  (seq.to-list (seq.truncate 3 (seq.of-list [1 2]))))

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

(test.assert-error-contains
  "seq.take rejects negative counts"
  "cannot take a negative amount from a seq"
  (freeze (seq.take -1 (seq.empty))))

(test.assert-error-contains
  "seq.truncate rejects negative counts"
  "cannot truncate a negative amount from a seq"
  (freeze (seq.truncate -1 (seq.empty))))

(test.assert-error-contains
  "seq.drop rejects negative counts"
  "cannot drop a negative amount from a seq"
  (freeze (seq.drop -1 (seq.empty))))

(test.assert-error-contains
  "seq.chunks rejects sizes below one"
  "cannot produce seq chunks of size < 1"
  (freeze (seq.chunks 0 (seq.empty))))

(test.assert-equal
  "seq.chunks handles empty, exact, and partial groups"
  [[] [[1 2] [3 4]] [[1 2] [3]]]
  (let ChunkLists
        (/. Seq
          (seq.to-list
            (seq.map
              (/. Vector (seq.to-list (seq.of-vector Vector)))
              Seq)))
    [(ChunkLists (seq.chunks 2 (seq.empty)))
     (ChunkLists (seq.chunks 2 (seq.of-list [1 2 3 4])))
     (ChunkLists (seq.chunks 2 (seq.of-list [1 2 3])))]))
