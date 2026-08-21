(test.assert-equal
  "portable record predicates require the exact tagged-vector shape"
  [true false false]
  (let Address (test-record-address.make city <- "Melo"; postal <- 37000;)
       Person (test-record-person.make
                name <- "Ada"; age <- 36; address <- Address;)
       Printer (<-address Person 0)
       Schema (<-address Person 1)
       Too-short (address->
                   (address-> (absvector 2) 0 Printer) 1 Schema)
       Too-long (address->
                  (address-> (absvector 6) 0 Printer) 1 Schema)
    [(test-record-person? Person)
     (test-record-person? Too-short)
     (test-record-person? Too-long)]))

(test.assert-equal
  "portable records preserve structural equality and named printing"
  [true "(test-record-address.make city <- c#34;Meloc#34;; postal <- 37000;)"]
  (let First (test-record-address.make city <- "Melo"; postal <- 37000;)
       Second (test-record-address.make postal <- 37000; city <- "Melo";)
    [(= First Second) (make-string "~S" First)]))

(test.assert-equal
  "portable with-return completes normally"
  9
  (with-return Return (+ 4 5)))

(test.assert-equal
  "portable with-return exits early"
  7
  (with-return Return
    (do (Return 7)
        8)))

(test.assert-equal
  "portable with-return typechecks"
  7
  (test-with-exit.return))

(test.assert-equal
  "portable with-break typechecks"
  ok
  (do (test-with-exit.break) ok))

(test.assert-equal
  "portable with-break completes with void"
  (void)
  (with-break Break 9))

(test.assert-equal
  "portable with-break exits early"
  1
  (let Count (box.make 0)
    (do (with-break Break
          (do (box.incr Count)
              (Break)
              (box.incr Count)))
        (box.unbox Count))))

(test.assert-equal
  "portable nested with-break respects shadowing"
  1
  (test-with-exit.nested-break))

(test.assert-equal
  "portable nested exit forms keep separate binders"
  1
  (test-with-exit.mixed-shadowing))

(test.assert-equal
  "portable exits propagate unrelated errors"
  "portable-unrelated-error"
  (trap-error
    (with-return Return
      (simple-error "portable-unrelated-error"))
    (/. Error (error-to-string Error))))

(test.assert-equal
  "iter stops its source through portable with-break"
  [2 [1 2]]
  (let Count (box.make 0)
       Iter (/. Yield
              (do (box.incr Count)
                  (Yield 1)
                  (box.incr Count)
                  (Yield 2)
                  (box.incr Count)
                  (Yield 3)))
       Values (iter.to-list (iter.take 2 Iter))
    [(box.unbox Count) Values]))

(test.assert-equal
  "iter searches through portable with-return"
  (@some 2)
  (iter.find (= 2) (iter.of-list [1 2 3])))

(test.assert-equal
  "portable early exit leaves iter.persistent-lazy uncached"
  [(@some 1) 1 [1 2 3] 4 [1 2 3] 4]
  (let Count (box.make 0)
       Source (/. Yield
                (do (box.incr Count)
                    (Yield 1)
                    (box.incr Count)
                    (Yield 2)
                    (box.incr Count)
                    (Yield 3)))
       Iter (iter.persistent-lazy Source)
       Head (iter.head Iter)
       AfterHead (box.unbox Count)
       Complete (iter.to-list Iter)
       AfterComplete (box.unbox Count)
       Replay (iter.to-list Iter)
    [Head AfterHead Complete AfterComplete Replay (box.unbox Count)]))
