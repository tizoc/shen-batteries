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
