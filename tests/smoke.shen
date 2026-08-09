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
  "seq cexpr declares its runtime dependency"
  [seq typ/sexp cexpr]
  (library.module-requires (library.read-module seq/cexpr)))
