(load "tests/harness.shen")
(load "library.shen")

(define test.checked-load-fails?
  Path -> (trap-error
            (do (load Path) false)
            (/. Error true)))

(library.use [typ/verified-objects])

(tc +)

(test.assert-true
  "verified true branches require typ/verified-if"
  (test.checked-load-fails? "tests/fixtures/typ/verified-if-required.shen"))

(library.use [typ/verified-if])
(load "tests/verified-if.shen")
(tc -)

(define test.type-error?
  Source -> (trap-error
              (do (shen.check-eval-and-print (read-from-string Source))
                  false)
              (/. Error true)))

(test.assert-equal
  "plain if typechecks and evaluates"
  [1 0]
  [(test.plain-if true) (test.plain-if false)])

(test.assert-equal
  "verified if narrows only the true branch"
  [4 0]
  [(test.verified-if 2) (test.verified-if "not-a-number")])

(test.assert-equal
  "compound tests provide all verified hypotheses"
  [5 0 0]
  [(test.compound-verified-if 2 3)
   (test.compound-verified-if 2 "not-a-number")
   (test.compound-verified-if "not-a-number" 3)])

(test.assert-true
  "verified hypotheses do not escape into the false branch"
  (test.type-error?
    "(define test.invalid-false-branch { A --> number } X -> (if (number? X) 0 (+ X 2)))"))

(test.assert-true
  "unrelated boolean tests do not refine values"
  (test.type-error?
    "(define test.invalid-unrelated-test { A --> number } X -> (if (= X X) (+ X 2) 0))"))

(test.assert-equal
  "if remains an ordinary core function"
  false
  (or (shen.special? if) (shen.extraspecial? if)))

(test.assert-equal
  "if retains ordinary curried application representation"
  [[[if true] 1] 0]
  (shen.curry [if true 1 0]))

(test.assert-true
  "if keeps its core signature"
  (cons? (assoc if (value shen.*sigf*))))

(test.finish)
