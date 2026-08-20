(load "tests/harness.shen")
(load "library.shen")

(define typ-conformance.load-fails?
  Path -> (trap-error
            (do (load Path) false)
            (/. Error true)))

(tc +)

(test.assert-true
  "union introductions require typ/or"
  (typ-conformance.load-fails? "tests/fixtures/typ/or.shen"))

(library.use [typ/or])
(load "tests/fixtures/typ/or.shen")

(test.assert-true
  "typ/or rejects values outside either alternative"
  (typ-conformance.load-fails? "tests/fixtures/typ/or-invalid.shen"))

(test.assert-true
  "S-expression introductions require typ/sexp"
  (typ-conformance.load-fails? "tests/fixtures/typ/sexp.shen"))

(library.use [typ/sexp])
(load "tests/fixtures/typ/sexp.shen")

(test.assert-true
  "typ/sexp rejects vectors"
  (typ-conformance.load-fails? "tests/fixtures/typ/sexp-invalid.shen"))

(test.assert-true
  "typ/sexp rejects improper lists"
  (typ-conformance.load-fails? "tests/fixtures/typ/sexp-improper-invalid.shen"))

(test.assert-true
  "discarded results require typ/void"
  (typ-conformance.load-fails? "tests/fixtures/typ/void.shen"))

(test.assert-true
  "kernel unit is not a discarded-result type"
  (typ-conformance.load-fails? "tests/fixtures/typ/unit-invalid.shen"))

(library.use [typ/void])
(load "tests/fixtures/typ/void.shen")

(test.assert-true
  "predicate refinements require typ/verified-objects"
  (typ-conformance.load-fails? "tests/fixtures/typ/verified-objects.shen"))

(library.use [typ/verified-objects])
(load "tests/fixtures/typ/verified-objects.shen")

(test.assert-true
  "and tail refinement requires typ/verified-and-head"
  (typ-conformance.load-fails? "tests/fixtures/typ/verified-and-head.shen"))

(library.use [typ/verified-and-head])
(load "tests/fixtures/typ/verified-and-head.shen")

(test.assert-true
  "and refinement follows evaluation order"
  (typ-conformance.load-fails? "tests/fixtures/typ/verified-and-head-invalid.shen"))

(tc -)

(test.assert-equal
  "typ/or accepts either alternative and nested unions"
  [1 "one" three]
  [(test.typ-or-select true)
   (test.typ-or-select false)
   (test.typ-or-nested)])

(test.assert-equal
  "typ/sexp accepts atoms and heterogeneous proper lists"
  [1 tag [form 1 "two" true []]]
  [(test.typ-sexp-number)
   (test.typ-sexp-symbol)
   (test.typ-sexp-call-form form [1 "two" true []])])

(test.assert-equal
  "void denotes a discarded result rather than a singleton value"
  7
  (test.typ-void-number 6))

(test.assert-equal
  "verified predicates refine all four primitive object types"
  [3 "x!" 2 false]
  [(test.typ-verified-number 2)
   (test.typ-verified-string "x")
   (test.typ-verified-symbol +)
   (test.typ-verified-boolean true)])

(test.assert-equal
  "verified conjunctions expose both successful refinements"
  (@p 3 "3!")
  (test.typ-verified-conjunction 2 "3"))

(test.assert-equal
  "and checks its tail under a verified head"
  [true false false]
  [(test.typ-verified-and 2)
   (test.typ-verified-and -1)
   (test.typ-verified-and not-a-number)])

(test.finish)
