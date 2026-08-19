(tc +)

(define test.verified-if
  { A --> number }
  X -> (if (number? X) (+ X 2) 0))

(tc -)

(test.assert-true
  "if is specialised"
  (shen.special? if))

(test.assert-true
  "if keeps its core signature"
  (cons? (assoc if (value shen.*sigf*))))

(test.assert-equal
  "verified if narrows the true branch"
  4
  (test.verified-if 2))

(test.assert-equal
  "verified if preserves the false branch"
  0
  (test.verified-if "not-a-number"))
