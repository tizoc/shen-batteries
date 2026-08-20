(define test.typ-verified-if-required
  { A --> number }
  X -> (if (number? X) (+ X 1) 0))
