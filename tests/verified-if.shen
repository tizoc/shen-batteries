(define test.plain-if
  { boolean --> number }
  Test -> (if Test 1 0))

(define test.verified-if
  { A --> number }
  X -> (if (number? X) (+ X 2) 0))

(define test.compound-verified-if
  { A --> B --> number }
  X Y -> (if (and (number? X) (number? Y))
             (+ X Y)
             0))
