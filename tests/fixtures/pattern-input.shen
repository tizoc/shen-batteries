(define test.match-maybe
  { (maybe.t A) --> A }
  (@some X) -> X
  (@none) -> (error "not a some value"))

(define test.match-nullable
  { (nullable.t A) --> A }
  (@just X) -> X
  (@null) -> (error "null has no value"))

(define test.match-lazy
  { (lazy A) --> A }
  (freeze X) -> X)
