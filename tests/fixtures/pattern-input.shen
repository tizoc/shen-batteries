(define test.match-maybe
  { (maybe.t A) --> A }
  (@some X) -> X
  (@none) -> (error "not a some value"))

(define test.classify-maybe-pattern
  { (or (maybe.t A) B) --> symbol }
  (@some X) -> present
  (@none) -> absent
  _ -> ordinary)

(define test.match-nullable
  { (nullable.t A) --> A }
  (@just X) -> X
  (@null) -> (error "null has no value"))

(define test.classify-nullable-pattern
  { (nullable.t A) --> symbol }
  (@null) -> absent
  (@just X) -> present)

(define test.match-lazy
  { (lazy A) --> A }
  (freeze X) -> X)
