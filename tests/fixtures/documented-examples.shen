(define test.documented-positive-number?
  { A --> boolean }
  X -> (and (number? X) (> X 0)))

(define test.documented-below-ten
  { number --> (maybe.t number) }
  X -> (@some X) where (< X 10)
  _ -> (@none))

(define test.documented-maybe-pipeline
  { number --> (maybe.t number) }
  N -> (maybe.do
         (bind X (test.documented-below-ten N))
         (bind Y (test.documented-below-ten (+ X 3)))
         (return (* Y 2))))

(define test.documented-feature-selection
  { --> (list symbol) }
  -> [(features.cond
        (or shen-batteries/documented-examples/intentionally-unavailable-7f31
            shen-batteries/documented-examples) or-selected
        true fallback-selected)
      (features.cond
        (and shen-batteries/documented-examples
             (not shen-batteries/documented-examples/intentionally-unavailable-7f31))
          and-selected
        true fallback-selected)])
