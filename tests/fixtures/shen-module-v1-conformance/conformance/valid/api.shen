(define module-conformance-valid.answer
  { number --> number }
  X -> (module-conformance-valid.twice
         (module-conformance-shared.increment X)))
