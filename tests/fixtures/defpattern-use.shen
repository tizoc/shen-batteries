(package defpattern-fixture []

(define match
  { A --> A }
  (paired P) -> P
  _ -> (error "not a pair"))

)
