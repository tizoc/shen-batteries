(package defpattern-fixture [defpattern]

(defpattern pair-handler
  Self Is? Assign [paired P]
    -> (do (Is? [tuple? Self])
           (Assign P Self)
           handled))

(datatype pair-pattern
  P : A;
  =================================================
  (@p shen.custom-pattern (paired P)) : A;)

(define match
  { A --> A }
  [paired P] -> P
  _ -> (error "not a pair"))

)
