(package defpattern-fixture [defpattern]

(defpattern pair-handler
  Self Is? Assign [paired P]
    -> (do (Is? [tuple? Self])
           (Assign P Self)
           handled))

(datatype pair-pattern
  P : A;
  =================================================
  (paired P) : A;)

)
