(package defpattern-fixture [defpattern]

(defpattern two-handler
  Self Is? Assign [two A B]
    -> (do (Is? [tuple? Self])
           (Assign A [fst Self])
           (Assign B [snd Self])))

(define match
  { (A * B) --> (A * B) }
  [two A B] -> (@p A B)
  _ -> no)

)
