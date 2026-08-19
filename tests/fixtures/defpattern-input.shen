(package defpattern-guide [defpattern]

(define tagged
  { A --> (symbol * A) }
  Value -> (@p tagged Value))

(datatype tagged-pattern
  Pattern : A;
  =================================================
  (tagged Pattern) : (symbol * A);)

(defpattern tagged-pattern-handler
  Self Is? Assign [tagged Pattern]
    -> (do (Is? [and [tuple? Self]
                        [= [fst Self] tagged]])
           (Assign Pattern [snd Self])
           handled))

)
