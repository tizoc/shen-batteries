(package defpattern-guide []

(define first-or
  { A --> (symbol * (list A)) --> A }
  _ (tagged [Head | _]) -> Head
  Default _ -> Default)

)
