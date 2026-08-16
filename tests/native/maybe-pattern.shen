(package batteries-native-test [maybe.t @some]

(define unwrap
  { (maybe.t A) --> A }
  (@some X) -> X)

(define answer
  { --> number }
  -> (unwrap (@some 42)))

)
