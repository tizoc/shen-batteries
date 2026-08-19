(package batteries-native-test
 [maybe.t @some maybe.do
  iter.of-list iter.take iter.to-list
  seq.do seq.of-list seq.to-list for yield]

(define unwrap
  { (maybe.t A) --> A }
  (@some X) -> X)

(define answer
  { --> number }
  -> (unwrap (@some 42)))

(define iter-answer
  { --> (list number) }
  -> (iter.to-list
       (iter.take 2 (iter.of-list [1 2 3]))))

(define maybe-do-answer
  { --> number }
  -> (unwrap
       (maybe.do
         (bind X (@some 41))
         (return (+ X 1)))))

(define seq-do-answer
  { --> (list number) }
  -> (seq.to-list
       (seq.do
         (for X [1 2])
         (bind Y (seq.of-list [10 20]))
         (yield (+ X Y)))))

)
