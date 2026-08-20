(define test.queue-append-one
  { (list number) --> (list number) }
  Values -> (queue.to-list (queue.snoc (queue.of-list Values) 4)))

(define test.queue-pop
  { (queue.t number) --> (maybe.t (number * (queue.t number))) }
  Queue -> (queue.uncons Queue))

(define test.result-number
  { (result.t number number) --> number }
  (@ok Value) -> (+ Value 1)
  (@err Error) -> (- 0 Error))
