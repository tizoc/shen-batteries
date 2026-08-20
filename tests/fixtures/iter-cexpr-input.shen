(define test.iter-do-checked
  { (list number) --> (list number) }
  Values -> (iter.to-list
              (iter.do
                (for X Values)
                (bind Y (iter.of-list [10 20]))
                (return (+ X Y)))))

(define test.iter-do-vector-checked
  { (vector number) --> (list number) }
  Values -> (iter.to-list
              (iter.do
                (bind X (iter.of-vector Values))
                (return (* X 10)))))
