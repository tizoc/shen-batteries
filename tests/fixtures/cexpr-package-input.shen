(package test-cexpr-package [seq.t seq.do for yield]

(define generate
  { --> (seq.t number) }
  -> (seq.do
       (for X [1 2])
       (yield (* X 10))))

)

(define test.cexpr-applicative4
  { number --> number --> number --> number --> number }
  W X Y Z -> (test.cexpr-applicative.do
               (and (bind A W)
                    (bind B X)
                    (bind C Y)
                    (bind D Z))
               (return (+ A (+ B (+ C D))))))

(define test.cexpr-packaged-builder
  { --> number }
  -> (test-cexpr-builder.do (yield 42)))
