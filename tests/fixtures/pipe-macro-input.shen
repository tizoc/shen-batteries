(package test-pipe-package [=> =>> doto box.make box.incr box.unbox]

(define scaled
  { number --> number }
  X -> (=> X (+ 3) (* 4)))

(define doubled-reversed
  { (list number) --> (list number) }
  Xs -> (=>> Xs (map (/. X (* X 2))) reverse))

(define bumped
  { --> number }
  -> (let Box (doto (box.make 1) (box.incr))
       (box.unbox Box)))

)

(define test.pipe-record-step
  { (box.t number) --> (box.t (list symbol)) --> symbol --> symbol }
  Target Seen Label
    -> (do (box.put Seen (append (box.unbox Seen) [Label]))
           (box.incr Target)
           ignored))
