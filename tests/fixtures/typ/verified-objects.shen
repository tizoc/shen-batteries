(define test.typ-verified-number
  { A --> number }
  X -> (+ X 1) where (number? X)
  _ -> 0)

(define test.typ-verified-string
  { A --> string }
  X -> (cn X "!") where (string? X)
  _ -> "")

(define test.typ-verified-symbol
  { A --> number }
  X -> (arity X) where (symbol? X)
  _ -> -1)

(define test.typ-verified-boolean
  { A --> boolean }
  X -> (not X) where (boolean? X)
  _ -> false)

(define test.typ-verified-conjunction
  { A --> B --> (number * string) }
  X Y -> (@p (+ X 1) (cn Y "!"))
    where (and (number? X) (string? Y))
  _ _ -> (@p 0 ""))
