(define test.typ-or-select
  { boolean --> (or number string) }
  true -> 1
  false -> "one")

(define test.typ-or-nested
  { --> (or number (or string symbol)) }
  -> three)
