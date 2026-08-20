(define test.typ-sexp-number
  { --> sexp }
  -> 1)

(define test.typ-sexp-symbol
  { --> sexp }
  -> tag)

(define test.typ-sexp-call-form
  { symbol --> (list sexp) --> sexp }
  Function Arguments -> [Function | Arguments])
