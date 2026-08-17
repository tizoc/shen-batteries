(define test.cexpr-trace-builder
  { (list sexp) --> sexp }
  [] -> empty
  [yield X] -> [cons yielded [cons X []]]
  [combine X Y] -> [cons combined [cons X [cons Y []]]]
  [delay X] -> [cons delayed [cons X []]]
  [run X] -> [cons root-run [cons X []]]
  Other -> (cexpr.default-builder cexpr-trace Other))

(cexpr.register cexpr-trace test.cexpr-trace-builder)
