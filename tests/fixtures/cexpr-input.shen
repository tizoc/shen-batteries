(define test.cexpr-trace-builder
  { (list sexp) --> sexp }
  [] -> zero
  [bind X F] -> [cons bound [cons X [cons [F X] []]]]
  [for X F] -> [cons for [cons X [cons [F X] []]]]
  [return X] -> [cons returned [cons X []]]
  [return-from X] -> [cons returned-from [cons X []]]
  [yield X] -> [cons yielded [cons X []]]
  [yield-from X] -> [cons yielded-from [cons X []]]
  [combine X Y] -> [cons combined [cons X [cons Y []]]]
  [bind-return X F] -> [cons bind-return [cons X [cons [F X] []]]]
  [merge-sources X Y] -> [cons merged [cons X [cons Y []]]]
  [merge-sources X Y | More]
    -> [cons merged
         [cons X
          [cons (test.cexpr-trace-builder [merge-sources Y | More]) []]]]
  [delay X] -> [cons delayed [cons X []]]
  [run X] -> [cons root-run [cons X []]]
  Other -> (cexpr.default-builder cexpr-trace Other))

(defcexpr test.cexpr-trace.do test.cexpr-trace-builder)

(define test.cexpr-monadic-builder
  { (list sexp) --> sexp }
  [bind X F] -> [F X]
  [return X] -> X
  [merge-sources X Y] -> [@p X Y]
  Other -> (cexpr.default-builder cexpr-monadic Other))

(defcexpr test.cexpr-monadic.do test.cexpr-monadic-builder)

(define test.cexpr-broken-builder
  { (list sexp) --> sexp }
  [bind-return _ _] -> (error "broken bind-return optimizer")
  [bind X F] -> [F X]
  [return X] -> X
  [merge-sources X Y] -> [@p X Y]
  Other -> (cexpr.default-builder cexpr-broken Other))

(define test.cexpr-applicative-builder
  { (list sexp) --> sexp }
  [bind-return X F] -> [F X]
  [merge-sources X Y] -> [@p X Y]
  [merge-sources X Y | More]
    -> [@p X (test.cexpr-applicative-builder [merge-sources Y | More])]
  Other -> (cexpr.default-builder cexpr-applicative Other))

(defcexpr test.cexpr-applicative.do test.cexpr-applicative-builder)

(define test.cexpr-maybe-map
  { (maybe.t number) --> (maybe.t number) }
  M -> (maybe.do
         (bind X M)
         (return (+ X 1))))

(define test.maybe-positive
  { number --> (maybe.t number) }
  X -> (@some X) where (> X 0)
  _ -> (@none))

(define test.cexpr-maybe-pipeline
  { number --> (maybe.t number) }
  N -> (maybe.do
         (bind X (test.maybe-positive N))
         (let Y (+ X 3))
         (return (* Y 2))))

(define test.cexpr-seq-map
  { (seq.t number) --> (seq.t number) }
  S -> (seq.do
         (bind X S)
         (return (+ X 1))))

(define test.cexpr-seq-let
  { (seq.t number) --> (seq.t number) }
  S -> (seq.do
         (bind X S)
         (let Y (+ X 1))
         (return (* Y 2))))

(package test-cexpr-builder [defcexpr cexpr.default-builder yield sexp]

(define builder
  { (list sexp) --> sexp }
  [yield X] -> X
  Other -> (cexpr.default-builder test-cexpr-builder Other))

(defcexpr test-cexpr-builder.do test-cexpr-builder.builder)

)
