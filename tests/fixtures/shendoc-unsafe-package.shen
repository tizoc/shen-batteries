\\ The external declaration must be inspected as source, never evaluated.
(package shendoc-unsafe
  (do (set test.*shendoc-package-probe* true) [public])

(define public
  -> ok)

)
