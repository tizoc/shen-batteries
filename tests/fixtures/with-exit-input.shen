(package test-with-exit [with-return with-break void]

(define test-with-exit.return
  { --> number }
  -> (with-return Return (Return 7)))

(define test-with-exit.break
  { --> void }
  -> (with-break Break (Break)))

)
