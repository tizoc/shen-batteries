(package test-with-exit [with-return with-break void]

(define test-with-exit.return
  { --> number }
  -> (with-return Return (Return 7)))

(define test-with-exit.break
  { --> void }
  -> (with-break Break (Break)))

(define test-with-exit.nested-break
  { --> number }
  -> (with-return Return
       (do (with-break Exit
             (do (with-break Exit (Exit))
                 (Return 1)))
           (Return 2))))

(define test-with-exit.mixed-shadowing
  { --> number }
  -> (with-return Return
       (do (with-break Exit
             (do (with-return Exit (Exit 7))
                 (Return 1)))
           (Return 2))))

)
