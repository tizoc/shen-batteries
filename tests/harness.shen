(set test.*assertions* 0)

(define test.assert-equal
  Label Expected Expected
    -> (do (set test.*assertions* (+ 1 (value test.*assertions*)))
           (output "[OK] ~A~%" Label))
  Label Expected Actual
    -> (error "~A: expected ~R, got ~R" Label Expected Actual))

(define test.assert-true
  Label Value -> (test.assert-equal Label true Value))

(define test.finish
  -> (output "~%~A assertions passed.~%" (value test.*assertions*)))
