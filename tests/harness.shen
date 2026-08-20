(set test.*assertions* 0)

(define test.assert-equal
  Label Expected Expected
    -> (do (set test.*assertions* (+ 1 (value test.*assertions*)))
           (output "[OK] ~A~%" Label))
  Label Expected Actual
    -> (error "~A: expected ~R, got ~R" Label Expected Actual))

(define test.assert-true
  Label Value -> (test.assert-equal Label true Value))

(define test.string-prefix?
  "" _ -> true
  (@s C Needle) (@s C Haystack) -> (test.string-prefix? Needle Haystack)
  _ _ -> false)

(define test.string-contains?
  Needle Haystack -> true where (test.string-prefix? Needle Haystack)
  _ "" -> false
  Needle (@s _ Haystack) -> (test.string-contains? Needle Haystack))

(define test.assert-error-contains
  Label Expected Thunk
    -> (test.assert-error-result
         Label
         Expected
         (trap-error [returned (thaw Thunk)]
                     (/. Error [raised (error-to-string Error)]))))

(define test.assert-error-result
  Label Expected [raised Message]
    -> (if (test.string-contains? Expected Message)
           (do (set test.*assertions* (+ 1 (value test.*assertions*)))
               (output "[OK] ~A~%" Label))
           (error "~A: expected error containing ~R, got ~R"
                  Label Expected Message))
  Label Expected [returned Value]
    -> (error "~A: expected error containing ~R, got value ~R"
              Label Expected Value))

(define test.finish
  -> (output "~%~A assertions passed.~%" (value test.*assertions*)))
