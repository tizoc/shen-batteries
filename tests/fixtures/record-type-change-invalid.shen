(define test.record-type-changing-with
  { (test-record-box number) --> (test-record-box string) }
  Box -> (test-record-box.with Box value <- "changed";))
