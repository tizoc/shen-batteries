(define test.record-location
  { test-record-person --> (string * number) }
  (test-record-person.match
    Person
    address (test-record-address.match Address city City))
    -> (@p City (test-record-person.age Person)))

(define test.record-named-adult?
  { test-record-person --> boolean }
  (test-record-person.match Person age Age name "Ada") -> (>= Age 18)
  _ -> false)

(define test.record-box-with-value
  { (test-record-box A) --> A --> (test-record-box A) }
  Box Value -> (test-record-box.with Box value <- Value;))

(define test.record-box-pair
  { (test-record-box A) --> (A * symbol) }
  (test-record-box.match Box label Label value Value) -> (@p Value Label))

(define test.record-marker-value
  { test-record-marker --> symbol }
  (test-record-marker.match Marker) -> marker)

(define test.record-make-phantom-number
  { symbol --> (test-record-phantom number) }
  Label -> (test-record-phantom.make label <- Label;))

(define test.record-phantom-label
  { (test-record-phantom A) --> symbol }
  Phantom -> (test-record-phantom.label Phantom))

(package test-record-client
 [test-record-model.person
  test-record-model.person.make
  test-record-model.person.match
  test-record-model.person.with
  test-record-model.person?
  test-record-model.person.name
  test-record-model.person.age]

(define make-person
  { string --> number --> test-record-model.person }
  Name Age -> (test-record-model.person.make age <- Age; name <- Name;))

(define birthday
  { test-record-model.person --> test-record-model.person }
  Person -> (test-record-model.person.with
              Person
              age <- (+ (test-record-model.person.age Person) 1);))

(define name-and-age
  { test-record-model.person --> (string * number) }
  (test-record-model.person.match Person age Age name Name) -> (@p Name Age))

(define person?
  { A --> boolean }
  X -> (test-record-model.person? X))

)
