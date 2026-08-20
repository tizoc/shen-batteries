\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = `typ/or.t`
\\:
\\: This datatype adds introduction rules for the union type `(or A B)`. An
\\: expression that typechecks as either `A` or `B` can be used where
\\: `(or A B)` is required.
\\:
\\: [source,shen]
\\: ----
\\: (library.use [typ/or])
\\:
\\: (define number-or-name
\\:   { boolean --> (or number string) }
\\:   true -> 1
\\:   false -> "one")
\\: ----
\\:
\\: `(or A B)` is an untagged, compile-time type only: these rules neither wrap
\\: values nor change their run-time representation. The datatype provides no
\\: elimination or refinement rule, so it cannot by itself determine which
\\: alternative a value inhabits when typechecking code that consumes the union.
\\: More than two alternatives can be expressed by nesting unions, for example
\\: `(or number (or string symbol))`.

(datatype typ/or.t
  X : A;
  ______________________
  X : (- (or A B));

  X : B;
  ______________________
  X : (- (or A B));)
