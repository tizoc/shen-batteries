\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: == `void`
\\:
\\: `(void)` returns the void object. The void object represents an unspecified value, and
\\: is useful as a result of side-effectful functions without a meaningful return value.
\\:
\\: Should not be used for anything else than to express that a return value should be ignored
\\: and discarded.

(datatype typ/void.internal-t
  ____________________
  X : (mode void -);)

(datatype typ/void.t
  X : A;
  ____________________
  X : (mode void -);)

(define void
  { --> void}
  -> (shen.x.features.cond-expand
       shen/scheme (scm.void)
       true -void-))

(preclude [typ/void.internal-t])