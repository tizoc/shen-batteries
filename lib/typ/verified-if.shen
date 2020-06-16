\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = `typ/verified-if.t`
\\:
\\:  This rule extends the typechecker so that when the `True` branch of `(if Test True False)` expressions
\\:  is typechecked, any `verified` rules that result from `Test` are taken into account.
\\:
\\:  Example: `(if (number? X) (+ X 2) 0)` for an `X` of unkown type would not typecheck
\\:  without this rule, but `X -> (+ X 2) where (number? X)` would.

(specialise if)
(destroy if)

(datatype typ/verified-if.t
  !;
  T : boolean;
  T : verified >> True : A;
  False : A;
  ______________________
  (if T True False) : A;)
