\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\** {1 [typ/verified-and-head.t]}

    This rule extends the typechecker so that then the [Tail] expression of [(and Head Tail)] expressions
    is typechecked, any [verified] rules that result from [Head] are are taken into account.

    Example:

  [(define test
    { A --> boolean }
    X -> (and (number? X) (> X 0)))]

    The above code doesn't typecheck by default, but if [typ/verified-and-head.t] and [typ/verified-objects.t]
    are enabled it does.
*\

(datatype typ/verified-and-head.t
  Q : boolean;
  Q : verified >> R : boolean;
  ______________________
  (and Q R) : boolean;)
