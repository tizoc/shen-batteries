\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\** {1 Core type rules}

    A few basic type rules that are useful for most programs.

    {2 Types}

    {3 [t.unit] }

    [unit] (alias of [unit]) is a type inhabited by every value. It is useful
    when writting functions where the type of the inputs is not known, or
    when the result is discarded. Use carefully.

    {3 [t.verified-objects]}

    Adds [verified] rules for the [number?], [string?], [symbol?] and [boolean?] predicate,
    and also for [and] expressions ([(and X Y)] adds [verified] for the [X] and [Y] expressions
    if there is a rule defined for them).

    {3 [t.verified-if]}

    This rule extends the typechecker so that when the [True] branch of [(if Test True False)] expressions
    is typechecked, any [verified] rules that result from [Test] are taken into account.

    Example: [(if (number? X) (+ X 2) 0)] for an [X] of unkown type would not typecheck
    without this rule, but [X -> (+ X 2) where (number? X)] would.

    {3 [t.verified-and-head]}

    This rule extends the typechecked so that then the [Tail] expression of [(and Head Tail)] expressions
    is typechecked, any [verified] rules that result from [Head] are are taken into account.

    Example:

  [(define test
    { A --> boolean }
    X -> (and (number? X) (> X 0)))]

    The above code doesn't typecheck by default, but if [t.verified-and-head] and [t.verified-objects]
    are enabled it does.

    {3 [t.or]}

    A value inhabits the type [(or t1 t2)] if it inhabits [t1] or [t2].

*\

(datatype t.unit
  __________
  X : (mode unit -);)

(specialise if)
(destroy if)

(datatype t.verified-if
  !;
  T : boolean;
  T : verified >> True : A;
  False : A;
  ______________________
  (if T True False) : A;)

(datatype t.verified-objects
  ______________________
  (number? X) : verified >> X : number;

  ______________________
  (string? X) : verified >> X : string;

  ______________________
  (symbol? X) : verified >> X : symbol;

  ______________________
  (boolean? X) : verified >> X : boolean;

  Q : verified, R : verified >> P;
  ______________________
  (and Q R) : verified >> P;)

(datatype t.verified-and-head
  Q : boolean;
  Q : verified >> R : boolean;
  ______________________
  (and Q R) : boolean;)

(datatype t.or
  X : A;
  ______________________
  X : (mode (or A B) -);

  X : B;
  ______________________
  X : (mode (or A B) -);)
