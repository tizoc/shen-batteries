\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\** {1 [typ/verified-objects.t]}

    Adds [verified] rules for the [number?], [string?], [symbol?] and [boolean?] predicate,
    and also for [and] expressions ([(and X Y)] adds [verified] for the [X] and [Y] expressions
    if there is a rule defined for them).
*\

(datatype typ/verified-objects.t
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
