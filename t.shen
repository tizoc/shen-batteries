\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(datatype t.unit
  X : A;
  __________
  X : (mode unit -);)

(synonyms unit.t unit)

(datatype t.any
  __________
  X : (mode any.t -);)

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

(datatype t.or
  X : A;
  ______________________
  X : (mode (or A B) -);

  X : B;
  ______________________
  X : (mode (or A B) -);)