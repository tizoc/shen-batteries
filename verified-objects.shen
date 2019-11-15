\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package verified-objects []

(specialise if)
(destroy if)

(datatype verified-if
  !;
  T : boolean;
  T : verified >> True : A;
  False : A;
  ______________________
  (if T True False) : A;)

(datatype verified-objects
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

)