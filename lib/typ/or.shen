\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\** {1 [typ/or.t]}

    A value inhabits the type [(or t1 t2)] if it inhabits [t1] or [t2].
*\

(datatype typ/or.t
  X : A;
  ______________________
  X : (mode (or A B) -);

  X : B;
  ______________________
  X : (mode (or A B) -);)
