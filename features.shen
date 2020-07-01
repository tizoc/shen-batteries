\\ Copyright (c) 2020 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(datatype features.t-internal
  Name : symbol;
  _____________________
  (shen.x.features.add Name) : (list symbol);

  _____________________
  (shen.x.features.current) : (list symbol);)

(defmacro features.macro
  [features.cond | Rest] -> [shen.x.features.cond-expand | Rest])

(define features.add
  { symbol --> (list symbol) }
  Name -> (shen.x.features.add Name))

(define features.current
  { --> (list symbol) }
  -> (shen.x.features.current))

(preclude [features.t-internal])
