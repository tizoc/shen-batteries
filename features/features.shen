\\ Copyright (c) 2020 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(defmacro features.macro
  [features.cond | Rest] -> [shen.x.features.cond-expand | Rest])

(define features.add
  { symbol --> (list symbol) }
  Name -> (shen.x.features.add Name))

(define features.current
  { --> (list symbol) }
  -> (shen.x.features.current))
