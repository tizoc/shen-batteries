\\ Copyright (c) 2020 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package defpattern [defpattern sexp void]

(defmacro defpattern-macro
  [defpattern Name | Body] ->
    (let Definition
      (append [define Name { sexp --> [sexp --> void] --> [sexp --> sexp --> void] --> sexp --> void }]
              Body
              [_ _ _ _ -> [fail]])
      [package null []
        Definition
        [shen.x.programmable-pattern-matching.register-handler Name]]))

(define undef
  { symbol --> symbol }
  Name -> (shen.x.programmable-pattern-matching.unregister-handler Name))

)
