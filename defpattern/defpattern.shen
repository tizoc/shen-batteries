\\ Copyright (c) 2020 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package defpattern [defpattern sexp]

(datatype t
  F : symbol;
  ___________________________________________________________________
  (shen.x.programmable-pattern-matching.register-handler F) : symbol;)

(defmacro defpattern-macro
  [defpattern Name | Body] ->
    (let Definition
      (append [define Name
                { sexp --> [sexp --> (protect A)]
                       --> [sexp --> sexp --> (protect B)]
                       --> sexp
                       --> symbol }]
              Body
              [_ _ _ _ -> [fail]])
      [package null []
        Definition
        [shen.x.programmable-pattern-matching.register-handler Name]]))

)
