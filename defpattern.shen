\\ Copyright (c) 2020 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package defpattern [defpattern]

(defmacro defpattern-macro
  [defpattern Name | Body] ->
    (do
      (shen.update-symbol-table Name 4)
      (shen.x.programmable-pattern-matching.register-handler Name)
      (append [define Name { unit --> unit --> unit --> unit --> unit }]
                Body
                [_ _ _ _ -> [fail]])))

)
