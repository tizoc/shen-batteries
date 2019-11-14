\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package pipe-macro [=> =>> doto]

(defmacro pipe-first-macro
  [=> Exp] -> Exp
  [=> Exp [Op | Args] | Rest] -> [=> [Op Exp | Args] | Rest]
  [=> Exp Op | Rest] -> [=> [Op Exp] | Rest])

(defmacro pipe-last-macro
  [=>> Exp] -> Exp
  [=>> Exp [Op | Args] | Rest] -> [=>> [Op | (append Args [Exp])] | Rest]
  [=>> Exp Op | Rest] -> [=>> [Op Exp] | Rest])

(defmacro doto-macro
  [doto Val | Ops] -> (let V (gensym (protect V))
                        [let V Val
                          [do | (map (/. Op [(head Op) V | (tail Op)]) Ops)]
                          V]))
)
