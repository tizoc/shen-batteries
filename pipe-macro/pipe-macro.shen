\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Pipes and object threading
\\:
\\: These macros make a sequence of nested calls read from left to right.
\\:
\\: == API
\\:
\\: === `=>`
\\:
\\: `(=> Value Stage ...)` threads the result of each stage into the first
\\: argument position of the next stage. A bare function name is a one-place
\\: stage.
\\:
\\: [source,shen]
\\: ----
\\: (=> 2 (+ 3) (* 4))
\\: \\ Expands like: (* (+ 2 3) 4)
\\: \\ Result: 20
\\: ----

(package pipe-macro [=> =>> doto]

(defmacro pipe-first-macro
  [=> Exp] -> Exp
  [=> Exp [Op | Args] | Rest] -> [=> [Op Exp | Args] | Rest]
  [=> Exp Op | Rest] -> [=> [Op Exp] | Rest])

\\: === `=>>`
\\:
\\: `(=>> Value Stage ...)` threads each result into the last argument
\\: position. This is convenient for Shen functions such as `map`, whose data
\\: argument follows the function argument.
\\:
\\: [source,shen]
\\: ----
\\: (=>> [1 2 3] (map (/. X (* X 2))) reverse)
\\: \\ Result: [6 4 2]
\\: ----
(defmacro pipe-last-macro
  [=>> Exp] -> Exp
  [=>> Exp [Op | Args] | Rest] -> [=>> [Op | (append Args [Exp])] | Rest]
  [=>> Exp Op | Rest] -> [=>> [Op Exp] | Rest])

\\: === `doto`
\\:
\\: `(doto Value (Operation Args ...) ...)` evaluates `Value` once, invokes
\\: every operation with that value inserted as its first argument, discards
\\: the operation results, and returns the original value. Operations must be
\\: parenthesized.
\\:
\\: [source,shen]
\\: ----
\\: (let Box (doto (box.make 1)
\\:             (box.incr)
\\:             (box.incr))
\\:   (box.unbox Box))
\\: \\ Result: 3
\\: ----
(defmacro doto-macro
  [doto Val] -> Val
  [doto Val | Ops] -> (let V (gensym (protect V))
                        [let V Val
                          [do | (append
                                  (map (/. Op [(head Op) V | (tail Op)]) Ops)
                                  [V])]]))
)
