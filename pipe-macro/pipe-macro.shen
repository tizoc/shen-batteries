\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Pipes and object threading
\\:
\\: These macros make a sequence of nested calls read from left to right.
\\: Load them with `(library.use [pipe-macro])`. For a tutorial and a
\\: comparison with computation expressions, see
\\: link:using-pipe-macros.adoc[Using pipe macros].
\\:
\\: Pipes are syntax rewrites, not runtime control-flow constructs. They
\\: preserve ordinary Shen call behavior: errors propagate, and explicit
\\: stage arguments follow the evaluation rules of the expanded call.
\\: Inside a Shen `package`, list `=>`, `=>>`, and `doto` among the package
\\: externals when using them, along with any non-core stage functions from
\\: other modules.
\\:
\\: == API
\\:
\\: === `=>`
\\:
\\: `(=> Value Stage ...)` threads the result of each stage into the first
\\: argument position of the next stage. A parenthesized `(F A ...)` stage
\\: becomes `(F Current A ...)`; a bare `F` stage becomes `(F Current)`.
\\: With no stages, `(=> Value)` expands to `Value`.
\\:
\\: [source,shen]
\\: ----
\\: (=> 2 (+ 3) (* 4))
\\: \\ Expands to: (* (+ 2 3) 4)
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
\\: position. A parenthesized `(F A ...)` stage becomes `(F A ... Current)`;
\\: a bare `F` stage becomes `(F Current)`. With no stages, `(=>> Value)`
\\: expands to `Value`. Thread-last is convenient for Shen functions such as
\\: `map`, whose data argument follows the function argument.
\\:
\\: [source,shen]
\\: ----
\\: (=>> [1 2 3] (map (/. X (* X 2))) reverse)
\\: \\ Expands to: (reverse (map (/. X (* X 2)) [1 2 3]))
\\: \\ Result: [6 4 2]
\\: ----
(defmacro pipe-last-macro
  [=>> Exp] -> Exp
  [=>> Exp [Op | Args] | Rest] -> [=>> [Op | (append Args [Exp])] | Rest]
  [=>> Exp Op | Rest] -> [=>> [Op Exp] | Rest])

\\: === `doto`
\\:
\\: `(doto Value (Operation Args ...) ...)` evaluates `Value` exactly once,
\\: invokes every operation from left to right with that same value inserted
\\: as its first argument, discards the operation results, and returns the
\\: original value. Operations must be parenthesized. With no operations,
\\: `(doto Value)` expands to `Value`.
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
