\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Destructuring lets
\\:
\\: Loading `let-match` extends ordinary `let` binders with list-cons and
\\: tuple destructuring. Non-variable input expressions are evaluated once
\\: before their components are extracted.
\\: Require it with `(library.use [let-match])`.
\\:
\\: [source,shen]
\\: ----
\\: (let [Head | Tail] [1 2 3]
\\:   [Head Tail])
\\: \\ Result: [1 [2 3]]
\\:
\\: (let (@p Left Right) (@p 20 22)
\\:   (+ Left Right))
\\: \\ Result: 42
\\: ----
\\:
\\: `_` may discard either component. These are destructuring forms, not full
\\: pattern matches: they use `hd`, `tl`, `fst`, and `snd` and provide no
\\: fallback clause. For example, `[Head]` extracts the head but does not check
\\: that the tail is empty. An input that an accessor cannot read raises that
\\: accessor's usual error.
\\:
\\: This module extends `let` binders only. Anonymous functions still require
\\: an ordinary variable binder; destructure that variable inside the body:
\\:
\\: [source,shen]
\\: ----
\\: (/. Pair
\\:   (let (@p Left Right) Pair
\\:     (+ Left Right)))
\\: ----
\\:
\\: Reusable functions can instead use Shen's native definition patterns:
\\:
\\: [source,shen]
\\: ----
\\: (define add-pair
\\:   (@p Left Right) -> (+ Left Right))
\\: ----

(package let-match []

(defmacro let-match-macro
  [let [cons H []] Exp Body] -> [let H [hd Exp] Body]
  [let [cons H _T] Exp Body] -> [let H [hd Exp] Body] where (= _V _)
  [let [cons _H T] Exp Body] -> [let T [tl Exp] Body] where (= _H _)
  [let [cons H T] Var Body] -> [let H [hd Var]
                                    T [tl Var]
                                 Body]
      where (variable? Var)
  [let [cons H T] Exp Body]  -> (let Tmp (gensym (protect V))
                                  [let Tmp Exp
                                       H [hd Tmp]
                                       T [tl Tmp]
                                    Body])

  [let [@p F _S] Exp Body] -> [let F [fst Exp] Body] where (= _S _)
  [let [@p _F S] Exp Body] -> [let S [snd Exp] Body] where (= _F _)
  [let [@p H T] Var Body] -> [let H [fst Var]
                                  T [snd Var]
                               Body]
      where (variable? Var)
  [let [@p F S] Exp Body]  -> (let Tmp (gensym (protect V))
                                [let Tmp Exp
                                     F [fst Tmp]
                                     S [snd Tmp]
                                  Body]))
)
