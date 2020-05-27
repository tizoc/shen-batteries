\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package let-match []

(defmacro let-match-macro
  [let [cons H []] Exp Body] -> [let H [hd Exp] Body]
  [let [cons H _T] Exp Body] -> [let H [hd Exp] Body] where (= _V _)
  [let [cons _H T] Exp Body] -> [let T [tl Exp] Body] where (= _H _)
  [let [cons H T] Exp Body]  -> (let Tmp (gensym (protect V))
                                  [let Tmp Exp
                                       H [hd Tmp]
                                       T [tl Tmp]
                                     Body])

  [let [@p F _S] Exp Body] -> [let F [fst Exp] Body] where (= _S _)
  [let [@p _F S] Exp Body] -> [let S [snd Exp] Body] where (= _F _)
  [let [@p F S] Exp Body]  -> (let Tmp (gensym (protect V))
                                [let Tmp Exp
                                     F [fst Tmp]
                                     S [snd Tmp]
                                   Body]))

)