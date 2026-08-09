(set *test-home-directory* (value *home-directory*))
(set *test-typechecking* (tc?))

(load "library.shen")
(library.use
  [features
   box
   lazy
   dict
   typ/or
   typ/sexp
   typ/verified-objects
   typ/verified-and-head
   let-match
   pipe-macro
   cexpr])
(library.use [box])
(shen.x.features.cond-expand
  shen/scheme (library.use [with-exit])
  true skip)

\\ Load the iterator cache independently while pattern matching is deferred.
(let Typechecking (if (tc?) + -)
  (do (tc +)
      (load "iter/mlist.shen")
      (tc Typechecking)))
