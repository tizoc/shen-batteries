(set *test-home-directory* (value *home-directory*))
(set *test-typechecking* (tc?))

(load "library.shen")
(shen.x.features.cond-expand
  shen/scheme (library.use [with-exit])
  true skip)
(library.use
  [features
   box
   lazy
   dict
   typ/or
   typ/sexp
   typ/verified-objects
   typ/verified-and-head
   defpattern
   let-match
   pipe-macro
   cexpr
   shendoc])
(library.use [box])

\\ Exercise automatic registration and same-file pattern use.
(let Typechecking (if (tc?) + -)
  (do (tc -)
      (load "tests/fixtures/defpattern-input.shen")
      (tc Typechecking)))

\\ Load the iterator cache independently while pattern matching is deferred.
(let Typechecking (if (tc?) + -)
  (do (tc +)
      (load "iter/mlist.shen")
      (tc Typechecking)))
