(load "tests/harness.shen")

\\ Select the portable implementations before their defining sources are
\\ macro-expanded. This runs in its own process so the feature change cannot
\\ affect the main suite.
(shen.x.features.initialise
  (remove shen/scheme (shen.x.features.current)))

(test.assert-equal
  "portable fallback test disables shen/scheme"
  false
  (element? shen/scheme (shen.x.features.current)))

(load "library.shen")
(library.use [iter])

(let Typechecking (if (tc?) + -)
  (do (tc +)
      (load "tests/fixtures/with-exit-input.shen")
      (tc Typechecking)))

\\ Load assertions only after with-exit's selected macro implementation has
\\ been installed at the preceding file boundary.
(load "tests/portable-fallback.shen")

(test.finish)
