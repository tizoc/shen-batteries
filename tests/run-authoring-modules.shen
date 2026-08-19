(hush +)
(load "library.shen")
(library.set-home "tests/fixtures/authoring-modules")
(library.use [example/greeting])
(hush -)

(if (= "Hello, Shen!" (example-greeting.greet "Shen"))
    (output "[OK] module-authoring example loaded its dependency and ordered sources~%")
    (error "module-authoring example returned the wrong greeting"))
