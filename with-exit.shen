\\ Copyright (c) 2020 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Early exits
\\:
\\: `(with-exit ExitF Body)` binds `ExitF` to a one-place function that when called
\\: interrupts the rest of the computation and returns from `with-exit` with the
\\: value passed as an argument.
\\:
\\: Example:
\\:
\\: [source,shen]
\\: (with-exit Exit (+ 3 4 (Exit 10) 5))
\\: // Result: 10 : number
\\:

(package with-exit [sexp void maybe.t maybe.unsafe-get @some @none box.make box.unbox box.put with-exit features.cond]

(features.cond
  shen/scheme
    (datatype t
      Exit : (A --> B) >> Body : A;
      ____________________________
      (scm.call/1cc (lambda Exit Body)) : A;)

  true skip)

(datatype t-internal
  X : sexp;
  ____________________________
  (cons? X) : verified >> X : (list sexp);)

(define guard-catch
  { string --> (exception --> A) --> exception --> A }
  Tag Handler Err -> (let S (error-to-string Err)
                       (if (= Tag S)
                           (Handler Err)
                           (simple-error S))))

(define subst-application
  { symbol --> (sexp --> sexp) --> sexp --> sexp }
  Name F [Name Arg] -> (F Arg)
  Name F [Name | Rest] -> (error "Exit function '~A' must be called with one argument, not ~A" Name (length Rest))
  Name F [let Name Value Body] -> [let Name (subst-application Name F Value) Body]
  Name F Z -> (map (/. W (subst-application Name F W)) Z)  where (cons? Z)
  _ _ Z -> Z)

(defmacro macro
  [with-exit ExitF Body] -> (features.cond
                              shen/scheme
                                [scm.call/1cc [lambda ExitF Body]] \\ TODO: validate arity of calls to ExitF in Body

                              true
                                (let BoxName (gensym (protect Box))
                                     ExitName (str (gensym #exit--tag--))
                                     ExitExpr (/. R [do [box.put BoxName [@some R]] [simple-error ExitName]])
                                  [let BoxName [box.make [@none]]
                                       _ [trap-error [do [box.put BoxName [@some (subst-application ExitF ExitExpr Body)]] 1]
                                           [guard-catch ExitName [lambda _ 1]]]
                                    [maybe.unsafe-get [box.unbox BoxName]]])))

(preclude [t-internal])

)