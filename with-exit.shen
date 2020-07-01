\\ Copyright (c) 2020 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Early exits
\\:
\\: `(with-return ReturnF Body)` binds `ReturnF` to a one-place function that when called
\\: interrupts the rest of the computation and returns from `with-exit` with the
\\: value passed as an argument.
\\:
\\: Example:
\\:
\\: [source,shen]
\\: (with-return Return (+ 3 4 (Return 10) 5))
\\: \\ Result: 10 : number
\\:
\\: `(with-break BreakF Body)` binds `BreakF` to a zero-place function that when called
\\: interrupts the rest of the computation and returns from `with-exit` with `(void)`.
\\: The result of a `with-break` expression is always the `void` object.
\\:
\\: Example:
\\:
\\: [source,shen]
\\: (with-break Break
\\:   (do (print "Hello ")
\\:       (Break)
\\:       (print "world!")))
\\: \\ Prints only "Hello "
\\:

(package with-exit [sexp void maybe.t maybe.unsafe-get @some @none box.make box.unbox box.put with-return with-break features.cond]

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

(define subst-return-application
  { symbol --> (sexp --> sexp) --> sexp --> sexp }
  Name F [Name Arg] -> (F Arg)
  Name F [Name | Rest] -> (error "Return function '~A' must be called with one argument, not ~A" Name (length Rest))
  Name F [let Name Value Body] -> [let Name (subst-return-application Name F Value) Body]
  Name F Z -> (map (/. W (subst-return-application Name F W)) Z)  where (cons? Z)
  _ _ Z -> Z)

(define subst-break-application
  { symbol --> sexp --> sexp --> sexp }
  Name Subst [Name] -> Subst
  Name Subst [Name | Rest] -> (error "Break function '~A' must be called with no arguments, not ~A" Name (length Rest))
  Name Subst [let Name Value Body] -> [let Name (subst-break-application Name Subst Value) Body]
  Name Subst Z -> (map (/. W (subst-break-application Name Subst W)) Z)  where (cons? Z)
  _ _ Z -> Z)

(defmacro macro
  [with-break BreakF Body]
    -> (features.cond
         shen/scheme
           [scm.call/1cc [lambda BreakF Body]] \\ TODO: validate arity of calls to BreakF in Body

         true
           (let ExitName (str (gensym #exit--tag--))
                ExitExpr [simple-error ExitName]
             [trap-error [do (subst-break-application BreakF ExitExpr Body) [void]]
                [guard-catch ExitName [lambda _ [void]]]]))


  [with-return ReturnF Body]
    -> (features.cond
         shen/scheme
           [scm.call/1cc [lambda ReturnF Body]] \\ TODO: validate arity of calls to ReturnF in Body

         true
           (let BoxName (gensym (protect Box))
                ResultVar (gensym (protect Result))
                ExitName (str (gensym #exit--tag--))
                ExitExpr (/. R [do [box.put BoxName [@some R]]
                                   [simple-error ExitName]])
             [let BoxName [box.make [@none]]
                  _ [trap-error [let ResultVar (subst-return-application ReturnF ExitExpr Body)
                                     _ [box.put BoxName [@some ResultVar]]
                                  ignore]
                      [guard-catch ExitName [lambda _ ignore]]]
               [maybe.unsafe-get [box.unbox BoxName]]])))

(preclude [t-internal])

)