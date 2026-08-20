\\ Copyright (c) 2020 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Early exits
\\:
\\: `with-return` and `with-break` provide portable, lexically scoped early
\\: exits. Shen/Scheme uses continuations for the exit operation; other ports
\\: use a tagged exception without exposing that implementation detail to the
\\: caller. Require the module with `(library.use [with-exit])`.
\\:
\\: == API
\\:
\\: === `with-return`
\\:
\\: `(with-return ReturnF Body)` binds `ReturnF` to a one-place exit. Calling
\\: it interrupts the rest of `Body` and makes the whole `with-return`
\\: expression produce the supplied value. If `ReturnF` is not called, the
\\: expression produces the normal result of `Body`.
\\:
\\: [source,shen]
\\: ----
\\: (with-return Return
\\:   (do (Return 10)
\\:       20))
\\: \\ Result: 10 : number
\\: ----
\\:
\\: === `with-break`
\\:
\\: `(with-break BreakF Body)` binds `BreakF` to a zero-place exit. Calling it
\\: interrupts the rest of `Body`. Whether or not the exit is called, the
\\: whole `with-break` expression returns `(void)`.
\\:
\\: [source,shen]
\\: ----
\\: (with-break Break
\\:   (do (print "Hello ")
\\:       (Break)
\\:       (print "world!")))
\\: \\ Prints only "Hello "
\\: ----
\\:
\\: == Scope restrictions
\\:
\\: The name bound by either form is a syntactic construct rather than an
\\: ordinary reusable function value. Consequently:
\\:
\\: * letting it escape the expression body is undefined;
\\: * rebinding it to another name is undefined; and
\\: * passing it to another function is valid only when it is wrapped in a
\\:   lambda that calls it.
\\:
\\: == Portability
\\:
\\: The Shen/Scheme implementation uses continuations. On ports without that
\\: feature, the exit is represented by a private tagged exception. A
\\: `trap-error` inside `Body` must therefore propagate exceptions it does not
\\: handle, or it may also intercept the portable exit signal.

(package with-exit
 [sexp void maybe.t maybe.unsafe-get @some @none
  box.make box.unbox box.put
  with-return with-break features.cond shen/scheme scm.call/1cc]

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
  Name _ [lambda Name Body] -> [lambda Name Body]
  Name F Z -> (map (/. W (subst-return-application Name F W)) Z)  where (cons? Z)
  _ _ Z -> Z)

(define subst-break-application
  { symbol --> sexp --> sexp --> sexp }
  Name Subst [Name] -> Subst
  Name Subst [Name | Rest] -> (error "Break function '~A' must be called with no arguments, not ~A" Name (length Rest))
  Name Subst [let Name Value Body] -> [let Name (subst-break-application Name Subst Value) Body]
  Name _ [lambda Name Body] -> [lambda Name Body]
  Name Subst Z -> (map (/. W (subst-break-application Name Subst W)) Z)  where (cons? Z)
  _ _ Z -> Z)

(defmacro macro
  [with-break BreakF Body]
    -> (features.cond
         shen/scheme
           [[foreign scm.call/1cc]
            [lambda BreakF
              [do (subst-break-application BreakF [BreakF [void]] Body)
                  [void]]]]

         true
           (let ExitName (str (gensym #exit--tag--))
                HandlerVar (gensym (protect Error))
                ExitExpr [simple-error ExitName]
             [trap-error
               [do (subst-break-application BreakF ExitExpr Body) [void]]
               [guard-catch ExitName [lambda HandlerVar [void]]]]))

  [with-return ReturnF Body]
    -> (features.cond
         shen/scheme
           [[foreign scm.call/1cc]
            [lambda ReturnF
              (subst-return-application ReturnF (/. R [ReturnF R]) Body)]]

         true
           (let BoxName (gensym (protect Box))
                ResultVar (gensym (protect Result))
                HandlerVar (gensym (protect Error))
                ExitName (str (gensym #exit--tag--))
                ExitExpr (/. R [do [box.put BoxName [@some R]]
                                   [simple-error ExitName]])
             [let BoxName [box.make [@none]]
                  _ [trap-error
                      [let ResultVar
                           (subst-return-application ReturnF ExitExpr Body)
                           _ [box.put BoxName [@some ResultVar]]
                        ignore]
                      [guard-catch ExitName
                                   [lambda HandlerVar ignore]]]
               [maybe.unsafe-get [box.unbox BoxName]]])))

(preclude [t-internal])

)
