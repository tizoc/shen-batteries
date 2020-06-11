\\ Copyright (c) 2020 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\** {1 Libraries loader}

    Libraries for Shen.

    {2 Overview}

    TODO

    {2 Declaring libraries}

    Libraries are declared with the [library.declare] special form, with syntax:

    [(library.declare <LIBRARY-NAME> <LIBRARY-DIRECTIVE> ...)]

    [<LIBRARY-NAME>] is a symbol, it must be unique, and will be used to reference this library.

    Each [<LIBRARY-DIRECTIVE>] is one of:

-   [(loads <TC-OR-FILE> ...)] declares which files to load when requiring this library.
    [<TC-OR-FILE>] is a string with the name of a file to load, [tc+] to enable type-checking before
    loading the files that come next, or [tc-] to disable type-checking before loading the files that come next.
    By default, [tc-] is assumed.
-   [(requires <LIBRARY-NAME> ...)] declares the dependencies of this library.
    Each [<LIBRARY-NAME>] is the name of a library that will be required and activated before loading
    the files of the library being defined.
-   [(provides-pattern-handlers <FUNCTION-NAME> ...)] declares any function defined in this library that
    will be activated to extend the pattern handler when this library is activated.

    {2 API}

    {3 Loading libraries}

    [(library.use [<LIBRARY-NAME> ...])] loads and activates every library named in the list.
    Performing [library.use] on a library that is already active has no effect.

    [(library.require [<LIBRARY-NAME> ...])] loads every library named in the list without performing any activation.
    Performing [library.require] on a library that has been required before has no effect.

    Most of the time [library.use] will be used in user code, with [library.required] reserved for special situations.

    {3 Deactivating libraries}

    [(library.unuse [<LIBRARY-NAME> ...])] deactivates every library named in the list.
    Performing [library.unuse] on a library that is not active has no effect.

*\

(package library [
    loads requires tc+ tc-
    disable-macros
    preclude-types
    provides-pattern-handlers disable-pattern-handlers
]

(set *libraries* (shen.dict 100))
(set *defaults* [
    [loaded | false]
    [active | false]
    [requires]
    [provides-macros]
    [provides-types]
    [provides-pattern-handlers]
    [preclude-types]
    [disable-pattern-handlers]
    [disable-macros]
    [loads]
])

(define register-prop
  Name Prop Value -> (put Name Prop Value (value *libraries*)))

(define get-prop
  Name Prop -> (trap-error
                 (get Name Prop (value *libraries*))
                 (/. _ (error "Could not find a library named `~A`" Name))))

(define set-default-props
  Name [] -> Name
  Name [[Prop | Value] | Rest] -> (do (register-prop Name Prop Value)
                                      (set-default-props Name Rest)))

(define for-each
  F [] -> unit
  F [X | Rest] -> (do (F X) (for-each F Rest)))

(defmacro library-macro
  [library.declare Name | Declarations]
    -> (do (set-default-props Name (value *defaults*))
           (for-each
             (/. Declaration (process-declarations Name Declaration))
             Declarations)
           Name))

(define process-declarations
  Name [requires | Libraries]     -> (register-prop Name requires Libraries)
  Name [disable-macros | Macros]  -> (register-prop disable-macros Macros)
  Name [preclude-types | Types]   -> (register-prop preclude-types Types)
  Name [disable-pattern-handlers | Handlers]
                                  -> (register-prop disable-pattern-handlers Handlers)
  Name [provides-pattern-handlers | Handlers]
                                  -> (register-prop provides-pattern-handlers Handlers)
  Name [loads | Loads]            -> (register-prop Name loads Loads)
  Name Other -> (error "Invalid library declaration for ~A" Name)
  )

(define current-compiler-context
  -> (let OriginalMacros (value *macros*)
          OriginalMacroreg (value shen.*macroreg*)
          OriginalDatatypes (value shen.*datatypes*)
          OriginalAlldatatypes (value shen.*alldatatypes*)
       [OriginalMacros OriginalMacroreg OriginalDatatypes OriginalAlldatatypes]))

(define restore-compiler-context
  [OriginalMacros OriginalMacroreg OriginalDatatypes OriginalAlldatatypes]
    -> (do (set *macros* OriginalMacros)
           (set shen.*macroreg* OriginalMacroreg)
           (set shen.*datatypes* OriginalDatatypes)
           (set shen.*alldatatypes* OriginalAlldatatypes)))

(define remove-#type-suffix
  [] -> []
  [T | Rest] -> [(remove-#type-suffix-h T) | (remove-#type-suffix Rest)])

(define remove-#type-suffix-h
  Sym -> (intern (remove-#type-suffix-h (str Sym))) where (symbol? Sym)
  "#type" -> ""
  (@s C Rest) -> (@s C (remove-#type-suffix-h Rest)))

(define register-compiler-context-diff
  Name [OriginalMacros OriginalMacroreg OriginalDatatypes OriginalAlldatatypes]
    -> (let MacroRegDiff (difference (value shen.*macroreg*) OriginalMacroreg)
            DatatypesDiff (difference (value shen.*datatypes*) OriginalDatatypes)
          (do (register-prop Name provides-macros MacroRegDiff)
              (register-prop Name provides-types (remove-#type-suffix DatatypesDiff)))))

(define remove-internal-types
  [_ _ OriginalDatatypes OriginalAllDatatypes]
    -> (let DatatypesDiff (difference (value shen.*datatypes*) OriginalDatatypes)
            AllDatatypesDiff (difference (value shen.*alldatatypes*) OriginalAllDatatypes)
            InternalTypes (difference AllDatatypesDiff DatatypesDiff)
         (set shen.*alldatatypes* (difference (value shen.*alldatatypes*) InternalTypes))))

(define use-one
  Name -> Name where (get-prop Name active)
  Name -> (let Require (require-one Name)
               Macros (get-prop Name provides-macros)
               PatternHandlers (get-prop Name provides-pattern-handlers)
               Types (get-prop Name provides-types)
               EnableMacros (for-each (/. M (shen.add-macro M)) (reverse Macros))
               EnablePatternHandlers (for-each (/. H (shen.x.programmable-pattern-matching.register-handler H))
                                               (reverse PatternHandlers))
               EnableTypes (include Types)
               MarkActive (register-prop Name active true)
            Name))

(define unuse-one
  Name -> Name where (not (get-prop Name active))
  Name -> (do (for-each (/. Macro (trap-error (undefmacro Macro) (/. _ skip)))
                        (get-prop Name provides-macros))
              (for-each (/. H (trap-error (shen.x.programmable-pattern-matching.unregister-handler H) (/. _ skip)))
                        (get-prop Name provides-macros))
              (preclude (get-prop Name provides-types))
              (register-prop Name active false)
              Name))

(define inactive-libraries
  [] -> []
  [Name | Rest] -> (inactive-libraries Rest) where (get-prop Name active)
  [Name | Rest] -> [Name | (inactive-libraries Rest)])

(define require-one
  Name -> Name where (get-prop Name loaded)
  Name -> (let Requires (get-prop Name requires)
               _ (require Requires)
               InactiveLibs (inactive-libraries Requires)
               _ (use InactiveLibs)
               Loads (get-prop Name loads)
               OriginalTC (if (tc?) + -)
               OriginalContext (current-compiler-context)
               _ (tc -)
               _ (trap-error (handle-loads Loads)
                   (/. E (do (tc OriginalTC)
                             (unuse InactiveLibs)
                             (restore-compiler-context OriginalContext)
                             (error (error-to-string E)))))
               _ (register-compiler-context-diff Name OriginalContext)
               _ (remove-internal-types OriginalContext)
               _ (tc OriginalTC)
               _ (unuse [Name | InactiveLibs])
               _ (register-prop Name loaded true)
            Name))

(define handle-loads
  [] -> []
  [tc+ | Rest] -> (do (tc +) (handle-loads Rest))
  [tc- | Rest] -> (do (tc -) (handle-loads Rest))
  [File | Rest] -> (do (load File) (handle-loads Rest)))

(define use
  [] -> unit
  [Name | Rest] -> (do (use-one Name)
                       (use Rest)))

(define unuse
  [] -> unit
  [Name | Rest] -> (do (unuse-one Name)
                       (unuse Rest)))

(define require
  [] -> unit
  [Name | Rest] -> (do (require-one Name)
                       (require Rest)))

(declare use [[list symbol] --> unit])
(declare unuse [[list symbol] --> unit])
(declare require [[list symbol] --> unit])

)