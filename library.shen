\\ Copyright (c) 2020 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Libraries loader
\\:
\\: Libraries for Shen.
\\:
\\: == Overview
\\:
\\: TODO
\\:
\\: == Declaring libraries
\\:
\\: Libraries are declared with the `library.declare` special form, with syntax:
\\:
\\: [source,shen]
\\: (library.declare <LIBRARY-NAME> <LIBRARY-DIRECTIVE> ...)
\\:
\\: `<LIBRARY-NAME>` is a symbol, it must be unique, and will be used to reference this library.
\\:
\\: Each `<LIBRARY-DIRECTIVE>` is one of:
\\:
\\: - `(loads <TC-OR-FILE> ...)` declares which files to load when requiring this library.
\\:   `<TC-OR-FILE>` is a string with the name of a file to load, `tc+` to enable type-checking before
\\:   loading the files that come next, or [tc-] to disable type-checking before loading the files that come next.
\\:   By default, [tc-] is assumed.
\\: - `(requires <LIBRARY-NAME> ...)` declares the dependencies of this library.
\\:   Each `<LIBRARY-NAME>` is the name of a library that will be required and activated before loading
\\:   the files of the library being defined.
\\: - `(provides-pattern-handlers <FUNCTION-NAME> ...)` declares any function defined in this library that
\\:   will be activated to extend the pattern handler when this library is activated.
\\:
\\: == API
\\:
\\: === Loading libraries
\\:
\\: `(library.use [<LIBRARY-NAME> ...])` loads and activates every library named in the list.
\\: Performing `library.use` on a library that is already active has no effect.
\\:
\\: `(library.require [<LIBRARY-NAME> ...])` loads every library named in the list without performing any activation.
\\: Performing [library.require] on a library that has been required before has no effect.
\\:
\\: Most of the time `library.use` will be used in user code, with `library.required` reserved for special situations.
\\:
\\: === Deactivating libraries
\\:
\\: `(library.unuse [<LIBRARY-NAME> ...])` deactivates every library named in the list.
\\: Performing [library.unuse] on a library that is not active has no effect.

(package library [
    loads requires tc+ tc-
    disable-macros
    preclude-types
    disable-pattern-handlers
]

(set *home* (value *home-directory*))
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
  Name []                      -> Name
  Name [[Prop | Value] | Rest] -> (do (register-prop Name Prop Value)
                                      (set-default-props Name Rest)))

(define for-each
  F []         -> unit
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
  Name [loads | Loads]            -> (register-prop Name loads Loads)
  Name Other -> (error "Invalid library declaration for ~A" Name)
  )

(define current-compiler-context
  -> (let OriginalMacros             (value *macros*)
          OriginalMacroreg           (value shen.*macroreg*)
          OriginalDatatypes          (value shen.*datatypes*)
          OriginalAlldatatypes       (value shen.*alldatatypes*)
          OriginalPatternHandlers    (value shen.x.programmable-pattern-matching.*pattern-handlers*)
          OriginalPatternHandlersReg (value shen.x.programmable-pattern-matching.*pattern-handlers-reg*)
       [OriginalMacros    OriginalMacroreg
        OriginalDatatypes OriginalAlldatatypes
        OriginalPatternHandlers OriginalPatternHandlersReg]))

(define restore-compiler-context
  [OriginalMacros    OriginalMacroreg
   OriginalDatatypes OriginalAlldatatypes
   OriginalPatternHandlers OriginalPatternHandlersReg]
    -> (do (set *macros*             OriginalMacros)
           (set shen.*macroreg*      OriginalMacroreg)
           (set shen.*datatypes*     OriginalDatatypes)
           (set shen.*alldatatypes*  OriginalAlldatatypes)
           (set shen.x.programmable-pattern-matching.*pattern-handlers*     OriginalPatternHandlers)
           (set shen.x.programmable-pattern-matching.*pattern-handlers-reg* OriginalPatternHandlersReg)))

(define remove-#type-suffix
  [] -> []
  [T | Rest] -> [(remove-#type-suffix-h T) | (remove-#type-suffix Rest)])

(define remove-#type-suffix-h
  Sym -> (intern (remove-#type-suffix-h (str Sym))) where (symbol? Sym)
  "#type" -> ""
  (@s C Rest) -> (@s C (remove-#type-suffix-h Rest)))

(define register-compiler-context-diff
  Name [OriginalMacros OriginalMacroreg OriginalDatatypes OriginalAlldatatypes OriginalPatternHandlers OriginalPatternHandlersReg]
    -> (let MacroRegDiff           (difference (value shen.*macroreg*) OriginalMacroreg)
            DatatypesDiff          (difference (value shen.*datatypes*) OriginalDatatypes)
            PatternHandlersRegDiff (difference (value shen.x.programmable-pattern-matching.*pattern-handlers-reg*) OriginalPatternHandlersReg)
          (do (register-prop Name provides-macros           MacroRegDiff)
              (register-prop Name provides-types            (remove-#type-suffix DatatypesDiff))
              (register-prop Name provides-pattern-handlers PatternHandlersRegDiff))))

(define remove-internal-types
  [_ _ OriginalDatatypes OriginalAllDatatypes _ _]
    -> (let DatatypesDiff    (difference (value shen.*datatypes*)    OriginalDatatypes)
            AllDatatypesDiff (difference (value shen.*alldatatypes*) OriginalAllDatatypes)
            InternalTypes    (difference AllDatatypesDiff            DatatypesDiff)
         (set shen.*alldatatypes* (difference (value shen.*alldatatypes*) InternalTypes))))

(define remove-pattern-handlers
  [_ _ _ _ _ OriginalPatternHandlersReg]
    -> (let PatternHandlersRegDiff (difference (value shen.x.programmable-pattern-matching.*pattern-handlers-reg*) OriginalPatternHandlersReg)
         (map (/. Handler (shen.x.programmable-pattern-matching.unregister-handler Handler))
              PatternHandlersRegDiff)))

(define use-one
  Name -> (do (require-one Name)
              (use-one Name))
      where (not (defined? Name))
  Name -> Name where (get-prop Name active)
  Name -> (let Require           (require-one Name)
               Macros            (get-prop Name provides-macros)
               PatternHandlers   (get-prop Name provides-pattern-handlers)
               Types             (get-prop Name provides-types)
               EnableMacros      (for-each (/. M (shen.add-macro M)) (reverse Macros))
               EnablePatternHandlers
                                 (for-each (/. H (shen.x.programmable-pattern-matching.register-handler H))
                                           (reverse PatternHandlers))
               EnableTypes       (include Types)
               MarkActive        (register-prop Name active true)
            Name))

(define unuse-one
  Name -> Name where (not (get-prop Name active))
  Name -> (do (for-each (/. Macro (trap-error (undefmacro Macro) (/. _ skip)))
                        (get-prop Name provides-macros))
              (for-each (/. H (trap-error (shen.x.programmable-pattern-matching.unregister-handler H) (/. _ skip)))
                        (get-prop Name provides-pattern-handlers))
              (preclude (get-prop Name provides-types))
              (register-prop Name active false)
              Name))

(define inactive-libraries
  [] -> []
  [Name | Rest] -> (inactive-libraries Rest) where (get-prop Name active)
  [Name | Rest] -> [Name | (inactive-libraries Rest)])

(define apply-filters
  Name -> (do (for-each (/. Macro (trap-error (undefmacro Macro) (/. _ skip)))
                        (get-prop Name disable-macros))
              (for-each (/. H (trap-error (shen.x.programmable-pattern-matching.unregister-handler H) (/. _ skip)))
                        (get-prop Name disable-pattern-handlers))
              (preclude (get-prop Name preclude-types))))

(define load-library
  Name -> (trap-error (load (cn Name "/ShenLib.shen"))
                      (/. E (error "Library ~A not found in path: ~A" Name (error-to-string E)))))

(define defined?
  Name -> (trap-error
            (do (shen.<-dict (value *libraries*) Name)
                true)
            (/. _ false)))

(define with-cd
  Path F -> (let Current (value *home-directory*)
              (do (trap-error
                    (do (cd Path)
                        (F Path)
                        (cd Current))
                  (/. E (do (cd Current)
                            (error (error-to-string E))))))))

(define require-one
  Name -> (let SName (str Name)
            (with-cd (value *home*)
              (/. Path
                (do (load-library SName)
                    (cd (@s Path "/" SName))
                    (require-one Name)))))
      where (not (defined? Name))
  Name -> Name where (get-prop Name loaded)
  Name -> (let Requires     (get-prop Name requires)
               _            (require Requires)
               InactiveLibs (inactive-libraries Requires)
               _            (use InactiveLibs)
               Loads        (get-prop Name loads)
               OrigTC       (if (tc?) + -)
               OrigCtx      (current-compiler-context)
               _            (tc -)
               _            (apply-filters Name)
               _            (trap-error (handle-loads Loads)
                              (/. E (do (tc OrigTC)
                                        (unuse InactiveLibs)
                                        (restore-compiler-context OrigCtx)
                                        (error (error-to-string E)))))
               _            (register-compiler-context-diff Name OrigCtx)
               _            (remove-internal-types OrigCtx)
               _            (remove-pattern-handlers OrigCtx)
               _            (tc OrigTC)
               _            (unuse [Name | InactiveLibs])
               _            (register-prop Name loaded true)
            Name))

(define handle-loads
  [] -> []
  [tc+  | Rest] -> (do (tc +)      (handle-loads Rest))
  [tc-  | Rest] -> (do (tc -)      (handle-loads Rest))
  [File | Rest] -> (do (load File) (handle-loads Rest)))

(define use
  []            -> unit
  [Name | Rest] -> (do (use-one Name)
                       (use Rest)))

(define unuse
  []            -> unit
  [Name | Rest] -> (do (unuse-one Name)
                       (unuse Rest)))

(define require
  []            -> unit
  [Name | Rest] -> (do (require-one Name)
                       (require Rest)))

(define set-home
  Path -> (set *home* Path))

(declare set-home [string --> unit])
(declare use     [[list symbol] --> unit])
(declare unuse   [[list symbol] --> unit])
(declare require [[list symbol] --> unit])

)