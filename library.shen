\\ Copyright (c) 2020 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Runtime module loader
\\:
\\: Loads permanent, versioned `shen.module` declarations. Descriptors are
\\: resolved as `<home>/<module-name>.shenmod`; source paths are relative to
\\: the descriptor. Dependencies load before their dependents, and reusing an
\\: already loaded module has no effect in the current Shen process.
\\:
\\: A typical application sets its module root before loading anything, then
\\: requests one or more top-level modules:
\\:
\\: [source,shen]
\\: ----
\\: (library.set-home "vendor")
\\: (library.use [acme/app])
\\: ----
\\:
\\: == API

(package library
 [shen.module version name sources requires requires-features extension tc+ tc-]

(set *home* (value *home-directory*))
(set *loaded* [])

(define read-file-unprocessed
  File -> (let Bytes (read-file-as-bytelist File)
            (trap-error
             (compile (/. X (shen.<s-exprs> X)) Bytes)
             (/. E (shen.reader-error (value shen.*residue*))))))

(define single-form
  _ [Form] -> Form
  Path [] -> (error "Module declaration ~A is empty" Path)
  Path Forms -> (error "Module declaration ~A expected one form, got ~A"
                       Path (length Forms)))

(define add-seen
  Field Seen -> (if (element? Field Seen)
                    (error "Module declaration has duplicate field: ~A" Field)
                    [Field | Seen]))

(define symbol-field
  _ X -> X where (symbol? X)
  Field X -> (error "Module declaration field ~A expected a symbol, got: ~S"
                   Field X))

(define symbol-list
  _ [] -> []
  Field [X | Xs] -> [X | (symbol-list Field Xs)] where (symbol? X)
  Field Xs -> (error "Module declaration field ~A expected symbols, got: ~S"
                    Field Xs))

(define source-list
  Sources -> (source-list-h Sources (fail) [] false))

(define relative-source?
  "" -> false
  (@s "/" _) -> false
  (@s "c#92;" _) -> false
  (@s _ (@s ":" _)) -> false
  Path -> (backslash-free? Path))

(define backslash-free?
  "" -> true
  (@s "c#92;" _) -> false
  (@s _ Rest) -> (backslash-free? Rest))

(define source-list-h
  [] _ [] _ -> (error "Module declaration requires at least one source")
  [] Mode _ true -> (error "Module source mode ~A must be followed by a source"
                           Mode)
  [] _ Out false -> (reverse Out)
  [tc+ | _] Mode _ true
  -> (error "Module source mode ~A must be followed by a source" Mode)
  [tc- | _] Mode _ true
  -> (error "Module source mode ~A must be followed by a source" Mode)
  [tc+ | Rest] _ Out false -> (source-list-h Rest tc+ Out true)
  [tc- | Rest] _ Out false -> (source-list-h Rest tc- Out true)
  [Source | _] Mode _ _
  -> (error "Module source ~S must follow tc+ or tc-" Source)
    where (= Mode (fail))
  [Source | Rest] Mode Out _
  -> (source-list-h Rest Mode [[module-source Mode Source] | Out] false)
    where (and (string? Source) (relative-source? Source))
  [Source | _] _ _ _
  -> (error "Module source must be relative, got: ~S" Source)
    where (string? Source)
  [Source | _] _ _ _
  -> (error "Module source must be a string, got: ~S" Source))

(define extension-id
  [module-extension Id _] -> Id)

(define extension-ids
  Extensions -> (map (fn extension-id) Extensions))

(define add-extension
  Id Body Extensions
  -> (let Id (symbol-field extension Id)
       (if (element? Id (extension-ids Extensions))
           (error "Module declaration has duplicate extension: ~A" Id)
           [[module-extension Id Body] | Extensions])))

(define parse-module
  [shen.module | Fields]
  -> (parse-fields Fields [] (fail) (fail) (fail) [] [] [])
  Form -> (error "Module declaration expected shen.module form, got: ~S" Form))

(define parse-fields
  [] _ V N Ss Rs RFs Extensions
  -> (finalize-module V N Ss Rs RFs (reverse Extensions))
  [[version V] | Fields] Seen _ N Ss Rs RFs Extensions
  -> (parse-fields Fields (add-seen version Seen)
                   V N Ss Rs RFs Extensions)
  [[name N] | Fields] Seen V _ Ss Rs RFs Extensions
  -> (parse-fields Fields (add-seen name Seen)
                   V N Ss Rs RFs Extensions)
  [[sources | Ss] | Fields] Seen V N _ Rs RFs Extensions
  -> (parse-fields Fields (add-seen sources Seen)
                   V N Ss Rs RFs Extensions)
  [[requires | Rs] | Fields] Seen V N Ss _ RFs Extensions
  -> (parse-fields Fields (add-seen requires Seen)
                   V N Ss Rs RFs Extensions)
  [[requires-features | RFs] | Fields] Seen V N Ss Rs _ Extensions
  -> (parse-fields Fields (add-seen requires-features Seen)
                   V N Ss Rs RFs Extensions)
  [[extension Id | Body] | Fields] Seen V N Ss Rs RFs Extensions
  -> (parse-fields Fields Seen V N Ss Rs RFs
                   (add-extension Id Body Extensions))
  [Field | _] _ _ _ _ _ _ _
  -> (error "Module declaration has unknown or malformed field: ~S" Field))

(define finalize-module
  V _ _ _ _ _ -> (error "Module declaration requires (version 1)")
    where (not (= V 1))
  _ N _ _ _ _ -> (error "Module declaration requires a name field")
    where (= N (fail))
  _ _ Ss _ _ _ -> (error "Module declaration requires a sources field")
    where (= Ss (fail))
  _ N Ss Rs RFs Extensions
  -> [module-declaration
      (symbol-field name N)
      (source-list Ss)
      (symbol-list requires Rs)
      (symbol-list requires-features RFs)
      Extensions])

(define module-name
  [module-declaration Name _ _ _ _] -> Name)

(define module-sources
  [module-declaration _ Sources _ _ _] -> Sources)

\\: `(library.module-requires Module)` returns the descriptor's direct module
\\: requirements in declaration order. Obtain `Module` with
\\: `library.read-module`; dependencies are not loaded by this operation.
(define module-requires
  [module-declaration _ _ Requires _ _] -> Requires)

(define module-required-features
  [module-declaration _ _ _ Features _] -> Features)

(define ends-in-slash?
  "" -> false
  "/" -> true
  Path -> (ends-in-slash? (tlstr Path)))

(define home-prefix
  "" -> ""
  Home -> Home where (ends-in-slash? Home)
  Home -> (@s Home "/"))

(define module-parent
  Name -> (module-parent-h (str Name) "" ""))

(define module-parent-h
  "" Parent _ -> Parent
  (@s C Rest) Parent Path
  -> (let Next (@s Path C)
       (module-parent-h Rest
                        (if (= C "/") Next Parent)
                        Next)))

(define with-home
  Home F -> (let Original (value *home-directory*)
              (trap-error
               (let Set (set *home-directory* Home)
                    Result (F)
                 (do (set *home-directory* Original)
                     Result))
               (/. E (do (set *home-directory* Original)
                          (error (error-to-string E)))))))

(define with-typechecking-state
  F -> (let Original (if (tc?) + -)
        (trap-error
         (let Result (F)
           (do (tc Original)
               Result))
         (/. E (do (tc Original)
                    (error (error-to-string E)))))))

\\: `(library.read-module Name)` reads and validates `Name`'s descriptor below
\\: the configured module home. It checks that the declaration has the
\\: requested name, but does not load its dependencies or source files.
(define read-module
  Name -> (let Path (@s (str Name) ".shenmod")
               Root (home-prefix (value *home*))
               Module (with-home Root
                        (freeze
                         (parse-module
                          (single-form Path
                           (read-file-unprocessed Path)))))
            (if (= Name (module-name Module))
                Module
                (error "Module ~A declares the name ~A"
                       Name (module-name Module)))))

(define current-features
  -> (trap-error (shen.x.features.current) (/. E [])))

(define require-features
  [] -> skip
  [Feature | Features]
  -> (if (element? Feature (current-features))
         (require-features Features)
         (error "Module requires unavailable feature: ~A" Feature)))

(define load-source
  [module-source tc+ File] -> (do (tc +) (load File))
  [module-source tc- File] -> (do (tc -) (load File)))

(define load-sources
  Base Sources
  -> (with-home Base (freeze (load-sources-h Sources))))

(define load-sources-h
  [] -> skip
  [Source | Sources] -> (do (load-source Source)
                            (load-sources-h Sources)))

(define use-one
  Name _ -> Name where (element? Name (value *loaded*))
  Name Stack -> (error "Module dependency cycle includes: ~A" Name)
    where (element? Name Stack)
  Name Stack -> (let Module (read-module Name)
                     Features (module-required-features Module)
                     CheckFeatures (require-features Features)
                     Dependencies (module-requires Module)
                     LoadDependencies (use-h Dependencies [Name | Stack])
                     Base (@s (home-prefix (value *home*))
                              (module-parent Name))
                     LoadSources (load-sources Base (module-sources Module))
                  (do (set *loaded* [Name | (value *loaded*)])
                      Name)))

(define use-h
  [] _ -> unit
  [Name | Names] Stack -> (do (use-one Name Stack)
                              (use-h Names Stack)))

\\: `(library.use Names)` loads every named module and its transitive
\\: dependencies. Each module is loaded once, dependency cycles and missing
\\: required features are rejected, and each source file uses the typechecking
\\: mode declared by its descriptor. The caller's typechecking mode and home
\\: directory are restored even if loading raises an error.
(define use
  Names -> (with-typechecking-state
            (freeze (use-h Names []))))

\\: `(library.set-home Path)` selects the directory below which module
\\: descriptors are resolved. It must be called before any module has been
\\: loaded; changing the root afterwards raises an error.
(define set-home
  Path -> (do (set *home* Path) unit)
    where (= [] (value *loaded*))
  _ -> (error "Module home cannot change after loading a module"))

(declare set-home [string --> unit])
(declare use [[list symbol] --> unit])
(declare read-module [symbol --> A])
(declare module-requires [A --> [list symbol]])

)
