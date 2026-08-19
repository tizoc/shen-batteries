\\ Port-neutral conformance checks for the public shen.module v1 loader API.
\\ The runner loads a loader implementation and dispatches one named case in
\\ a fresh process so permanent module state cannot leak between cases.

(define module-conformance.raises?
  Thunk -> (trap-error
             (do (thaw Thunk) false)
             (/. Error true)))

(define module-conformance.use
  Names -> (trap-error
             (let Quiet (hush +)
                  Result (library.use Names)
               (do (hush -)
                   Result))
             (/. Error
               (do (hush -)
                   (error (error-to-string Error))))))

(define module-conformance.assert-read-error
  Label Name
  -> (test.assert-true
       Label
       (module-conformance.raises?
        (freeze (library.read-module Name)))))

(define module-conformance.assert-use-error
  Label Name
  -> (test.assert-true
       Label
       (module-conformance.raises?
        (freeze (module-conformance.use [Name])))))

(define module-conformance.bump-counter
  { symbol --> number }
  shared -> (set module-conformance.*shared-loads*
              (+ 1 (value module-conformance.*shared-loads*)))
  failure-dependency
    -> (set module-conformance.*failure-dependency-loads*
         (+ 1 (value module-conformance.*failure-dependency-loads*)))
  failure-source
    -> (set module-conformance.*failure-source-attempts*
         (+ 1 (value module-conformance.*failure-source-attempts*)))
  feature-dependency
    -> (set module-conformance.*feature-dependency-loads*
         (+ 1 (value module-conformance.*feature-dependency-loads*)))
  feature-source
    -> (set module-conformance.*feature-source-loads*
         (+ 1 (value module-conformance.*feature-source-loads*)))
  cycle-a -> (set module-conformance.*cycle-a-loads*
               (+ 1 (value module-conformance.*cycle-a-loads*)))
  cycle-b -> (set module-conformance.*cycle-b-loads*
               (+ 1 (value module-conformance.*cycle-b-loads*)))
  home-poison -> (set module-conformance.*home-poison-loads*
                   (+ 1 (value module-conformance.*home-poison-loads*))))

(define module-conformance.record-load
  { symbol --> symbol }
  Label -> (do
             (set module-conformance.*events*
               [[Label (if (tc?) tc+ tc-)]
                | (value module-conformance.*events*)])
             Label))

(define module-conformance.begin
  -> (do
       (set module-conformance.*events* [])
       (set module-conformance.*shared-loads* 0)
       (set module-conformance.*failure-dependency-loads* 0)
       (set module-conformance.*failure-source-attempts* 0)
       (set module-conformance.*feature-dependency-loads* 0)
       (set module-conformance.*feature-source-loads* 0)
       (set module-conformance.*cycle-a-loads* 0)
       (set module-conformance.*cycle-b-loads* 0)
       (set module-conformance.*home-poison-loads* 0)
       (library.set-home "tests/fixtures/shen-module-v1-conformance")))

(define module-conformance.test-success-restoration
  OriginalHome
  -> (do
       (tc -)
       (module-conformance.use [conformance/valid])
       [(= OriginalHome (value *home-directory*))
        (not (tc?))]))

(define module-conformance.test-error-restoration
  OriginalHome
  -> (do
       (tc +)
       (let Failed (module-conformance.raises?
                     (freeze
                      (module-conformance.use
                       [conformance/source-failure conformance/top-a])))
            Result [Failed
                    (= OriginalHome (value *home-directory*))
                    (tc?)]
         (do (tc -)
             Result))))

(define module-conformance.test-use-error-state
  Names Mode
  -> (let OriginalHome (value *home-directory*)
       (do
         (tc Mode)
         (let Failed (module-conformance.raises?
                       (freeze (module-conformance.use Names)))
              Result [Failed
                      (= OriginalHome (value *home-directory*))
                      (= Mode (if (tc?) + -))]
           (do (tc -)
               Result)))))

(define module-conformance.descriptors
  -> (do
       (module-conformance.begin)

       (test.assert-equal
        "read-module preserves declared dependency order"
        [conformance/left conformance/right]
        (library.module-requires (library.read-module conformance/valid)))

       (test.assert-equal
        "read-module does not load dependencies or sources"
        [[] 0]
        [(value module-conformance.*events*)
         (value module-conformance.*shared-loads*)])

       (test.assert-equal
        "read-module accepts a descriptor whose source is missing"
        []
        (library.module-requires
         (library.read-module conformance/missing-source)))

       (test.assert-equal
        "read-module does not check features or load their dependencies"
        [[conformance/feature-dependency] 0]
        [(library.module-requires
          (library.read-module conformance/feature-failure))
         (value module-conformance.*feature-dependency-loads*)])

       (module-conformance.assert-read-error
        "an empty descriptor is rejected" invalid/empty)
       (module-conformance.assert-read-error
        "a descriptor with multiple forms is rejected"
        invalid/multiple-forms)
       (module-conformance.assert-read-error
        "a descriptor with the wrong head is rejected" invalid/wrong-head)
       (module-conformance.assert-read-error
        "a descriptor without a version is rejected" invalid/missing-version)
       (module-conformance.assert-read-error
        "an unsupported descriptor version is rejected" invalid/wrong-version)
       (module-conformance.assert-read-error
        "a descriptor without a name is rejected" invalid/missing-name)
       (module-conformance.assert-read-error
        "a descriptor name must be a symbol" invalid/non-symbol-name)
       (module-conformance.assert-read-error
        "a descriptor without sources is rejected" invalid/missing-sources)
       (module-conformance.assert-read-error
        "an empty sources field is rejected" invalid/empty-sources)
       (module-conformance.assert-read-error
        "a duplicate standard field is rejected" invalid/duplicate-name)
       (module-conformance.assert-read-error
        "a duplicate optional field is rejected" invalid/duplicate-requires)
       (module-conformance.assert-read-error
        "an unknown standard field is rejected" invalid/unknown-field)
       (module-conformance.assert-read-error
        "a malformed bracket field is rejected" invalid/malformed-bracket-field)
       (module-conformance.assert-read-error
        "a source before its mode is rejected" invalid/source-without-mode)
       (module-conformance.assert-read-error
        "a dangling source mode is rejected" invalid/dangling-mode)
       (module-conformance.assert-read-error
        "adjacent source modes are rejected" invalid/adjacent-modes)
       (module-conformance.assert-read-error
        "a non-string source is rejected" invalid/non-string-source)
       (module-conformance.assert-read-error
        "an empty source path is rejected" invalid/empty-source-path)
       (module-conformance.assert-read-error
        "an absolute source path is rejected" invalid/absolute-source-path)
       (module-conformance.assert-read-error
        "a drive-prefixed source path is rejected" invalid/drive-source-path)
       (module-conformance.assert-read-error
        "a backslash source path is rejected" invalid/backslash-source-path)
       (module-conformance.assert-read-error
        "a descriptor name must equal its requested name" invalid/name-mismatch)
       (module-conformance.assert-read-error
        "duplicate extension identifiers are rejected" invalid/duplicate-extension)
       (module-conformance.assert-read-error
        "dependency names must be symbols" invalid/non-symbol-requirement)
       (module-conformance.assert-read-error
        "required features must be symbols" invalid/non-symbol-feature)
       (module-conformance.assert-read-error
        "extension identifiers must be symbols" invalid/non-symbol-extension-id)

       (let OriginalHome (value *home-directory*)
         (do
           (module-conformance.assert-read-error
            "descriptor read errors are reported" invalid/wrong-head)
           (test.assert-equal
            "descriptor read errors restore the caller home"
            OriginalHome
            (value *home-directory*))))

       (test.assert-equal
        "descriptor reads do not lock module home"
        unit
        (library.set-home "tests/fixtures/authoring-modules"))

       (test.finish)))

(define module-conformance.loading
  -> (do
       (module-conformance.begin)

       (test.assert-equal
        "successful loading restores home and caller typechecking mode"
        [true true]
        (module-conformance.test-success-restoration
         (value *home-directory*)))

       (test.assert-equal
        "dependencies and sources load in declared order"
        [[shared tc-] [left tc-] [right tc-] [syntax tc-]]
        (reverse (value module-conformance.*events*)))

       (test.assert-equal
        "a checked source uses its dependency and preceding macro source"
        42
        (module-conformance-valid.answer 20))

       (test.assert-equal
        "a shared transitive dependency loads once"
        1
        (value module-conformance.*shared-loads*))

       (let Events (value module-conformance.*events*)
         (do
           (module-conformance.use [conformance/valid conformance/valid])
           (test.assert-equal
            "completed modules are not loaded again"
            [Events 1]
            [(value module-conformance.*events*)
             (value module-conformance.*shared-loads*)])))

       (set module-conformance.*events* [])
       (module-conformance.use [conformance/top-a conformance/top-b])
       (test.assert-equal
        "top-level requests load from left to right"
        [[top-a tc-] [top-b tc-]]
        (reverse (value module-conformance.*events*)))

       (module-conformance.assert-use-error
        "tc+ rejects an ill-typed source" conformance/type-error)

       (let OriginalHome (value *home-directory*)
         (do
           (module-conformance.use [conformance/home-poison])
           (test.assert-equal
            "each source resolves from the descriptor base"
            [1 OriginalHome]
            [(value module-conformance.*home-poison-loads*)
             (value *home-directory*)])))

       (test.assert-true
        "module home cannot change after a completed load"
        (module-conformance.raises?
         (freeze
          (library.set-home "tests/fixtures/authoring-modules"))))

       (test.finish)))

(define module-conformance.source-failure
  -> (do
       (module-conformance.begin)

       (module-conformance.assert-use-error
        "a missing source is a load error" conformance/missing-source)

       (test.assert-equal
        "module home may still be set after a failure with no completed module"
        unit
        (do
          (library.set-home "tests/fixtures/authoring-modules")
          (library.set-home "tests/fixtures/shen-module-v1-conformance")))

       (test.assert-equal
        "source errors restore home and caller typechecking mode"
        [true true true]
        (module-conformance.test-error-restoration
         (value *home-directory*)))

       (test.assert-equal
        "a completed dependency survives a later source failure"
        1
        (value module-conformance.*failure-dependency-loads*))

       (test.assert-equal
        "effects before a source failure are not rolled back"
        1
        (value module-conformance.*failure-source-attempts*))

       (test.assert-equal
        "a failing top-level request suppresses later top-level modules"
        []
        (value module-conformance.*events*))

       (test.assert-true
        "a completed dependency locks module home despite parent failure"
        (module-conformance.raises?
         (freeze
          (library.set-home
           "tests/fixtures/authoring-modules"))))

       (module-conformance.assert-use-error
        "a failed module is not marked loaded and fails again on retry"
        conformance/source-failure)

       (test.assert-equal
        "retry reruns the failing module but not its completed dependency"
        [2 1]
        [(value module-conformance.*failure-source-attempts*)
         (value module-conformance.*failure-dependency-loads*)])

       (test.finish)))

(define module-conformance.cycle
  -> (do
       (module-conformance.begin)

       (test.assert-equal
        "cycle errors restore home and a tc+ caller"
        [true true true]
        (module-conformance.test-use-error-state
         [conformance/cycle-a] +))

       (test.assert-equal
        "cycle detection runs before either module source"
        [0 0]
        [(value module-conformance.*cycle-a-loads*)
         (value module-conformance.*cycle-b-loads*)])

       (test.assert-equal
        "cycle failure does not lock module home"
        unit
        (library.set-home "tests/fixtures/authoring-modules"))

       (test.finish)))

(define module-conformance.feature
  -> (do
       (module-conformance.begin)

       (test.assert-equal
        "feature-failure descriptor is valid without loading it"
        [conformance/feature-dependency]
        (library.module-requires
         (library.read-module conformance/feature-failure)))

       (test.assert-equal
        "feature errors restore home and a tc- caller"
        [true true true]
        (module-conformance.test-use-error-state
         [conformance/feature-failure] -))

       (test.assert-equal
        "feature requirements are checked before dependencies and sources"
        [0 0]
        [(value module-conformance.*feature-dependency-loads*)
         (value module-conformance.*feature-source-loads*)])

       (test.assert-equal
        "feature failure does not lock module home"
        unit
        (library.set-home "tests/fixtures/authoring-modules"))

       (test.finish)))

(define module-conformance.main
  [_ "descriptors"] -> (module-conformance.descriptors)
  [_ "loading"] -> (module-conformance.loading)
  [_ "source-failure"] -> (module-conformance.source-failure)
  [_ "cycle"] -> (module-conformance.cycle)
  [_ "feature"] -> (module-conformance.feature)
  Args -> (error "expected one conformance case, got: ~R" Args))
