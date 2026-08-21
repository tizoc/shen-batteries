\\ Copyright (c) 2026 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Records
\\:
\\: The `record` module adds closed, nominal, persistent records to Shen.
\\: Record declarations are typed, while construction, matching, and updates
\\: use field names rather than positional arguments. Require the module with
\\: `(library.use [record])`.
\\:
\\: A declaration gives every field a type and ends every field with a
\\: semicolon:
\\:
\\: [source,shen]
\\: ----
\\: (defrecord person
\\:   name : string;
\\:   age : number;)
\\:
\\: (defrecord (entry K V)
\\:   key : K;
\\:   value : V;)
\\: ----
\\:
\\: The first form introduces the nominal type `person`; the second introduces
\\: `(entry K V)`. A declaration generates named construction, matching, and
\\: persistent-update syntax, a safe predicate, and one accessor per field.
\\: The syntax becomes available to following source files and dependent
\\: modules, so a checked consumer must follow the declaration boundary.
\\:
\\: == Construction
\\:
\\: `person.make` requires every field exactly once. Fields can be written in
\\: any order, and their expressions are evaluated once from left to right as
\\: written:
\\:
\\: [source,shen]
\\: ----
\\: (person.make
\\:   age <- 36;
\\:   name <- "Ada";)
\\: ----
\\:
\\: Every labelled clause uses `Field <- Expression;`, including the final
\\: clause. The arrow reads as the field receiving the expression's value.
\\:
\\: Unknown, duplicate, and missing fields are expansion errors.
\\: A declaration may have no fields. Its zero-place `make` form constructs a
\\: nominal singleton value, and its binder-only `match` form is a type-only
\\: pattern.
\\:
\\: == Access and predicates
\\:
\\: The declaration above generates these ordinary typed functions:
\\:
\\: [source,shen]
\\: ----
\\: person?     : A --> boolean
\\: person.name : person --> string
\\: person.age  : person --> number
\\: ----
\\:
\\: The predicate is safe for arbitrary Shen values. Accessors require the
\\: declared record type in checked code.
\\:
\\: `make` is labelled syntax over an ordinary positional generated function
\\: under the same name. A Shen/Scheme module with an explicit sealed export
\\: boundary should export `person.make`, `person?`, and the accessors that
\\: dependent modules use. `person.match` and `person.with` are compile-time
\\: syntax carried by the module's compile-time metadata rather than its
\\: function export list.
\\:
\\: == Named patterns
\\:
\\: `person.match` is a programmable pattern. Its first argument is a variable
\\: bound to the whole record. The remaining arguments select any subset of
\\: the fields in any order, and each selected value accepts an ordinary
\\: nested Shen pattern:
\\:
\\: [source,shen]
\\: ----
\\: (define adult?
\\:   { person --> boolean }
\\:   (person.match Person age Age) -> (>= Age 18))
\\:
\\: (define named-adult?
\\:   { person --> boolean }
\\:   (person.match Person name "Ada" age Age) -> (>= Age 18)
\\:   _ -> false)
\\: ----
\\:
\\: The whole-record variable has the declared record type in checked code.
\\: Unmentioned fields are ignored. The pattern safely declines unrelated
\\: values so a later function clause can match them.
\\:
\\: == Persistent updates
\\:
\\: `person.with` returns a new record and leaves the original unchanged:
\\:
\\: [source,shen]
\\: ----
\\: (person.with Person
\\:   age <- 37;
\\:   name <- "Grace";)
\\: ----
\\:
\\: Updates use the same `Field <- Expression;` clauses as construction.
\\:
\\: The source record is evaluated once, followed by update expressions once
\\: each from left to right. Omitted fields retain their old values, and a
\\: multi-field update allocates one result record. For parameterized records,
\\: `with` preserves the original type parameters; use `make` when an update
\\: intentionally changes them. At least one field must be updated; an empty
\\: `with` is rejected instead of allocating an unchanged copy.
\\:
\\: Record type parameters may be phantom: a declaration does not need to use
\\: every parameter in a field type. The source anchor used by `with` preserves
\\: those parameters just like parameters that occur in fields.
\\:
\\: == Representation
\\:
\\: The public API does not expose storage. Shen/Scheme installs a backend
\\: using immutable Chez records. Other ports use the portable backend unless
\\: they install their own implementation: an exact-length tagged abstract
\\: vector with a nominal tag.
\\: Both backends provide structural equality and print the same
\\: reconstructible named `make` forms.
\\:
\\: A port selects a different representation by setting `*record-backend*`
\\: before record declarations are expanded. Its value is a processor called
\\: once with the complete normalized declaration
\\: `[defrecord Name Parameters [[Field Type] ...]]`. It returns:
\\:
\\: `[Initializer MakeCode PredicateCode FieldCode]`
\\:
\\: `Initializer` establishes the representation once. The remaining values
\\: are expression builders: `MakeCode` receives the field expressions,
\\: `PredicateCode` receives one expression, and `FieldCode` receives a record
\\: expression and a zero-based field index. One shared generator uses those
\\: builders for the representation datatype and the typed public functions;
\\: patterns, labelled construction, and updates remain representation-neutral.
\\: The portable backend builds tagged abstract-vector expressions.
\\: Shen/Scheme carries exact Chez `define-record-type` forms in its initializer,
\\: installs equality, hashing, and printing directly on each type without a
\\: registry, and builds direct calls to the generated constructor, predicate,
\\: and accessors.

(package record
 [defrecord defpattern sexp *record-backend*]

\\ Parse the declaration and call-site tails without teaching the shallow
\\ macros any arity or field-shape cases.
(defcc <declaration>
  Head <field-declarations>
    := [Head <field-declarations>];)

(defcc <field-declarations>
  Field <colon> Type <field-end> <field-declarations>
    := [[Field Type] | <field-declarations>];
  <e> := [];)

(defcc <colon>
  X := skip where (= (str X) ":");)

(defcc <field-end>
  X := skip where (= (str X) ";");)

(defcc <labelled-fields>
  Field <left-arrow> Value <field-end> <labelled-fields>
    := [[Field Value] | <labelled-fields>];
  <e> := [];)

(defcc <left-arrow>
  X := skip where (= (str X) "<-");)

(defcc <named-fields>
  Field Value <named-fields>
    := [[Field Value] | <named-fields>];
  <e> := [];)

(defcc <labelled-expression>
  Head <labelled-fields>
    := [Head <labelled-fields>];)

(defcc <named-expression>
  Head <named-fields>
    := [Head <named-fields>];)

(define parse-declaration
  Body -> (compile (/. Stream (<declaration> Stream)) Body))

(define parse-labelled-fields
  Body -> (compile (/. Stream (<labelled-fields> Stream)) Body))

(define parse-labelled-expression
  Body -> (compile (/. Stream (<labelled-expression> Stream)) Body))

(define parse-named-expression
  Body -> (compile (/. Stream (<named-expression> Stream)) Body))

(define unique?
  [] -> true
  [X | Xs] -> (and (not (element? X Xs)) (unique? Xs)))

(define variables?
  [] -> true
  [X | Xs] -> (and (variable? X) (variables? Xs))
  _ -> false)

(define local-string-h
  "" Local -> Local
  (@s "." Rest) _ -> (local-string-h Rest "")
  (@s C Rest) Local -> (local-string-h Rest (@s Local C)))

(define local-symbol
  Name -> (string->symbol (local-string-h (str Name) "")))

(define normalize-label
  _ Field -> (local-symbol Field)
    where (and (symbol? Field) (not (variable? Field)))
  Record Field -> (error "record ~A expected a literal field label, got: ~S"
                        Record Field))

(define valid-type?
  Parameters X -> (element? X Parameters) where (variable? X)
  _ X -> true where (symbol? X)
  _ [] -> false
  Parameters [X | Xs] -> (and (valid-type? Parameters X)
                               (valid-types? Parameters Xs))
  _ _ -> false)

(define valid-types?
  _ [] -> true
  Parameters [X | Xs] -> (and (valid-type? Parameters X)
                               (valid-types? Parameters Xs))
  _ _ -> false)

(define validate-type
  Record Parameters Type
    -> Type
    where (valid-type? Parameters Type)
  Record _ Type -> (error "record ~A has an invalid or undeclared field type: ~S"
                         Record Type))

(define reserved-label?
  Label -> (element? (str Label)
                     ["make" "match" "with"
                      "%match" "%pattern-handler" "%record-syntax" "%tag"
                      "%representation" "%pattern" "%schema"]))

(define normalize-fields
  _ _ [] -> []
  Record Parameters [[Raw Type] | Fields]
    -> (let Label (normalize-label Record Raw)
            CheckedType (validate-type Record Parameters Type)
         (if (reserved-label? Label)
             (error "record ~A field label is reserved: ~A" Record Label)
             [[Label CheckedType]
              | (normalize-fields Record Parameters Fields)]))
  Record _ Fields -> (error "record ~A has malformed fields: ~S" Record Fields))

(define field-label
  [Label _] -> Label)

(define field-type
  [_ Type] -> Type)

(define field-labels
  [] -> []
  [[Label _] | Fields] -> [Label | (field-labels Fields)])

(define field-types
  [] -> []
  [[_ Type] | Fields] -> [Type | (field-types Fields)])

(define normalize-head
  Name -> [Name []]
    where (and (symbol? Name) (not (variable? Name)))
  [Name | Parameters] -> [Name Parameters]
    where (and (and (symbol? Name) (not (variable? Name)))
               (and (variables? Parameters)
                    (unique? Parameters)))
  Head -> (error "invalid record head: ~S" Head))

(define record-type
  Name [] -> Name
  Name Parameters -> [Name | Parameters])

(define qualify
  Name Suffix -> (string->symbol (@s (str Name) Suffix)))

(define predicate-name
  Name -> (qualify Name "?"))

(define accessor-name
  Name Field -> (qualify Name (@s "." (str Field))))

(define make-name
  Name -> (qualify Name ".make"))

(define match-name
  Name -> (qualify Name ".match"))

(define with-name
  Name -> (qualify Name ".with"))

(define private-name
  Name Suffix -> (qualify Name (@s ".%" Suffix)))

(define schema-tag
  Name -> (private-name Name "tag"))

(define describe-h
  [RawHead RawFields]
    -> (let Head (normalize-head RawHead)
            Name (hd Head)
            Parameters (hd (tl Head))
            Fields (normalize-fields Name Parameters RawFields)
            Labels (field-labels Fields)
         (if (unique? Labels)
             [defrecord Name Parameters Fields]
             (error "record ~A has duplicate field labels" Name)))
  Declaration -> (error "invalid record declaration: ~S" Declaration))

(define describe
  Body -> (describe-h (parse-declaration Body)))

(define signature-tail
  [] Result -> [Result (string->symbol "}")]
  [Argument | Arguments] Result
    -> [Argument (string->symbol "-->") | (signature-tail Arguments Result)])

(define signature
  [] Result -> [(string->symbol "{") (string->symbol "-->")
                Result (string->symbol "}")]
  Arguments Result
    -> [(string->symbol "{") | (signature-tail Arguments Result)])

(define definition
  Name ArgumentTypes ResultType Arguments Body
    -> [define Name
        | (append (signature ArgumentTypes ResultType)
                  (append Arguments [(string->symbol "->") Body]))])

(define rule-separator
  -> (string->symbol "____________________________"))

(define typed-expression
  Expression Type -> [Expression (string->symbol ":") Type (string->symbol ";")])

(define datatype-rule
  Premises Expression Type
    -> (append Premises
         [(rule-separator)
          | (typed-expression Expression Type)]))

(define pattern-datatype-rule
  [] Expression Type -> (datatype-rule [] Expression Type)
  Premises Expression Type
    -> (append Premises
         [(string->symbol "============================")
          | (typed-expression Expression Type)]))

(define list-expression
  [] -> []
  [X | Xs] -> [cons X (list-expression Xs)])

(define theory-expression
  [[foreign Name] | Arguments] -> [Name | Arguments]
  Expression -> Expression)

(define field-rules
  _ _ [] _ _ -> []
  FieldCode Type [Field | Fields] Record Index
    -> (append
         (datatype-rule
          (typed-expression Record Type)
          (theory-expression (FieldCode Record Index))
          (field-type Field))
         (field-rules FieldCode Type Fields Record (+ Index 1))))

(define representation-rules
  Initializer MakeCode PredicateCode FieldCode Type Fields
    -> (let Values (map (/. Field (gensym (protect Value))) Fields)
            Any (gensym (protect Any))
            Record (gensym (protect Record))
         (append
          (datatype-rule [] Initializer symbol)
          (append
           (datatype-rule
            (pattern-premises Values Fields)
            (theory-expression (MakeCode Values))
            [- Type])
           (append
            (datatype-rule []
                           (theory-expression (PredicateCode Any))
                           boolean)
            (field-rules FieldCode Type Fields Record 0))))))

(define representation-theory
  Name Initializer MakeCode PredicateCode FieldCode Type Fields
    -> [datatype (private-name Name "representation")
        | (representation-rules
           Initializer MakeCode PredicateCode FieldCode Type Fields)])

(define make-definition
  MakeCode Name Type Fields
    -> (let Values (map (/. Field (gensym (protect Value))) Fields)
         (definition (make-name Name)
                     (field-types Fields)
                     Type
                     Values
                     (MakeCode Values))))

(define predicate-definition
  PredicateCode Name
    -> (let X (gensym (protect X))
            Any (gensym (protect A))
         (definition (predicate-name Name) [Any] boolean [X]
                     (PredicateCode X))))

(define field-definitions
  _ _ _ [] _ -> []
  FieldCode Name Type [Field | Fields] Index
    -> (let Record (gensym (protect Record))
            Public (accessor-name Name (field-label Field))
         [(definition Public [Type] (field-type Field) [Record]
                      (FieldCode Record Index))
          | (field-definitions
              FieldCode Name Type Fields (+ Index 1))]))

(define representation-forms
  [Initializer MakeCode PredicateCode FieldCode] Name Type Fields
    -> (let Theory (private-name Name "representation")
         [(representation-theory
            Name Initializer MakeCode PredicateCode FieldCode Type Fields)
          Initializer
          (make-definition MakeCode Name Type Fields)
          (predicate-definition PredicateCode Name)
          | (append (field-definitions FieldCode Name Type Fields 0)
                    [[preclude [cons Theory []]]])]))

(define pattern-theory
  Name Type Fields
    -> (let Whole (gensym (protect Whole))
            Patterns (map (/. Field (gensym (protect Pattern))) Fields)
            Premises (append (typed-expression Whole Type)
                             (pattern-premises Patterns Fields))
         [datatype (private-name Name "pattern")
          | (pattern-datatype-rule Premises
                                   [(private-name Name "match") Whole | Patterns]
                                   Type)]))

(define pattern-premises
  [] [] -> []
  [Pattern | Patterns] [Field | Fields]
    -> (append (typed-expression Pattern (field-type Field))
               (pattern-premises Patterns Fields)))

(define pattern-actions
  _ [] [] _ -> (string->symbol "handled")
  Name [Pattern | Patterns] [Field | Fields] Self
    -> [do [(protect Assign) Pattern
                             (list-expression
                               [(accessor-name Name (field-label Field)) Self])]
            (pattern-actions Name Patterns Fields Self)])

(define pattern-handler
  Name Fields
    -> (let Self (gensym (protect Self))
            Is? (gensym (protect Is?))
            Assign (protect Assign)
            Whole (gensym (protect Whole))
            Patterns (map (/. Field (gensym (protect Pattern))) Fields)
            Pattern (list-expression
                      [(private-name Name "match") Whole | Patterns])
            Actions [do [Assign Whole Self]
                        (pattern-actions Name Patterns Fields Self)]
            Handler (private-name Name "pattern-handler")
            Test (list-expression [(predicate-name Name) Self])
            Body [do [Is? Test] Actions]
         [defpattern Handler
          Self Is? Assign Pattern (string->symbol "->") Body]))

(define syntax-macro
  Name Labels
    -> (let Body (protect Body)
            Internal (private-name Name "record-syntax")
            LabelList (list-expression Labels)
         [defmacro Internal
          [cons (make-name Name) Body]
            (string->symbol "->")
            [expand-make Name LabelList Body]
          [cons (match-name Name) Body]
            (string->symbol "->")
            [expand-match Name LabelList Body]
          [cons (with-name Name) Body]
            (string->symbol "->")
            [expand-with Name LabelList Body]]))

(define public-declaration-forms
  Name Type Fields
    -> [(pattern-theory Name Type Fields)
        (pattern-handler Name Fields)
        (syntax-macro Name (field-labels Fields))])

(define generate
  [defrecord Name Parameters Fields] Implementation
    -> (let Type (record-type Name Parameters)
         [package null []
          | (append (representation-forms
                      Implementation Name Type Fields)
                    (public-declaration-forms Name Type Fields))]))

(define portable-backend
  [defrecord Name Parameters Fields]
    -> (let Tag (schema-tag Name)
            Initializer
              [do [set Tag
                       [portable-intern
                        Tag (make-name Name)
                        (list-expression (field-labels Fields))]]
                  Tag]
         [Initializer
          (/. Values
              [portable-make [value Tag] (list-expression Values)])
          (/. X [portable? [value Tag] X])
          (/. Record
              (/. Index [portable-ref [value Tag] Record Index]))]))

(define process
  Declaration -> (generate Declaration
                           ((value *record-backend*) Declaration))
    where (bound? *record-backend*)
  Declaration -> (generate Declaration (portable-backend Declaration)))

\\: == `defrecord`
\\:
\\: `(defrecord Head Field : Type; ...)` declares a record and generates its
\\: API. `Head` is a literal type name or `(Name TypeVariable ...)`. Field
\\: labels are literal symbols treated as local names, field types may mention
\\: only the declared type variables, and the final semicolon is mandatory.
(defmacro defrecord-macro
  [defrecord | Body]
    -> (process (describe Body)))

(define normalize-named-fields
  _ _ [] _ -> []
  Record Labels [[Raw Value] | Named] Seen
    -> (let Label (normalize-label Record Raw)
         (if (element? Label Seen)
             (error "record ~A field appears more than once: ~A" Record Label)
             (if (not (element? Label Labels))
                 (error "record ~A has no field named ~A" Record Label)
                 [[Label Value]
                  | (normalize-named-fields
                      Record Labels Named [Label | Seen])])))
  Record _ Named _ -> (error "record ~A has malformed named fields: ~S"
                             Record Named))

(define ensure-complete
  Record Labels Named
    -> Named where (= (length Labels) (length Named))
  Record _ _ -> (error "record ~A is missing one or more fields" Record))

(define named-bindings
  [] -> []
  [[Label Expression] | Named]
    -> [[Label (gensym (protect Value)) Expression]
        | (named-bindings Named)])

(define binding-variable
  [_ Variable _] -> Variable)

(define bind-expressions
  [] Body -> Body
  [[_ Variable Value] | Bindings] Body
    -> [let Variable Value
             (bind-expressions Bindings Body)])

(define construction-arguments
  [] _ -> []
  [Label | Labels] Bindings
    -> [(binding-variable
          (assoc Label Bindings))
        | (construction-arguments Labels Bindings)])

(define labelled-fields?
  [_ Arrow | _] -> (= (str Arrow) "<-") where (symbol? Arrow)
  _ -> false)

(define expand-make
  Name _ Body
    -> (cons (make-name Name) Body)
    where (not (labelled-fields? Body))
  Name Labels Body
    -> (let Named (ensure-complete
                    Name Labels
                    (normalize-named-fields Name Labels
                                            (parse-labelled-fields Body) []))
            Bindings (named-bindings Named)
            Construct (cons (make-name Name)
                            (construction-arguments Labels Bindings))
         (bind-expressions Bindings Construct)))

(define pattern-for-label
  Label Named
    -> (let Entry (assoc Label Named)
         (if (= Entry [])
             (protect _)
             (hd (tl Entry)))))

(define normalized-patterns
  [] _ -> []
  [Label | Labels] Named
    -> [(pattern-for-label Label Named)
        | (normalized-patterns Labels Named)])

(define expand-match
  Name Labels Body
    -> (let Parsed (parse-named-expression Body)
            Whole (hd Parsed)
            RawNamed (hd (tl Parsed))
            CheckedWhole (if (variable? Whole)
                             Whole
                             (error "record ~A.match expected a whole-record variable, got: ~S"
                                    Name Whole))
            Named (normalize-named-fields Name Labels RawNamed [])
         [(private-name Name "match") CheckedWhole
          | (normalized-patterns Labels Named)]))

(define final-field-expression
  Name Source Label Bindings
    -> (let Binding (assoc Label Bindings)
         (if (= Binding [])
             [(accessor-name Name Label) Source]
             (binding-variable Binding))))

(define final-field-expressions
  _ _ [] _ -> []
  Name Source [Label | Labels] Bindings
    -> [(final-field-expression Name Source Label Bindings)
        | (final-field-expressions Name Source Labels Bindings)])

(define expand-with-h
  Name Labels Source Named
    -> (let SourceVar (gensym (protect Record))
            Bindings (named-bindings Named)
            Rebuild (cons (make-name Name)
                          (final-field-expressions
                            Name SourceVar Labels Bindings))
            Anchored [if true Rebuild SourceVar]
         [let SourceVar Source
           (bind-expressions Bindings Anchored)]))

(define expand-with
  Name Labels Body
    -> (let Parsed (parse-labelled-expression Body)
            Source (hd Parsed)
            RawNamed (hd (tl Parsed))
            Named (normalize-named-fields Name Labels RawNamed [])
         (if (= Named [])
             (error "record ~A.with requires at least one field update" Name)
             (expand-with-h Name Labels Source Named))))

)
