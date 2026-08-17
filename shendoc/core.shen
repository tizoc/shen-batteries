\\ Copyright (c) 2020 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Shen documentation generator
\\:
\\: Shendoc generates AsciiDoc from documentation comments embedded in Shen
\\: source files. Consecutive lines beginning with `\\:` form a documentation
\\: block. A block immediately before a function documents that function; a
\\: blank line makes the block standalone.
\\:
\\: Shendoc never evaluates the documented source. Package external
\\: declarations must be literal symbol lists; computed declarations are
\\: rejected.
\\:
\\: == Usage
\\:
\\: Write documentation to standard output:
\\:
\\: [source,shell]
\\: ----
\\: shen-scheme script shendoc.shen INPUT
\\: ----
\\:
\\: Or write it directly to a file:
\\:
\\: [source,shell]
\\: ----
\\: shen-scheme script shendoc.shen INPUT OUTPUT
\\: ----

(package shendoc []

\\ Import the lexical rules used by Shen's current raw reader.
(defcc <whitespaces> shen.<whitespaces> := shen.<whitespaces>;)
(defcc <comment>     shen.<comment>     := shen.<comment>;)
(defcc <atom>        shen.<atom>        := shen.<atom>;)
(defcc <backslash>   shen.<backslash>   := shen.<backslash>;)
(defcc <lcurly>      shen.<lcurly>      := shen.<lcurly>;)
(defcc <rcurly>      shen.<rcurly>      := shen.<rcurly>;)
(defcc <lsb>         shen.<lsb>         := shen.<lsb>;)
(defcc <rsb>         shen.<rsb>         := shen.<rsb>;)
(defcc <lrb>         shen.<lrb>         := shen.<lrb>;)
(defcc <rrb>         shen.<rrb>         := shen.<rrb>;)
(defcc <bar>         shen.<bar>         := shen.<bar>;)
(defcc <semicolon>   shen.<semicolon>   := shen.<semicolon>;)
(defcc <colon>       shen.<colon>       := shen.<colon>;)
(defcc <comma>       shen.<comma>       := shen.<comma>;)
(defcc <equal>       shen.<equal>       := shen.<equal>;)

(defcc <doc-comment-line>
  <backslash> <backslash> <colon> <space?> <line-remaining>
    := <line-remaining>;)

(defcc <doc-comment-block>
  <doc-comment-line> <newline> <doc-comment-block>
    := [<doc-comment-line> | <doc-comment-block>];
  <doc-comment-line> := [<doc-comment-line>];)

(defcc <doc-comment>
  <doc-comment-block> := <doc-comment-block>;)

(defcc <line-remaining>
  <non-newline> <line-remaining> := (@s <non-newline> <line-remaining>);
  <e> := "";)

(defcc <non-newline>
  C := (n->string C) where (not (or (= C 10) (= C 13)));)

(defcc <space?>
  <space> := skip;
  <e> := skip;)

(defcc <horizontal-whitespace*>
  <space> <horizontal-whitespace*> := skip;
  <e> := skip;)

(defcc <space>
  32 := skip;
  9 := skip;)

(defcc <newline>
  13 10 := skip;
  10 := skip;)

\\ This mirrors Shen 41's raw <s-exprs> grammar, with documentation comments
\\ recognized before ordinary comments are discarded.
(defcc <s-exprs-withdocs>
  <doc-comment> <newline> <horizontal-whitespace*> <newline>
    <s-exprs-withdocs>
      := [["shendoc.standalone" | <doc-comment>] | <s-exprs-withdocs>];
  <doc-comment> <newline> <s-exprs-withdocs>
      := [["shendoc.associate" | <doc-comment>] | <s-exprs-withdocs>];
  <doc-comment> <e>
      := [["shendoc.standalone" | <doc-comment>]];
  <lsb> <s-exprs-withdocs1> <rsb> <s-exprs-withdocs2>
      := [(shen.cons-form <s-exprs-withdocs1>) | <s-exprs-withdocs2>];
  <lrb> <s-exprs-withdocs1> <rrb> <s-exprs-withdocs2>
      := (shen.add-sexpr <s-exprs-withdocs1> <s-exprs-withdocs2>);
  <lcurly> <s-exprs-withdocs>
      := [{ | <s-exprs-withdocs>];
  <rcurly> <s-exprs-withdocs>
      := [} | <s-exprs-withdocs>];
  <bar> <s-exprs-withdocs>
      := [bar! | <s-exprs-withdocs>];
  <semicolon> <s-exprs-withdocs>
      := [(intern ";") | <s-exprs-withdocs>];
  <colon> <equal> <s-exprs-withdocs>
      := [(intern ":=") | <s-exprs-withdocs>];
  <colon> <s-exprs-withdocs>
      := [(intern ":") | <s-exprs-withdocs>];
  <comma> <s-exprs-withdocs>
      := [(intern ",") | <s-exprs-withdocs>];
  <comment> <s-exprs-withdocs>
      := <s-exprs-withdocs>;
  <atom> <s-exprs-withdocs>
      := [<atom> | <s-exprs-withdocs>];
  <whitespaces> <s-exprs-withdocs>
      := <s-exprs-withdocs>;
  <e> := [];)

(defcc <s-exprs-withdocs1>
  <s-exprs-withdocs> := <s-exprs-withdocs>;)

(defcc <s-exprs-withdocs2>
  <s-exprs-withdocs> := <s-exprs-withdocs>;)

(define parse-file
  File -> (let Bytes (read-file-as-bytelist File)
            (trap-error
             (compile (function <s-exprs-withdocs>) Bytes)
             (/. X (shen.reader-error (value shen.*residue*))))))

\\ Flatten package declarations and qualify their contents without expanding
\\ macros. Documentation markers are strings, so package processing leaves
\\ them untouched. External declarations are source metadata, so accept only
\\ literal symbol lists rather than evaluating the documented file.
(define package-externals
  [] -> []
  [cons Symbol Rest]
    -> [Symbol | (package-externals Rest)] where (symbol? Symbol)
  External
    -> (error "unsupported package external declaration: ~R~%" External))

(define flatten-packages
  [] -> []
  [[package null _ | Code] | Rest]
    -> (flatten-packages (append Code Rest))
  [[package P External | Code] | Rest]
    -> (flatten-packages
        (append (shen.package-symbols
                 (str P)
                 (package-externals External)
                 Code)
                Rest))
  [Form | Rest] -> [Form | (flatten-packages Rest)])

(define make-docs
  [] -> []
  [["shendoc.standalone" | Lines] | Rest]
    -> [[standalone | Lines] | (make-docs Rest)]
  [["shendoc.associate" | Lines] [define Name | DefRest] | Rest]
    -> [[func Name (extract-type-signature DefRest) | Lines]
        | (make-docs Rest)]
  [["shendoc.associate" | Lines] [defcc Name | _] | Rest]
    -> [[func Name untyped | Lines] | (make-docs Rest)]
  [["shendoc.associate" | Lines] _ | Rest]
    -> [[standalone | Lines] | (make-docs Rest)]
  [["shendoc.associate" | Lines]]
    -> [[standalone | Lines]]
  [_ | Rest] -> (make-docs Rest))

(define extract-type-signature
  [{ | Rest] -> (extract-type-signature-h Rest)
  _ -> untyped)

(define extract-type-signature-h
  [} | _] -> []
  [X | Rest] -> [X | (extract-type-signature-h Rest)]
  [] -> (error "Unterminated function type signature"))

(define type-signature-string
  [] -> ""
  [T] -> (make-string "~R" T)
  [T | Rest] -> (@s (make-string "~R" T)
                    " "
                    (type-signature-string Rest)))

(define render-lines
  [] -> ""
  [Line | Rest] -> (@s Line (n->string 10) (render-lines Rest)))

(define function-heading
  Name untyped -> (make-string "==== `~A`~%~%" Name)
  Name Type -> (make-string "==== `~A` : `~A`~%~%"
                           Name
                           (type-signature-string Type)))

(define render-docs
  [] -> ""
  [[standalone | Lines] | Rest]
    -> (@s (render-lines Lines) (n->string 10) (render-docs Rest))
  [[func Name Type | Lines] | Rest]
    -> (@s (function-heading Name Type)
           (render-lines Lines)
           (n->string 10)
           (render-docs Rest)))

(define generate
  File -> (render-docs
           (make-docs
            (flatten-packages
             (parse-file File)))))

(define usage
  Script -> (error "usage: ~A INPUT [OUTPUT]~%" Script))

(define main
  [Script Input] -> (do (output "~A" (generate Input)) unit)
  [Script Input Output] -> (do (write-to-file Output (generate Input)) unit)
  [Script | _] -> (usage Script)
  _ -> (usage "shendoc.shen"))

(declare generate [string --> string])
(declare main [[list string] --> unit])

)
