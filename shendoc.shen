\\ Copyright (c) 2020 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\** {1 Shen documentation generator}

    Shendoc is a tool to generate documentation from special comments
    embedded in Shen source code files.

    {2 Documentation comment syntax}

    TODO

    {2 Usage}

    TODO
*\

(package shendoc [void]

\\ Needed so that package-macro doesn't alter annotations produced
\\ from comments inside package declarations.
(systemf standalone)
(systemf associate)

\\ Import these from Shen's kernel
(defcc <whitespaces>   shen.<whitespaces>   := shen.<whitespaces>;)
(defcc <comment>       shen.<comment>       := shen.<comment>;)
(defcc <atom>          shen.<atom>          := shen.<atom>;)
(defcc <digit>         shen.<digit>         := shen.<digit>;)
(defcc <backslash>     shen.<backslash>     := shen.<backslash>;)
(defcc <lcurly>        shen.<lcurly>        := shen.<lcurly>;)
(defcc <rcurly>        shen.<rcurly>        := shen.<rcurly>;)
(defcc <lsb>           shen.<lsb>           := shen.<lsb>;)
(defcc <rsb>           shen.<rsb>           := shen.<rsb>;)
(defcc <lrb>           shen.<lrb>           := shen.<lrb>;)
(defcc <rrb>           shen.<rrb>           := shen.<rrb>;)
(defcc <colon>         shen.<colon>         := shen.<colon>;)
(defcc <shen-code>     shen.<st_input>      := shen.<st_input>;)

(defcc <doc-comment-line>
  <backslash> <backslash> <colon> <space?> <line-remaining> := <line-remaining>;)

(defcc <doc-comment-block>
  <doc-comment-line> <newline> <doc-comment-block> := [<doc-comment-line> | <doc-comment-block>];
  <doc-comment-line> := [<doc-comment-line>];)

(defcc <doc-comment>
  <doc-comment-block> := <doc-comment-block>;)

(defcc <line-remaining>
  <non-newline> <line-remaining> := (@s <non-newline> <line-remaining>);
  <e> := "";)

(defcc <non-newline>
  C := (n->string C) where (not (or (= C 10) (= C 13)));)

(defcc <whitespace*>
  <whitespaces> := skip;
  <e>           := skip;)

(defcc <space?>
  <space> := skip;
  <e> := skip;)

(defcc <space>
  32 := skip;
  9 := skip;)

(defcc <newline>
  13 10 := skip;  \\ CRLF
  10    := skip;) \\ LF

(defcc <st_input-withdocs>
  <doc-comment> <newline> <newline> <st_input-withdocs>
      := [[standalone | <doc-comment>] | <st_input-withdocs>];
  <doc-comment> <newline> <st_input-withdocs>
      := [[associate | <doc-comment>] | <st_input-withdocs>];
  <lsb> <st_input-withdocs1> <rsb> <st_input-withdocs2>
      := [(macroexpand (shen.cons_form <st_input-withdocs1>)) | <st_input-withdocs2>];
  <lrb> <st_input-withdocs1> <rrb> <st_input-withdocs2>
      := (shen.package-macro (macroexpand <st_input-withdocs1>) <st_input-withdocs2>);
  <comment> <st_input-withdocs>
      := <st_input-withdocs>;
  <atom> <st_input-withdocs>
      := [(macroexpand <atom>) | <st_input-withdocs>];
  <whitespaces> <st_input-withdocs>
      := <st_input-withdocs>;
  <shen-code> := <shen-code>;
  <e> := [];)

(defcc <st_input-withdocs1>
  <st_input-withdocs> := <st_input-withdocs>;)

(defcc <st_input-withdocs2>
  <st_input-withdocs> := <st_input-withdocs>;)

(define make-docs
  [] -> []
  [[standalone | Lines] | Rest]
     -> [[standalone | Lines] | (make-docs Rest)]
  [[associate | Lines] [define Name | DefRest] | Rest]
    -> [[func Name (extract-type-signature DefRest) | Lines]
        | (make-docs Rest)]
  [[associate | Lines] [defcc Name | DefRest] | Rest]
    -> [[func Name untyped | Lines]
        | (make-docs Rest)]
  [[associate | Lines] _ | Rest]
    -> (make-docs [[standalone | Lines] | Rest])
  [_ | Rest] -> (do _ (make-docs Rest)))

(define extract-type-signature
  [{ | Rest] -> (extract-type-signature-h Rest)
  _ -> untyped)

(define extract-type-signature-h
  [} | Rest] -> []
  [X | Rest] -> [X | (extract-type-signature-h Rest)])

(define type-signature-string
  [T] -> (make-string "~R" T)
  [T | Rest] -> (@s (make-string "~R" T) " " (type-signature-string Rest)))

(define render-doc
  [[standalone | Lines] | Rest]
    -> (do (for-each (/. Line (output "~A~%" Line)) Lines)
           (nl)
           (render-doc Rest))
  [[func Name Type | Lines] | Rest]
    -> (do (if (= Type untyped)
               (output "==== `~A`~%~%" Name)
               (output "==== `~A` : `~A`~%~%" Name (type-signature-string Type)))
           (for-each (/. Line (output "~A~%" Line)) Lines)
           (nl)
           (render-doc Rest))
  [] -> void)

(define for-each
  F [] -> void
  F [X | Rest] -> (do (F X) (for-each F Rest)))

(define without-macros
  Expr -> (let Macros (value *macros*)
               _ (set *macros* [])
               Result (thaw Expr)
               _ (set *macros* Macros)
            Result))

(define main
  [Exe Input Output] -> (main [Exe Input]) \\ TODO
  [Exe Input] -> (let Bytes (read-file-as-bytelist Input)
                      Parsed (without-macros (freeze (compile (function <st_input-withdocs>) Bytes)))
                      Docs (make-docs Parsed)
                   (render-doc Docs))
  [Exe | Other] -> (print-usage Exe))

)

(shendoc.main (value *argv*))
