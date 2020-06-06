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

(package shendoc []

\\ Needed so that package-macro doesn't alter annotations produced
\\ from comments inside package declarations.
(systemf standalone)
(systemf associate)
(systemf text)
(systemf header)
(systemf code)

\\ Import these from Shen's kernel
(defcc <whitespaces>   shen.<whitespaces>   := shen.<whitespaces>;)
(defcc <comment>       shen.<comment>       := shen.<comment>;)
(defcc <atom>          shen.<atom>          := shen.<atom>;)
(defcc <digit>         shen.<digit>         := shen.<digit>;)
(defcc <times>         shen.<times>         := shen.<times>;)
(defcc <backslash>     shen.<backslash>     := shen.<backslash>;)
(defcc <lcurly>        shen.<lcurly>        := shen.<lcurly>;)
(defcc <rcurly>        shen.<rcurly>        := shen.<rcurly>;)
(defcc <lsb>           shen.<lsb>           := shen.<lsb>;)
(defcc <rsb>           shen.<rsb>           := shen.<rsb>;)
(defcc <lrb>           shen.<lrb>           := shen.<lrb>;)
(defcc <rrb>           shen.<rrb>           := shen.<rrb>;)
(defcc <shen-code>     shen.<st_input>      := shen.<st_input>;)

(defcc <whitespaces?>
  <whitespaces> := skip;
  <e> := skip;)

(defcc <newline>
  13 10 := skip;  \\ CRLF
  10 := skip;)    \\ LF

(defcc <doc-comment>
   <backslash> <times> <times> <whitespaces?> <doc-contents> := (parse-comment-block <doc-contents>);)

(defcc <doc-contents>
   <doc-comment-end>  := [];
   C <doc-contents>   := [C | <doc-contents>];)

(defcc <doc-comment-end>
  <whitespaces?> <times> <backslash> := skip)

(defcc <header>
  <lcurly> <digit> <whitespaces> <headercontents> <rcurly> := [header <digit> | <headercontents>];)

(defcc <code>
  <lsb> <code-text> <rsb> := [code <code-text>];)

(defcc <code-text>
  <shen-code> := <shen-code>;)

(defcc <escaped-text>
  <whitespaces> <escaped-char*> := (@s " " <escaped-char*>);
  <escaped-char> <escaped-char*> := (@s <escaped-char> <escaped-char*>);)

(defcc <escaped-char*>
  <whitespaces> <escaped-char*>  := (@s " " <escaped-char*>);
  <escaped-char> <escaped-char*> := (@s <escaped-char> <escaped-char*>);
  <e>                            := "";)

(defcc <escaped-char>
  92 92 := "\";                 \\ "\\"
  <backslash> <rcurly> := "}";  \\ "\}"
  <backslash> <lcurly> := "{";  \\ "\{"
  <backslash> <rsb>    := "]";  \\ "\["
  <backslash> <lsb>    := "[";  \\ "\]"
  C                    := (n->string C)
      where (not (element? C [91 93 123 125]));)

(defcc <doctext>
  <whitespaces?> <header> <doctext> := [<header> | <doctext>];
  <whitespaces?> <code> <doctext>   := [<code> | <doctext>];
  <escaped-text> <doctext>          := [[text <escaped-text>] | <doctext>];
  <e>                               := [];)

(defcc <headercontents>
  <whitespaces?> <code> <headercontents> := [<code> | <headercontents>];
  <escaped-text> <headercontents>        := [[text <escaped-text>] | <headercontents>];
  <e>                                    := [];)

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

(define parse-comment-block
  S -> (compile (function <doctext>) S))

(define make-docs
  [] -> []
  [[standalone | Fragments] | Rest] -> [[standalone | (process-fragments Fragments)] | (make-docs Rest)]
  [[associate | Fragments] [define Name | DefRest] | Rest]
    -> [[func Name (extract-type-signature DefRest) | (process-function-doc Name Fragments)]
        | (make-docs Rest)]
  [[associate | Fragments] [defcc Name | DefRest] | Rest]
    -> [[func Name untyped | (process-function-doc Name Fragments)]
        | (make-docs Rest)]
  [[associate | Fragments] _ | Rest]
    -> (make-docs [[standalone | (process-fragments Fragments)] | Rest])
  [_ | Rest] -> (do _ (make-docs Rest)))

(define process-fragments
  [] -> []
  [Fragment | Rest] -> [(process-fragment Fragment) | (process-fragments Rest)])

(define process-fragment
  [header | Rest] -> [header | (map (function process-fragment) Rest)]
  X -> X)

(define process-function-doc
  Name Fragments -> (process-fragments Fragments))

(define extract-type-signature
  [{ | Rest] -> (extract-type-signature-h Rest)
  _ -> untyped)

(define extract-type-signature-h
  [} | Rest] -> []
  [X | Rest] -> [X | (extract-type-signature-h Rest)])

(define type-signature-string
  [T] -> (make-string "~R" T)
  [T | Rest] -> (@s (make-string "~R" T) " " (type-signature-string Rest)))

(define render-docs-as-markdown
  [[standalone | Fragments] | Rest] -> (do (for-each (function render-docs-as-markdown-h) Fragments)
                                           (output "~%~%")
                                           (render-docs-as-markdown Rest))
  [[func Name untyped | Fragments] | Rest] -> (do (render-docs-as-markdown-h [header 6 [text Name]])
                                                  (output "~%")
                                                  (for-each (function render-docs-as-markdown-h) Fragments)
                                                  (output "~%")
                                                  (render-docs-as-markdown Rest))
  [[func Name Type | Fragments] | Rest] -> (do (render-docs-as-markdown-h [header 4 [text Name]])
                                               (output "**Type**: ")
                                               (render-docs-as-markdown-h [code Type])
                                               (output "~%~%")
                                               (for-each (function render-docs-as-markdown-h) Fragments)
                                               (output "~%~%")
                                               (render-docs-as-markdown Rest))
  [] -> unit)

(define render-docs-as-markdown-h
  [header N | Fragments] -> (do (output "~A " (times "#" N))
                                (for-each (function render-docs-as-markdown-h) Fragments)
                                (output "~%"))
  [text Text] -> (output "~A" Text)
  [code Code] -> (output "`~A`" (type-signature-string Code)))

(define times
  S 0 -> ""
  S N -> (@s S (times S (- N 1))))

(define for-each
  F [] -> unit
  F [X | Rest] -> (do (F X) (for-each F Rest)))

(define main
  [Exe Input Output] -> (main [Exe Input])
  [Exe Input] -> (let Bytes (read-file-as-bytelist Input)
                      Parsed (compile (function <st_input-withdocs>) Bytes)
                      Docs (make-docs Parsed)
                    (render-docs-as-markdown Docs))
  [Exe | Other] -> (print-usage Exe))

)

(shendoc.main (value *argv*))
