\\ Copyright (c) 2026 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Sequence/dictionary conversions
\\:
\\: Requiring `seq/dict` adds explicit conversions between lazy sequences and
\\: mutable Shen dictionaries without widening the input types accepted by
\\: `seq.of`. Require it with `(library.use [seq/dict])`. Dictionary snapshots
\\: are captured when a conversion is called, so later mutations do not change
\\: the returned sequence. Independently captured key and value sequences must
\\: not be assumed to align; use `seq.of-dict-entries` for associations.

\\: `(seq.of-dict-keys Dict)` returns a sequence over a snapshot of the keys in
\\: `Dict`. Dictionary traversal order is unspecified and may vary between Shen
\\: implementations.
(define seq.of-dict-keys
  { (dict.t K V) --> (seq.t K) }
  Dict -> (seq.of-list (dict.keys Dict)))

\\: `(seq.of-dict-values Dict)` returns a sequence over a snapshot of the values
\\: in `Dict`. Dictionary traversal order is unspecified and may vary between
\\: Shen implementations.
(define seq.of-dict-values
  { (dict.t K V) --> (seq.t V) }
  Dict -> (seq.of-list (dict.values Dict)))

\\: `(seq.of-dict-entries Dict)` returns a sequence over a snapshot of the
\\: associations in `Dict`, represented as `(@p Key Value)` tuples. Dictionary
\\: traversal order is unspecified and may vary between Shen implementations.
(define seq.of-dict-entries
  { (dict.t K V) --> (seq.t (K * V)) }
  Dict -> (seq.of-list
            (map (/. Key (@p Key (dict.get Dict Key)))
                 (dict.keys Dict))))

\\: `(seq.to-dict Entries)` consumes a finite sequence of `(@p Key Value)`
\\: tuples and returns a dictionary containing those associations. If a key
\\: occurs more than once, the value from its last occurrence wins. It does not
\\: terminate for an infinite sequence.
(define seq.to-dict
  { (seq.t (K * V)) --> (dict.t K V) }
  Entries -> (seq.fold-left
               (/. Dict Entry
                 (do (dict.set Dict (fst Entry) (snd Entry))
                     Dict))
               (dict.make 1)
               Entries))
