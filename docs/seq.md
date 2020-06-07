# Lazy sequences

The type `(seq.t A)` represents a delayed sequence of values of type `A`. Evaluation of each element in the sequence is delayed until the element is accessed when the sequence is traversed. The results of sequence transformations are also delayed, and no evaluation will happen until the sequence produced by the transformation is traversed.

## Dependencies

Before loading `seq.shen`, the following files need to be loaded with `(tc +)` enabled: `t.shen`, `maybe.shen`, `box.shen` and `lazy.shen`.

## API

### Creation

#### seq.empty

**Type**: `--> (seq.t A)`

`(seq.empty)` produces the empty sequence.

#### seq.singleton

**Type**: `A --> (seq.t A)`

`(seq.singleton X)` produces a singleton sequence containing just `X`.

#### seq.memo

**Type**: `(seq.t A) --> (seq.t A)`

`(seq.memo Seq)` wraps `Seq` in a memoized sequence for which elements are evaluated only once the first time the sequence is traversed.

#### seq.cons

**Type**: `A --> (seq.t A) --> (seq.t A)`

`(seq.cons V S)` produces a new sequence with `V` as the first element, followed by everything contained in sequence `S`.

#### seq.snoc

**Type**: `(seq.t A) --> A --> (seq.t A)`

`(seq.snoc S V)` produces a new sequence with everything in sequence `S`, followed by `V`.

#### seq.make

**Type**: `number --> A --> (seq.t A)`

`(seq.make N Elt)` produces a sequence containing `N` times the value of `Elt`.

#### seq.unfold

**Type**: `(B --> (maybe.t (A * B))) --> B --> (seq.t A)`

`(seq.unfold F Seed)` produces a sequence of values constructed from the results of `(F Seed)`. `F` must return `(@none)` to signal the end of the production of elements, and `(@some Elt NewSeed)` to produce a new sequence element `Elt`. `NewSeed` will be passed to `F` the next time a sequence element has to be produced.

#### seq.range-step

**Type**: `number --> number --> number --> (seq.t number)`

`(seq.range-step Step Start End)` produces a sequence containing all integers in the range (inclusive) from `Start` to `End` and separated by `Step`. If `Step` is a negative number, a sequence of decreasing values is produced.

#### seq.range

**Type**: `number --> number --> (seq.t number)`

`(seq.range Start End)` produces a sequence containing all integers in the range (inclusive) from `Start` to `End`. If `Start` is greater than `End`, a sequence of decreasing values is produced.

#### seq.of

**Type**: `(or (list A) (vector A)) --> (seq.t A)`

`(seq.of SeqLike)` produces a sequence containing all elements in `SeqLike`, where `SeqLike` is either a list, or a vector.

#### seq.of-list

**Type**: `(list A) --> (seq.t A)`

`(seq.of-list List)` produces a sequence containing all elements in the list `List`.

#### seq.of-vector

**Type**: `(vector A) --> (seq.t A)`

`(seq.of-vector Vector)` produces a sequence containing all elements in the vector `Vector`.

#### seq.of-vector-reversed

**Type**: `(vector A) --> (seq.t A)`

`(seq.of-vector Vector)` produces a sequence containing all elements in the vector `Vector` in reverse order.

#### seq.of-string

**Type**: `string --> (seq.t string)`

`(seq.of-string String)` produces a sequence of unit strings, each one being a character in the string `String`.

#### seq.to-list

**Type**: `(seq.t A) --> (list A)`

`(seq.to-list Seq)` constructs a list containing every element produced by the sequence `Seq`.

#### seq.into-vector

**Type**: `number --> number --> (vector A) --> (seq.t A) --> ((seq.t A) * number)`

`(seq.into-vector Start Count Vector Seq)` fills the vector `Vector` with elements produced by the sequence `Seq`. If `Count` is positive, the vector slots from `Start` to `Start + Count - 1` are filled in increasing order. If `Count` is negative, the vector slots from `Start` to `Start - seq.abs (Count) + 1` are filled in decreasing order. The return value is a `(@p RemainingSeq NotFilledCount)` tuple, with `RemainingSeq` being what is left to be consumed of the sequence, and `NotFilledCount` the amount of slots that couldn't be filled because `Seq` got fully consumed before `Count` amount of slots were filled. If `NotFilledCount` is `0`, that means that the operation succeeded without the `Seq` sequence ending prematurely.

#### seq.to-string

**Type**: `(seq.t string) --> string`

`(seq.to-string Seq)` constructs a list that is the concatenation of every string produced by the sequence `Seq`.

#### seq.forever

**Type**: `(lazy A) --> (seq.t A)`

`(seq.forever Frozen)` produces an infinite sequence that always produces values that are the result of `(thaw Frozen)`.

### Predicates

#### seq.empty?

**Type**: `(seq.t A) --> boolean`

`(seq.empty? Seq)` will return `true` if `Seq` is an empty sequence, `false` otherwise. Note that performing this check will cause the evaluation of the first element of `Seq` if it is not empty.

### Accessors

#### seq.head

**Type**: `(seq.t A) --> A`

`(seq.head Seq)` evaluates and returns the first element of `Seq`.

#### seq.tail

**Type**: `(seq.t A) --> (seq.t A)`

`(seq.tail Seq)` returns `Seq` without the first element. Note that this will cause the evaulation of the first element of `Seq`.

### Consumption

#### seq.drain

**Type**: `(seq.t A) --> unit`

`(seq.drain Seq)` consumes `Seq` until no more elements are left. The produced elements will be discarded.

#### seq.fold-left

**Type**: `(A --> B --> A) --> A --> (seq.t B) --> A`

`(seq.fold-left F Acc Seq)` consumes the sequence `Seq`, combining each of it's elements with the accumulator `Acc` by calling `(F Acc Elt)`. The result of each call to `F` is a new `NewAcc` value that will be passed as argument to the next call to `F` along with the next element in the `Seq` sequence: `(F NewAcc NextElt)`. The return value of `seq.fold-left` is the result of the final call to `F` when the sequence is fully consumed.

#### seq.for-each

**Type**: `(A --> Any) --> (seq.t A) --> unit`

`(seq.for-each F Seq)` consumes `Seq` until no more elements are left. Each value produced is passed to `F` and the result discarded.

#### seq.equal?

**Type**: `(seq.t A) --> (seq.t A) --> boolean`

`(seq.equal? SeqA SeqB)` consumes `SeqA` and `SeqB` one element at a time comparing the elements with `(= EltA EltB)` until `false` is returned or one of the sequences is fully consumed. The result is `true` if `false` is never returned and both sequences produce the samee amount of values

#### seq.equal-cmp?

**Type**: `(A --> B --> boolean) --> (seq.t A) --> (seq.t B) --> boolean`

`(seq.equal-cmp? Cmp SeqA SeqB)` consumes `SeqA` and `SeqB` one element at a time comparing the elements with `(Cmp EltA EltB)` until `false` is returned or one of the sequences is fully consumed. The result is `true` if `false` is never returned and both sequences produce the samee amount of values

#### seq.for-all?

**Type**: `(A --> boolean) --> (seq.t A) --> boolean`

`(seq.for-all? Test Seq)` return `true` if `(Test Elt)` is `true` for every value produced the traversal of `Seq`.

#### seq.exists?

**Type**: `(A --> boolean) --> (seq.t A) --> boolean`

`(seq.exists? Test Seq)` return `true` if `(Test Elt)` is `true` for any value produced by the traversal of `Seq`. `Seq` is only consumed until `(Test Elt)` is true.

#### seq.element?

**Type**: `A --> (seq.t A) --> boolean`

`(seq.element? X Seq)` is equivalent to `(seq.exists? (= X) Seq)`.

#### seq.element-cmp?

**Type**: `(A --> B --> boolean) --> A --> (seq.t B) --> boolean`

`(seq.element? Cmp X Seq)` is equivalent to `(seq.exists? (Cmp X) Seq)`.

#### seq.find

**Type**: `(A --> boolean) --> (seq.t A) --> (maybe.t A)`

`(seq.find Test Seq)` returns the first element in `Seq` for which `(Test Elt)` is true wrapped as `(@some Elt)` or `@none` otherwise.

#### seq.find-map

**Type**: `(A --> (maybe.t B)) --> (seq.t A) --> (maybe.t B)`

`(seq.find-map F Seq)` returns the first result of `(F Elt)` of the form `(@some Result)` or `(@none)` if there is no such result.

### Transformation

#### seq.map

**Type**: `(A --> B) --> (seq.t A) --> (seq.t B)`

`(seq.map F Seq)` returns a new sequence with all the elements in `Seq` transformed with `(F Elt)`.

#### seq.filter

**Type**: `(A --> boolean) --> (seq.t A) --> (seq.t A)`

`(seq.filter Test Seq)` returns a new sequence with all the elements in `Seq` for which `(Test Elt)` is `false` removed.

#### seq.filter-map

**Type**: `(A --> (maybe.t B)) --> (seq.t A) --> (seq.t B)`

`(seq.filter F Seq)` returns a new sequence with all the elements in `Seq` for which `(F Elt)` is `(@none)` removed, and for which the result is `(@some NewElt)` replaced by `NewElt`.

#### seq.flat-map

**Type**: `(A --> (seq.t B)) --> (seq.t A) --> (seq.t B)`

`(seq.flat-map F Seq)` returns a sequence that is the concatenation of all subsequences produced by calling `(F Elt)` for each element in `Seq`.

#### seq.append

**Type**: `(seq.t A) --> (seq.t A) --> (seq.t A)`

`(seq.append SeqA SeqB)` returns a sequence that produces all elements in `SeqA` followed by all elements in `SeqB`.

#### seq.concat

**Type**: `(list (seq.t A)) --> (seq.t A)`

`(seq.concat (cons Seq1 (cons Seq2 (cons seq.... (cons SeqN ())))))` returns a sequence that produces all elements in `Seq1` followed by all elements in `Seq2`, .... followed by all emenets in `SeqN`.

#### seq.flatten

**Type**: `(seq.t (seq.t A)) --> (seq.t A)`

`(seq.flatten SeqOfSeqs)` returns a sequence that produces every element produced by each subsequence produced by `SeqOfSeqs`.

#### seq.cycle

**Type**: `(seq.t A) --> (seq.t A)`

`(seq.cycle Seq)` returns an infinite sequence that produces all the elements from `Seq` repeated from the beginning each time the end of the original sequence is reached.

#### seq.truncate

**Type**: `number --> (seq.t A) --> (seq.t A)`

`(seq.truncate N Seq)` returns a sequence containing at most the first `N` elements of `Seq`.

#### seq.take

**Type**: `number --> (seq.t A) --> (seq.t A)`

`(seq.take N Seq)` returns a sequence containing the first `N` elements of `Seq`. An error will be thrown when traversing the new sequence if the original sequence had fewer than `N` elements.

#### seq.drop

**Type**: `number --> (seq.t A) --> (seq.t A)`

`(seq.drop N Seq)` returns a sequence with the elements from `Seq` with the first `N` elements removed. An error will be thrown when traversing the new sequence if the original sequence had fewer than `N` elements.

#### seq.take-while

**Type**: `(A --> boolean) --> (seq.t A) --> (seq.t A)`

`(seq.take-while Test Seq)` returns a sequence containing elements of `Seq` for which `(Test Elt)` is `true` until `false` is returned or the sequence ends.

#### seq.drop-while

**Type**: `(A --> boolean) --> (seq.t A) --> (seq.t A)`

`(seq.drop-while Test Seq)` returns a sequence containing elements of `Seq` skipping all elements for which `(Test Elt)` is `true` until `false` is returned or the sequence ends.

#### seq.zip-with

**Type**: `(A --> B --> C) --> (seq.t A) --> (seq.t B) --> (seq.t C)`

`(seq.zip-with Cons SeqA SeqB)` returns a new sequence containing elements that are the result of calling `(Cons EltA EltB)` for each element produced by the parallel traversal of `SeqA` and `SeqB`. The produced sequence is as long as the shortest of the input sequences.

#### seq.zip

**Type**: `(seq.t A) --> (seq.t B) --> (seq.t (A * B))`

`(seq.zip SeqA SeqB)` is equivalent to `(seq.zip-with (/. A B (@p A B)) SeqA SeqB)`.

#### seq.unzip

**Type**: `(seq.t (A * B)) --> ((seq.t A) * (seq.t B))`

`(seq.unzip SeqA*B)` returns `(@p SeqA SeqB)`, where `SeqA*B` is a sequence of tuples `(@p A B)`, `SeqA` is `(seq.map (function fst) SeqA*B)` and `SeqB` is `(seq.map (function snd) SeqA*B)`.

#### seq.chunks

**Type**: `number --> (seq.t A) --> (seq.t (vector A))`

`(seq.chunks N Seq)` returns a sequence of vectors of size `N`, with each vector filled with the elements obtained from taking `N` elements from `Seq`. The last vector may have a size smaller than `N` if the sequence ends before it can be fully filled.

