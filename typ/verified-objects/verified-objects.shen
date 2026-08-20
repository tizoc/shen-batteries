\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = `typ/verified-objects.t`
\\:
\\: `typ/verified-objects` is an opt-in typechecker extension. Load it before
\\: typechecking definitions that use its refinements:
\\:
\\: [source,shen]
\\: ----
\\: (library.use [typ/verified-objects])
\\: ----
\\:
\\: The theory adds no runtime checks, conversions, or values. It teaches the
\\: typechecker what follows when one of these predicate calls is already a
\\: `verified` hypothesis:
\\:
\\: * `(number? X)` being `verified` permits `X : number`.
\\: * `(string? X)` being `verified` permits `X : string`.
\\: * `(symbol? X)` being `verified` permits `X : symbol`.
\\: * `(boolean? X)` being `verified` permits `X : boolean`.
\\:
\\: Shen makes a successful `where` guard available as a `verified` hypothesis
\\: while typechecking the guarded expression. For example, after loading this
\\: module the following definition typechecks even though `X` initially has an
\\: unknown type:
\\:
\\: [source,shen]
\\: ----
\\: (define increment-number
\\:   { A --> number }
\\:   X -> (+ X 1) where (number? X)
\\:   _ -> 0)
\\: ----
\\:
\\: The inference is directional. Knowing that `X : number` does not establish
\\: `(number? X) : verified`, and a predicate which is false does not establish
\\: a complementary type.
\\:
\\: == Compound `and` guards
\\:
\\: When `(and Q R)` is already a `verified` hypothesis, the compound rule makes
\\: `Q : verified` and `R : verified` available while deriving consequences from
\\: that hypothesis. This lets a guarded expression use refinements contributed
\\: by either conjunct.
\\:
\\: The compound rule does not help typecheck `R` while `(and Q R)` itself is
\\: being checked as a boolean condition. Load `typ/verified-and-head` as well
\\: when the verified head must refine the tail of an `and` expression. These
\\: rules cover only the four predicates above and binary `and`; other predicates
\\: and boolean forms need their own type rules.

(datatype typ/verified-objects.t
  ______________________
  (number? X) : verified >> X : number;

  ______________________
  (string? X) : verified >> X : string;

  ______________________
  (symbol? X) : verified >> X : symbol;

  ______________________
  (boolean? X) : verified >> X : boolean;

  Q : verified, R : verified >> P;
  ______________________
  (and Q R) : verified >> P;)
