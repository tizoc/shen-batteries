\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = `typ/unit.t`
\\:
\\: `unit` is a type inhabited by every value. It is useful
\\: when writting functions where the type of the inputs is not known, or
\\: when the result is discarded. Use carefully.

(datatype typ/unit.t
  __________
  X : (mode unit -);)
