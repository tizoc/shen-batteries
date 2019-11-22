\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package lazy [mfreeze box.make box.unbox box.put]

(datatype memo-internal
  _____________
  #not-thawed#7907# : A;)

(define memo
  { (lazy A) --> (lazy A) }
  L -> (let Result (box.make #not-thawed#7907#)
         (freeze
          (let X (box.unbox Result)
            (if (= #not-thawed#7907# X)
                (let Thawed (thaw L)
                     Update (box.put Result Thawed)
                  Thawed)
                X)))))

(defmacro mfreeze-macro
  [mfreeze Exp] -> [memo [freeze Exp]])

(preclude [memo-internal])
(undefmacro mfreeze-macro)

)