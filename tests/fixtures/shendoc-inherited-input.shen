\\: = Inherited Fixture

(package inherited [public | (external lazy)]

\\: Documents a private function using an inherited external type.
(define private
  { (box.t A) --> A }
  B -> (box.unbox B))

\\: Documents a public function.
(define public
  -> ok)

)
