(defrecord test-record-address
  city : string;
  postal : number;)

(defrecord test-record-person
  name : string;
  age : number;
  address : test-record-address;)

(defrecord test-record-person-shadow
  name : string;
  age : number;
  address : test-record-address;)

(defrecord (test-record-box A)
  value : A;
  label : symbol;)

(defrecord test-record-marker)

(defrecord (test-record-phantom A)
  label : symbol;)

(package test-record-model [defrecord]

(defrecord person
  name : string;
  age : number;)

)
