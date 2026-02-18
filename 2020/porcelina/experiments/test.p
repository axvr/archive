;;;; vim: set ft=clojure :


(= := :=)
(def := :=)

(. := :new)

(+ = :+)
(- = :-)
(* = :*)
(/ = :/)
(< = :<)
(> = :>)
(<= = :<=)
(>= = :>=)
(== = :==)
(<> = :<>)


(foo = (String :new [:value "Hello, world!"]))
(foo := (String :new [:value "Hello, world!"]))
(foo :def (String :new [:value "Hello, world!"]))

(def foo (:new String "Hello, world!"))

(Console :write-line foo)
;; Hello, world!

(def MyClass (-> Object
                 :subclass
                 [:extend]))


:do
:doto


(set = (:new Set [1 2 3 4 5 6 7 8 9 0]))
(foo = (:new String "Hello, world!"))

(set = (Set . [1 2 3 4 5 6 7 8 9 0]))
(foo = (String . "Hello, world!"))

(add1 = (Lambda . :args [x] :body [x + 1]))
(add1 :x 2)

(add1 = (Lambda . [[x] x + 1]))
(add1 = (fn . [[x] x + 1]))

(add1 = (fn . [[to] (to + 1)]))
(add1 :to 4)


(r = (range :from 1 :to 100 :step 1))

(range = (-> Iterator
             :subclass
             [:extend ...]
             :new))

;; Implement Clojure's sequence abstraction, first/rest.


Syntax:

  A form is anything wrapped by parenthesis.

  Anything beginning with a colon, is a message.


  (obj :msg)
  (obj :msg arg)
  (obj :msg arg1 :msg-arg2 arg2)

  (:msg obj)            -> (obj :msg)
  (:msg obj1 obj2)      -> (obj1 :msg obj2)
  (:msg obj1 obj2 obj3) -> ((obj1 :msg obj2) :msg obj3)
  ;; (:msg obj1 obj2 obj3) -> (obj1 :msg obj2 :msg obj3)


  Any plain text is a symbol object.  The symbol object defaults to an instance
  of the nothing object, which supports the following methods.
    :=         Become a different object.
    :lock      Disable use of the := method.
    :nothing?  Check if still a :nothing? object.
    :;         Comment (ignored and returns nothing).
  For any other method, the nothing object returns itself: nothing.

  The nothing object is considered to be "falsy".


  Comments are implemented as method calls.
    ; (:; "Hello world")  -> nothing
    ; (:; Hello world)    -> nothing
