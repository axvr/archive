# Porcelina

Syntax must be as consistent and regular as possible.

Applicative order evaluation.

[Examples](examples.md).


Clojure-like s-expressions, but rather than data structures, they act as
membranes and different parens mean invoke/evaluation order?

Cells > tissues > programs.

Start this again?  This is too focused on the class model and Smalltalk's way
of doing things.  Need to move closer to actors and MOP.  True async messages (no response?).


## Syntax

```
(account :deposit 5)
(account :withdraw 3)
 ^ object  ^ message

account deposit: 5.
account withdraw: 3.


[a b c]
```


### Keywords

Keywords begin with a colon.  These are used as messages.  Keywords evaluate to
themselves.

    :keyword


### Symbols

Just text.

    foo
    foo-bar

Free (variables) symbols are objects and have a := message for assignment.

    (foo := "Hi")
    (forty-two := 42)

    (= := :=)     ; Assign a symbol to the assignment message.
    (bar = "Hi")  ; Use new assignment syntax (the `=` is expanded to `:=`).

No built-in classes and singletons have a := message.  But one can be added.
This makes variables immutable (they are obviously scoped).


### Numbers

Numbers are singletons, which evaluate to themselves.


### Booleans

Booleans are singleton objects which evaluate to themselves.

    true
    false


### Strings

Strings are enclosed in double quotes.

    "Hello, world!"


### Characters

Character literals are denoted with a backslash followed by the Unicode code
point value or the actual value encoded in UTF-8.  (It may be possible to
implement custom encodings?)

    \a
    \2
    \£
    \u+2014  ; em-dash: same as \—


### Comments

Comments are started with a semi-colon, the comment expands to the end of the
line.

    ; This is a comment.
    (account withdraw: 5) ; This is also a comment.


### Message sending

    ; These could be predefined early on in the environment.  (Probably can't
    ; be locally scoped.)
    (def + :add)  ; Need better assignment syntax.
    (def - :subtract)
    (def * :multiply)
    (def / :divide)

    (3 + 5)  ; This way + can be a message, as it is expanded into it's keyword form.


Translating pre-fix notation to message syntax (message-chaining).  (Possible syntax?)

    (+ 3 5)
    => (3 + 5)
    => (3 :add 5)

    (+ 3 4 5)
    => ((3 + 4) + 5)
    => ((3 :add 4) :add 5)


### Namespacing



### Assignment

    (+ := :+)  ; Need better assignment syntax.
    (- := :-)
    (* := :*)
    (/ : =:/)
    (! := :factorial)

    (foo := "Hello, world")
    (= := :=)  ; Create nicer message syntax for assignment.
    (foo = "Bar")

    foo := @object :new.

    ; C#
    var foo = new Object();
    ; Python
    foo = object()

When assigning a variable to an object, it captures its state at that moment.
Mutation to either won't affect the other as they aren't the same object.
Objects must be referenced by their true name?


### Data types

#### Arrays

This is syntax which creates an array object.  Array objects use 1-based
indexing.  Arrays exist only to act as the internals of an object or as glue
code.

    [1 2 3]
    [:foo :bar 42]

    ([1 2 3] :first) => 1
    ([1 2 3] :item 2) => 2
    ([1 2 3] :append 4) => [1 2 3 4]

    [1 2 3 4 5 6]
    => (:append (porcelina/array :init) 1 2 3 4 5 6)
       => (((((((porcelina/array :init) :append 1) :append 2) :append 3) :append 4) :append 5) :append 6)
    => ((porcelina/array :init) :append 1 :then 2 :then 3 :then 4 :then 5 :then 6)

    (<- := :append)
    ([1 2 3] <- 4)

    (<- := :append)
    (nums := [1 2 3])
    (nums <- 4)


#### Dictionaries

Maybe I don't need dictionary syntax.

This is syntax which creates dictionary objects.  Dictionary syntax exists only
for object internals or glue code.

    {:foo 1, :bar 2, :biz "hi"}

    (porcelina/dict :init [:foo 1]
                          [:bar 2]
                          [:biz "hi"])


### Conditionals

Boolean objects contain :if-true and :if-false methods.

    ((1 < 4) :if-true (lambda :args [x] :body )
             :if-false  )




### Objects

(operation = ((foo > 3)
                :if-true +
                :if-false -))

(operation = ((foo > 3) :if-true +
                        :if-false -))
(1 operation 2)


(+ 1 2)
1 + 2.
(1 + 2) => (1 :+ 2)


(class/true := (class/object :subclass))
(class/true :method ...)
(class/true :method ...)
; (:method class/true ... ...)
(class/true :singleton)
(true := (class/true :init))

Convention that classes begin with `@`.


### Lambda

Lambda is an object.


:become
:is


## Classes

@class
@object
@nothing

foo := @object :new.
nothing := @nothing :new.

@class :new

@nothing := (@class :new, :method {:null? | | true}).

Seal classes to avoid unwanted changes propagating.


## Mutation

If keyword starts (instead of `:`) or ends with an exclamation mark, mutate.


## The "nothing" object

The nothing object is the unassigned object (it is a constant).  It takes
a method which handles assignment, rather than being done by the VM.  (Possible
**new** programming language theory concept.)

> foo
nothing
> foo :null?.
true
> foo := 5.
> foo.
5
> foo :null?.
false

The nothing object can also be returned when attempting to retrieve
a non-existent object from an array object.

> foo := [1 :bar "hi"].
> foo :get 2
:bar
> foo :get 4
nothing

You can technically still do an assignment on an anonymous nothing object, in
which case it will just replace the anonymous object.

The symbol acts as a pointer to the `nothing` object.  When the object mutates
the symbol/reference used to access it will have the pointer updated to the new
object.
