; vim: set ft=lisp :

=========================================================================================================

```
Transcript clear.                                           "clear to transcript window"
Transcript show: 'Hello World'.                             "output string in transcript window"
Transcript nextPutAll: 'Hello World'.                       "output string in transcript window"
Transcript nextPut: $A.                                     "output character in transcript window"
Transcript space.                                           "output space character in transcript window"
Transcript tab.                                             "output tab character in transcript window"
Transcript cr.                                              "carriage return / linefeed"
'Hello' printOn: Transcript.                                "append print string into the window"
'Hello' storeOn: Transcript.                                "append store string into the window"
Transcript endEntry.                                        "flush the output buffer"
```

---------------------------------------------------------------------------------------------------------

```
Transcript :clear.                                          ; clear to transcript window
Transcript :show "Hello World".                             ; output string in transcript window
Transcript :next-put-all "Hello World".                     ; output string in transcript window
Transcript :next-put \A.                                    ; output character in transcript window
Transcript :space.                                          ; output space character in transcript window
Transcript :tab.                                            ; output tab character in transcript window
Transcript :cr.                                             ; carriage return / linefeed
"Hello" :print-on Transcript.                               ; append print string into the window
"Hello" :store-on Transcript.                               ; append store string into the window
Transcript :end-entry.                                      ; flush the output buffer
```

=========================================================================================================

```
| x y |
x := 5.                                                     "assignment"
x := y := z := 6.                                           "compound assignment"
x := (y := 6) + 1.
"x _ 4."                                                    "older assignment style, Squeak used to repurpose the underscore to a left-arrow glyph <-"
x := Object new.                                            "bind to allocated instance of a class"
x := 123 class.                                             "discover the object class"
x := Integer superclass.                                    "discover the superclass of a class"
x := Object allInstances.                                   "get an array of all instances of a class"
x := Integer allSuperclasses.                               "get all superclasses of a class"
x := 1.2 hash.                                              "hash value for object"
y := x copy.                                                "copy object"
y := x shallowCopy.                                         "copy object (not overridden)"
y := x deepCopy.                                            "copy object and instance vars"
y := x veryDeepCopy.                                        "complete tree copy using a dictionary"
```

---------------------------------------------------------------------------------------------------------

```
x := 5.                                                     ; assignment
x := (y := (z := 6)).                                       ; compound assignment
x := ((y := 6) + 1).
x := (@object :new).                                        ; bind to allocated instance of a class
x := (123 :class).                                          ; discover the object class
x := (@integer :superclass).                                ; discover the superclass of a class
x := (@object :all-instances).                              ; get an array of all instances of a class
x := (@integer :all-superclasses).                          ; get all superclasses of a class
x := (1.2 :hash).                                           ; hash value for object
y := (x :copy).                                             ; copy object
y := (x :shallow-copy).                                     ; copy object (not overridden)
y := (x :deep-copy).                                        ; copy object and instance vars
y := (x :very-deep-copy).                                   ; complete tree copy using a dictionary
```

=========================================================================================================

```
| b x |
b := true.                                                  "true constant"
b := false.                                                 "false constant"
x := nil.                                                   "nil object constant"
x := 1.                                                     "integer constants"
x := 3.14.                                                  "float constants"
x := 2e-2.                                                  "fractional constants"
x := 16r0F.                                                 "hex constant"
x := -1.                                                    "negative constants"
x := 'Hello'.                                               "string constant"
x := 'I''m here'.                                           "single quote escape"
x := $A.                                                    "character constant"
x := $ .                                                    "character constant (space)"
x := #aSymbol.                                              "symbol constants"
x := #(3 2 1).                                              "array constants"
x := #('abc' 2 $a).                                         "mixing of types allowed"
```

---------------------------------------------------------------------------------------------------------

```
b := true.                                                  ; true constant
b := false.                                                 ; false constant
b := c                                                      ; nothing constant (empty object)
x := 1.                                                     ; integer constants
x := 3.14.                                                  ; float constants
x := 2e-2.                                                  ; fractional constants
x := 16r0F.                                                 ; hex constant
x := -1.                                                    ; negative constants
x := "Hello".                                               ; string constant
x := "I'm \"here\""                                         ; double quote escape
x := \A                                                     ; character constant
x := \u0020.                                                ; Unicode character constant (space)
%foo                                                        ; symbol constants (syntax may change)
:foo                                                        ; keyword constants
x := [3 2 1].                                               ; array constants
x := ["abc" 2 \a].                                          ; mixing of types allowed
```

=========================================================================================================

```
| b x y |
x := 1. y := 2.
b := (x = y).                                               "equals"
b := (x ~= y).                                              "not equals"
b := (x == y).                                              "identical"
b := (x ~~ y).                                              "not identical"
b := (x > y).                                               "greater than"
b := (x < y).                                               "less than"
b := (x >= y).                                              "greater than or equal"
b := (x <= y).                                              "less than or equal"
b := b not.                                                 "boolean not"
b := (x < 5) & (y > 1).                                     "boolean and"
b := (x < 5) | (y > 1).                                     "boolean or"
b := (x < 5) and: [y > 1].                                  "boolean and (short-circuit)"
b := (x < 5) or: [y > 1].                                   "boolean or (short-circuit)"
b := (x < 5) eqv: (y > 1).                                  "test if both true or both false"
b := (x < 5) xor: (y > 1).                                  "test if one true and other false"
b := 5 between: 3 and: 12.                                  "between (inclusive)"
b := 123 isKindOf: Number.                                  "test if object is class or subclass of"
b := 123 isMemberOf: SmallInteger.                          "test if object is type of class"
b := 123 respondsTo: sqrt.                                  "test if object responds to message"
b := x isNil.                                               "test if object is nil"
b := x isZero.                                              "test if number is zero"
b := x positive.                                            "test if number is positive"
b := x strictlyPositive.                                    "test if number is greater than zero"
b := x negative.                                            "test if number is negative"
b := x even.                                                "test if number is even"
b := x odd.                                                 "test if number is odd"
b := x isLiteral.                                           "test if literal constant"
b := x isInteger.                                           "test if object is integer"
b := x isFloat.                                             "test if object is float"
b := x isNumber.                                            "test if object is number"
b := $A isUppercase.                                        "test if upper case character"
b := $A isLowercase.                                        "test if lower case character"
```

---------------------------------------------------------------------------------------------------------

```
x := 1. y := 2.

= := :equals.
<> := :not-equal.
> := :greater-than.
< := :less-than.
>= := :greater-than-or-equal.
<= := :less-than-or-equal.
& := :bitwise-and   ; TODO: better name
| := :bitwise-or    ; TODO: better name
&& := :and
|| := :or

b := (x = y).
b := (x <> y).
b := (x :is y).
b := (x :is-not y).
b := (x > y).
b := (x < y).
b := (x >= y).
b := (x <= y).
b := (b :not).
b := ((x < 5) & {y > 1}).
b := ((x < 5) | {y > 1}).
b := ((x < 5) :and {y > 1}).
b := ((x < 5) :or {y > 1}).
b := ((x < 5) :is (y > 1)).
b := ((x < 5) :xor (y > 1)).
b := (5 :between 3 :and 12).
b := (123 :is-kind-of @number).
b := (123 :is-member-of @small-integer).
b := (123 :responds-to :sqrt).
b := (x :nothing?).
b := (x :zero?).
b := (x :positive?).
b := (x :strictly-positive?).
b := (x :negative?).
b := (x :even?).
b := (x :odd?).
b := (x :literal?).
b := (x :integer?).
b := (x :float?).
b := (x :number?).
b := (\A :uppercase?).
b := (\A :lowercase?).
```

=========================================================================================================

```
| x |
x > 10 ifTrue: [Transcript show: 'ifTrue'; cr].             "if then"
x > 10 ifFalse: [Transcript show: 'ifFalse'; cr].           "if else"
x > 10                                                      "if then else"
   ifTrue: [Transcript show: 'ifTrue'; cr]
   ifFalse: [Transcript show: 'ifFalse'; cr].
x > 10                                                      "if else then"
   ifFalse: [Transcript show: 'ifFalse'; cr]
   ifTrue: [Transcript show: 'ifTrue'; cr].
Transcript show:
    (x > 10
        ifTrue: ['ifTrue']
        ifFalse: ['ifFalse']);
   cr.
Transcript                                                  "nested if then else"
   show:
      (x > 10
         ifTrue: [x > 5
            ifTrue: ['A']
            ifFalse: ['B']]
         ifFalse: ['C']);
   cr.
switch := Dictionary new.                                   "switch functionality"
switch at: $A put: [Transcript show: 'Case A'; cr].
switch at: $B put: [Transcript show: 'Case B'; cr].
switch at: $C put: [Transcript show: 'Case C'; cr].
result := (switch at: $B) value.
```

---------------------------------------------------------------------------------------------------------

```
x > 10 :if-true {Transcript :show "if-true", :cr}.
x > 10 :if-false {Transcript :show "if-false", :cr}.
x > 10
  :if-true {Transcript :show "if-true", :cr}
  :if-false {Transcript :show "if-false", :cr}.
x > 10
  :if-false {Transcript :show "if-false", :cr}
  :if-true {Transcript :show "if-true", :cr}.
Transcript :show (x > 10
                    :if-true ["if-true"]
                    :if-false ["if-false"]),
           :cr.
Transcript
  :show
    (x > 10
       :if-true {x > 5
                   :if-true ["A"]
                   :if-false ["B"]}
       :if-false ["C"]),
    :cr.
switch := (Dictionary :new).
switch :at \A :put {Transcript :show "Case A", :cr}.
switch :at \B :put {Transcript :show "Case B", :cr}.
switch :at \C :put {Transcript :show "Case C", :cr}.
result := ((switch :at \B) :value).
```

=========================================================================================================

A map is a special type of object.

Lambdas:

```
#(3 > 2) :value.  ; -> true

#(> 4 %:foo 2 %:bar) :foo 3 :bar 1.  ; -> true

;; #{:foo #(3 > 2).
;;   :foo-2 [a b c] #()}
```
