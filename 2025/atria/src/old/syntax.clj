(offer accept: [1 2 3] with: 123 | ...)
 ^object ^method ^msg  ^method-pt2


;; com/example/_mod.nq

(= !/bind 'def:)

(mod registry-extend: '{axvr {git/url-prefix "git@github.com:axvr"}})
(mod requires: '[axvr/fsm])

(mod init: (fn [] ...))

(axvr/fsm init: ...)

(schema-reg ...)

(class _meta {} [:enter ...])



()    <- lookup symbols and execute.        (linked list)
[]    <- lookup symbols but don't execute.  (vector)
{}    <- lookup symbols but don't execute.  (hash map)
'     <- quote = don't look up.
`     <- quasiquote = don't look up, but selectively allow escaping quoting.
~     <- escape quasiquote.
sym   <- symbol.
:sym  <- symbols beginning with `:` are not evaluated.
,     <- commas are whitespace.
/     <- ns separator.
:"a"  <- equivalent to :a but allows for any characters to be used.
:"a"/"b" <- :a/b namespace qualified method allowing any characters.
:a/"b" :"a"/b
::a/b <- expand the `a` namespace from an alias.
"str" <- string.
^  meta data?
%sym <- placeholder value.

 (object :method payload :named-params payload)
 [object :method payload]   ;; thunk
([object :method payload])  ;; execute thunk

;; The `:` on the start of the method, enables an editor to ask the object for
;; the list of methods and simplifies reading.  Maybe use the classic `.`
;; instead?


(= :/become (macro [] ...))

;; undefined symbols are "undefined" objects.

(= := :=)
(! = nq/log)
(http = org.enqueue.net/http)

(http :get {url: "...", body: ...})


(foo := (nq/obj :/new-class
                (...)))

 foo := (org.enqueue.core/obj :subclass).
(foo := (org.enqueue.core/obj :subclass))

objects internally vs. functions internally?

objects as module/service interface?

Functions AND objects and macros?

Functions as an abstraction over objects.

[+ 1 2 %placeholder]
(+ 1 2 3)

#(+ 1 2 3)
