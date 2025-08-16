# NANDC

_30th November 2019 – 10th May 2020_ (intermittently)

A collection of very early notes, code and other exploratory material for what would later become [Gait][].

"NANDC" (pronounced "Nancy"; short for "NAND Compiler") was envisioned as an esoteric logic simulation language, a technical joke, where the only construct was NAND, represented as brackets, that would be really difficult to read and write.

```lisp
(1 1)    ; => 1 NAND 1

(0 1)    ; => 0 NAND 1

((a b)
 (b c))  ; => (a NAND b) NAND (b NAND c)

;; Many sophisticated examples can be found in the accompanying files.
```

Quickly the idea morphed from a hypothetical joke to a still hypothetical, but serious, visual [Hardware Description Language (HDL)](https://en.wikipedia.org/wiki/Hardware_description_language) featuring a highly interactive IDE.

(This thought process of thinking about NANDC is actually what lead to my "Lisp realisation".  That frequently documented moment where Lisp suddenly makes sense...)

Since 2020, the idea sat at the back of my mind, gradually evolving as I learnt more about computing.  (Lisp, the Xerox Alto, Smalltalk, the Burroughs B5000 and much more.)

As of 2022-10-24, the implementation process began.  Originally Machina, now [Gait][], it is intended to become a highly interactive, object-oriented (in the Smalltalk sense), Lisp-like HDL and eventually gain that visual IDE.

[Gait]: ../../2022/gait/

_Public domain.  No rights reserved._
