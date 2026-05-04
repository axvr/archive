This directory contains several old experiments into creating a version of
`void`.  Some of these attempted to extend Clojure's data structures to
natively support `void`, however it was later discovered that Clojure's data
structures are marked as "final" which means they cannot be subclassed.  This
is likely a performance optimisation, which is great, but annoying in this
case.

Ultimately I went for the simpler approach which was a purely macro-based one,
without native "void" as a value support as that can handle the most common use
case without the limitations of the others (e.g. void leakage/`void != nil`
problem).
