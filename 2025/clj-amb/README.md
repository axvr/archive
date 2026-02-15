# Exception-based non-determinism in Clojure

_2025-01-06 – 2025-01-08_

This is an implementation of the non-determinism found in the book _On Lisp_ in
Clojure that uses exceptions in place of continuations to achieve the desired
behaviour with excellent performance.

This was the [3rd attempt][attempts] and was quite successful.  Using it I was
able to encode many algorithms that were previously complicated to custom
implement. The `nondeterm_try.clj` file is packed full of examples.

This is actually quite similar to the control loop structures found in other
languages, where `(fail)` is equivalent to `continue;` and `(cut)` is
equivalent to `break;`.  Not exactly the same, but still interesting to see the
commonality.

_Public domain.  No rights reserved._

[attempts]: ../../2024/non-determinism/
