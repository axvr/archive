# On Lisp non-determinism in Clojure

_2024-07-05_

Failed attempt at replicating the non-determinism macros from the book _On
Lisp_ to Clojure.

I believe this was my second attempt.  I was trying to make it integrate well
into Clojure, by using dynamic vars, but unfortunately I wasn't able to get the
macro expansions to work correctly.  Later I came to the realisation that
[exceptions](../../2025/clj-amb) could achieve the desired result.

_Public domain.  No rights reserved._
