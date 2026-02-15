# Elixir implementation of the `amb` operator

_2026-01-20_

A quick implementation of the `amb` operator in Elixir.

Following on from the success of using [JVM exceptions from Clojure to implement
this][clj-amb], I figured I'd give it a quick go in Elixir too.

It's not quite as nice to use as you need to pass it a function as the body
rather than being entirely macros.  The main difference is that this one does
not short-circuit on success, instead returning all possible values.  Creating
a short-circuiting implementation should be fairly easy to achieve.

_Public domain.  No rights reserved._

[clj-amb]: ../../2025/clj-amb/
