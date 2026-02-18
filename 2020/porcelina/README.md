# Programming language experiments (Porcelina)

_2020-05-09 – 2021-06-15_

> [!NOTE]
> This was one of my earliest programming language design experiments.  Later
> followed by [Nq](../../2023/nq-proglang) in 2023.  Some ideas carry through
> to my later experiments.

Porcelina is a long-term experimental research programming language, [Virtual
Machine][vm] and development environment, aiming to showcase and build a bridge
to a better model of computing.  It is **not** a programming language you can
use for your next project.

```
> dict := (@dict :init [:foo "Hello"]
                       [:bar "World"]
                       [:biz 1]
                       [:baz true]).
> dict :get :foo.
"Hello"
> dict :assoc :bar
       :value nothing.
> dict :get :bar.
nothing
> (dict :get :bar) :nothing?.
true
```

[vm]: https://en.wikipedia.org/wiki/Virtual_machine#Process_virtual_machines

_Public domain.  No rights reserved._
