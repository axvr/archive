# Programming language experiments (Nq)

_2023-08 – 2024-07_

_Public domain.  No rights reserved._

> [!NOTE]
> Another early programming language design experiment.  See also the
> [precursor](../../2020/porcelina) to this and the intermeidate language
> [Vila](../../2024/vila).

An interactive, programming language designed for constructing and maintaining large distributed systems.

```clojure
(-> [0 1 2 3 4 5 6 7 8 9]
    (map inc)
    (filter even?))

;; eduction by default.  Self-reduces when needed.

;; (~ 1 3)

(let [x (amb 1 2 3)
      y (amb 4 6 8)]
  (infer even? x)
  (infer #(= 8 (* %1 %2)) x y)
  (conclude x))  ; @x

(defprotocol Inferable
  (conclude [this] "")
  (inference [this f dependents] "")
  (explain [this] ""))

(defrecord Amb [options deductions]
  Inferable
  (inference [_this] ...)
  (conclude [_this] ...)
  (explain [_this] ...)
  clojure.lang.Deref
  (deref [this] (conclude this)))

(defn amb [& options]
  (ref (->Amb options nil)))

(defn infer [premise & subjects]
  (dosync
    ...
    (alter ...)))

::fallacy

premise
fact
infer

(eduction)
```

Interactive systems programming language.

- Connect to systems with visual GUI.
- Contract checkers.
- Tracing and logging built-in and automatic.
- Automatically distribute across VMs.

Erlang/OTP + Clojure.

Transducers from the foundation.

No floating point maths!  Auto-promote numbers.

VM modules which hijack operations replacing core functions with it's own
implementations.  E.g. UUID generation, dates/times, etc.
This hijacking method could be used to provide FFI?

`eval` which supports custom environments and sandboxes for building powerful
DSLs.

Everything can have metadata!

Protocols are at the core.  Custom types are extendable to any protocol.
^ name protocols interfaces?

If writing in Zig, wait for async + use C libraries by DJB.  (e.g. libtai,
NaCl, etc.)

OpenTelemetry support requires HTTP server and Protocol Buffers.  Perhaps
export another way and run collector on separate container?

```
nq start --nodes=3
```

Lacking libc and other C foundations puts us at a disadvantage as we need to
reimplement a lot, especially the various networking stacks.  Potentially an
"[Epoxy](../../2022/epoxy)" like solution may work?

Maybe .NET based?
