# Virtual machines & compiler backends

___Last updated: 2024-10-27___

There are a decent number of pre-built programming language virtual machines
(VMs) and compiler backends readily available as a platform for building
programming languages and environments upon.  From my research the top options
for my language are:

| Target | VM | JIT | GC | Runtime | Tooling\* | Dynamic | Performant |
|--------|----|-----|----|---------|-----------|---------|------------|
| [JVM][] ([OpenJDK][] HotSpot)  | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| [CLR][]/[CLI][] ([.NET][])         | ✅ | ✅ | ✅ | ✅ | ✅ |    | ✅ |
| [SBCL][]                   |    |    | ✅ | ✅ | ✅ | ✅ | ✅ |
| [BEAM][]                   | ✅ |    | ✅ | ✅ | ✅ | ✅ |    |
| [MoarVM][]                 | ✅ | ✅ | ✅ |    |    | ✅ |    |
| [WebAssembly][] ([Wasmtime][]) | ✅ | ✅ | ✅ | [Lunatic][]? | | ? | |

\* Tooling = effective profilers, decompilers, benchmarking tools, etc.

Of these the most feasible options for me are: the JVM, the CLR and SBCL.
Let's explore this further...


## Why use an existing VM?

It would be a waste of my time to even attempt to build a competitive GC, JIT
compiler and runtime completely from scratch when many experts have already
spent huge amounts of time in doing it far better than I ever could, and for
free!

By utilising an existing VM, GC, JIT compiler and runtime, I can keep language
development more focused on the important parts that move it towards my
ambitious goals.  Not every programming language needs its own VM, JIT and GC.
Why reinvent the wheel if you're only going to make a worse wheel.

While Julia has shown that it is possible to use low-level compiler backends
(e.g. [LLVM][] and [MIR][]) for high performance dynamic languages, these are
far too low level for me (e.g. no garbage collector or runtime).


## Why not BEAM?

[BEAM][] and [Erlang/OTP][] are excellent pieces of engineering (and is exactly
what I'm trying to achieve), but it unfortunately hasn't seen anywhere near the
support and effort that its counterparts (the JVM and CLR) have received into
tooling and making it fast.

BEAM is bad at numeric computation and copies data between processes.  The
latter is the biggest issue; I believe the combination of a faster VM and
persistent immutable data structures will easily be able to significantly
outperform BEAM in every aspect.  Every other guaranatee that Erlang/OTP and
BEAM provide can be implemented a top of the JVM or CLR and should be
reasonably straightforward as I intend to severely limit (and wrap) the VM
capabilities directly available to programs written in my language to achieve
isolation and powerful hot reloading.


## Why not WASM?

[WebAssembly][] is still new and has not reached maturity yet.  While there are
some excellent projects in this area, such as [Cranelift][], [Wasmtime][] and
[Lunatic][] they are still very far behind the JVM, CLR, BEAM and SBCL.  The
WebAssembly GC being optional is also an issue for my language as that implies
that it'll be even less mature.  Even browser vendors don't care very much
about WASM.  As a final note on this, something doesn't sit right with me about
using Web technologies outside of the Web.


## Why not MoarVM?

[MoarVM][] is designed for the [Raku][] programming language; a highly dynamic
and flexible language.  This dynamism is reflected in MoarVM, which makes it
a compelling choice.  Unfortunately though, it is still very young and sees
little development activity, hence its poor performance.  The final nail in the
coffin for targetting MoarVM is the lack of stable bytecode for it.


## SBCL

Condition system would make things easier.

Decent low-level performance.

Supports TCO.


## JVM vs. CLR

The main VM competitors available are the [JVM][] ([OpenJDK][] Hotspot) and [CLR][] ([.NET][]).

HotSpot C1/C2 JIT is superior to .NET [Tiered Compilation][.NET TC] (TC) JIT,
although .NET's is getting better slowly.  A future option for HotSpot is to
use the [Graal JIT][] which is even faster than C1/C2.  .NET has PGO (Profile
Guided Optimisation) but once again it is a more limited compared to HotSpot's
profiling optimiser.

HotSpot has the ZGC garbage collector, capable of terrabytes of garbage
collection with sub 1ms pauses, which makes it far more capable than any other
language.  .NET GC by comparison is much simpler, but is by no means bad.

CLR bytecode is much better than JVM bytecode as are the modules and assembly
formats.  Similarly CLR bytecode features more types (e.g. non-signed bytes and
signed integers) and reified generics.  .NET also has lower level constructs,
e.g. "spans" and "buffers".

Both VMs effectively lack tail-call optimisation (TCO).  While the CLR has the
`tail.` bytecode, it is really just a suggestion to the JIT (less of
a suggestion on "Production" builds) and stack overflows can still occur.  The
JVM *still* has no TCO support, but while Project Loom aims to rectify this (at
least partially), it is still potentially possible using the ASM library.

In terms of AOT support on the JVM and CLR, they are both no-gos as they
destroy runtime tangibility and dynamism.

Overall, the comparative limitations of the .NET JIT and GC are avoided by
smarter compilers and type systems along with use of lower level tools.  The
OpenJDK HotSpot VM almost does the inverse by using its magic JIT and GC to
overcome compiler limitations.  All this is to say that the JVM makes
implementing a performant dynamic language on the JVM far easier than on the
CLR, but it is not impossible on the CLR, but the result will very likely be
slower than the JVM equivalent unless much of the language is written in C#.

If only there were some middleground.

> What was particularly interesting for me in implementing Clojure, was how
> much runtime tangibility and situated sensibilities were in the JVM design.
> The JVM is actually a very dynamic thing.  As much as Java looks like, say C#
> or C++, the JVM was written with the idea of we're going to embed these
> programs on set-top boxes, network them and need to send code around [to]
> update their capabilities.  That's like it's situated everywhere you turn,
> and the runtime has got a ton of excellent support for that.  Which makes it
> a great platform for languages like Clojure.  Thank goodness that the work
> that the people did on Self didn't die; that it actually carried through
> here.  Not everything did, but it's quite important and it'll be a sad day
> when someone says "let's just replace the JVM with some static compilation
> technology".  And I'll tell you, targetting the JVM and the CLR, it's plain.
> The CLR is static thinking, and the JVM is dynamic thinking.
>
> — Rich Hickey, 2017, [Effective Programs - 10 Years of Clojure](https://www.youtube.com/watch?v=2V1FtfBDsLU)


> The focus difference between Java and .NET couldn't be more different.  Even
> though the platforms from a high level look very similar, they have a very
> different focus in terms of optimisation and priorities for development.
> HotSpot is all about dynamic stuff, other Java VMs are necessarily doing the
> same things, because the patterns of core match with the whole ecosystems and
> traditions in the Java space.  Then there's [...] all the dynamic languages
> now coming online (or already online) with even more now with `invokedynamic`
> on the Java virtual machines.  Whereas .NET focuses very much on static
> compilation; not doing any runtime optimisations [sic. no longer true with
> .NET Tiered Compilation] other than straight JITing and straight inlining and
> stuff like that.  No HotSpot type of recompilation or things like that.  They
> really focus on getting the working set down of the code, instead of trying
> to make it run as fast as possible.  They try to make it as efficient in
> memory usage and in terms of how many pages are touched when you start up
> a process.  I think this difference is primarily caused by the fact that Java
> became popular on the server, whereas Microsoft is still focused a lot on the
> client.  On the server it makes sense to spend some extra time optimising
> your process because it's usually a long running process, whereas on the
> client you start up lots of different applications and you also have
> multiuser systems.  [...] So this is why reified generics are very important,
> because if you don't have any HotSpot type optimisation where you can get rid
> of the cost of the erased generics that Java has, you need real generics to
> get rid of that cost.
>
> — Jeroen Frijters, 2019, [Inspiration from the Other Side](https://www.youtube.com/watch?v=ETg8oRXXhD8)


.NET GC supports server and workstation modes.  .NET has better documentation and
standardised documentation XML formats.

Profiling and benchmarking tools.  BenchmarkDotNet.

The JVM now has lightweight (green) threads in fibres.

.NET has better SIMD API.

.NET means that some code could be reused in a GUI built with Godot .NET.

Both implement IEEE-754 floating point maths.  Bad!  But largely have better
alternatives built in.

More competition between JVM languages than CLR languages.

GraalVM makes me slightly worried about future Java development on HotSpot
potentially ceasing.

.NET can be statically compiled for use on a Docker `FROM scratch` image,
whereas OpenJDK can't.

.NET is much better designed and more secure than OpenJDK too.

Both are fully cross platform and work on ARM and x64.

.NET can run in the browser via WASM compilation.  GraalVM can too, but that
won't work for me since it requires AOT compilation.

Tradeoffs!



## Verdict

JVM HotSpot or .NET.


[Graal JIT]: https://www.graalvm.org/latest/reference-manual/compiler/operations/
[.NET TC]: https://github.com/dotnet/runtime/blob/main/docs/design/features/tiered-compilation.md
[JVM]: https://en.wikipedia.org/wiki/Java_virtual_machine
[MIR]: https://github.com/vnmakarov/mir
[OpenJDK]: https://openjdk.org/
[CLR]: https://learn.microsoft.com/en-us/dotnet/standard/clr
[CLI]: https://en.wikipedia.org/wiki/Common_Language_Infrastructure
[.NET]: https://dotnet.microsoft.com/
[BEAM]: https://en.wikipedia.org/wiki/BEAM_VM
[Erlang/OTP]: https://erlang.org/
[MoarVM]: https://www.moarvm.org/
[SBCL]: https://sbcl.org/
[Cranelift]: https://cranelift.dev/
[Wasmtime]: https://github.com/bytecodealliance/wasmtime
[WebAssembly]: https://webassembly.org/
[Lunatic]: https://lunatic.solutions/
[Raku]: https://raku.org/

---

## JVM

Write it in Clojure and Java, use ASM and high performance Clojure libs.  AOT
compile Enqueue with direct linking.  Make use of faster alternatives to
Clojure's data structures.

VM on a VM?  Maximise the amount of the system written in Enqueue?

Possible data structures:
- Clojure built-ins.
- [cnuernber/ham-fisted](https://github.com/cnuernber/ham-fisted)
- [lacuna/bifurcan](https://github.com/lacuna/bifurcan)
- [bsless/clj-fast](https://github.com/bsless/clj-fast)
