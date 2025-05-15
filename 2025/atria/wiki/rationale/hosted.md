# Rationale: why host on an existing platform?

Like Clojure, Catalyst is intended to run on an existing platform.  With
hosting, I can utilise the platform's powerful VM, JIT compiler, garbage
collector and libraries/tooling.  Many existing platforms have had huge amounts
of work put into them by experts for free!  It would be foolish to believe that
I could build a competitive GC, JIT compiler and runtime from scratch, or that
my language would become popular enough to attract those who could.

With this freed time, I can focus language development on what I consider to be
the more critical parts.  While each platform will have design choices counter
to Catalyst's, I believe the trade-off will be worth it.  Remember that not
every programming language needs to completely reinvent the wheel.


## Host comparison

There are a decent number of pre-built host platforms available for building
programming languages and programming environments upon.  The top options for
Catalyst are:

| Target | VM | JIT | GC | Runtime | Tooling\* | Dynamic | Performant |
|--------|----|-----|----|---------|-----------|---------|------------|
| [JVM][] ([OpenJDK][]) | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| [CLR][]/[CLI][] ([.NET][]) | ✅ | ✅ | ✅ | ✅ | ✅ | | ✅ |
| [SBCL][] | | | ✅ | ✅ | ✅ | ✅ | ✅ |
| [BEAM][] | ✅ | | ✅ | ✅ | ✅ | ✅ | |
| [MoarVM][] | ✅ | ✅ | ✅ | | | ✅ | |
| [WebAssembly][] (via [Wasmtime][]) | ✅ | ✅ | WIP | [Lunatic][]? | | ? | |

\* Tooling = effective profilers, decompilers, micro-benchmarking tools, etc.

Of these, the JVM and CLR are likely the best suited for Catalyst, but let's
explore the available options further...


### The JVM and the CLR

The big two cross-language VMs are the [JVM][] ([OpenJDK][] Hotspot) and
[CLR][] ([.NET][]) which are fully cross-platform and work on x86 and ARM.

OpenJDK HotSpot is designed for long running servers, and its ZGC garbage
collector can perform sub 10ms garbage collections with a heap size in the
terabytes!

.NET has made incredible strides in catching up with the JVM in the last few
years and now it even overtakes it in some areas.

HotSpot C1/C2 JIT is still superior to .NET [Tiered Compilation][.NET TC] (TC)
and it looks to become even better with [Graal JIT][].  Similarly .NET's PGO
(Profile Guided Optimisation) is still more limited.

While the .NET GC is simpler than the JVM GCs it is still excellent.  Since
there is only one garbage collector in .NET it integrates well with the rest of
the .NET ecosystem as the runtime is designed to avoid GC pressure.  This
simplicity offers better security.

CLR bytecode is generally superior to JVM bytecode as are the modules and
assembly formats.  CLR bytecode includes more types (e.g. non-signed bytes and
signed integers) and reified generics.  .NET also has lower level constructs,
e.g. "spans" and "buffers".

Realistically both VMs lack tail-call optimisation (TCO).  While the CLR has
the `tail.` bytecode, it more of a suggestion to the JIT (and generally ignored
unless it is on a "Production" build) meaning that stack overflows can still
occur.  While the JVM *still* has no TCO support, it is potentially possible
through the ASM library.

The JVM and CLR both have AOT support but are are no-gos as they destroy
runtime tangibility and dynamism, however .NET's WASM compilation retains the
dynamism, unlike GraalVM.  The community focus on AOT and GraalVM makes me
slightly worried about future Java development on dynamic-focused HotSpot
potentially decreasing.

.NET versions of immutable persistent data structures may be much more
performant (compute and memory) than equivalents in Java however when
processing large data, there is an unavoidable amount of garbage that would be
created which the JVM will be capable of handling better.

Both have equally good profiling and micro-benchmarking tools ([JMH][]
& [BenchmarkDotNet][]).

[JMH]: https://github.com/openjdk/jmh
[BenchmarkDotNet]: https://github.com/dotnet/BenchmarkDotNet

Overall, the limitations of the .NET are generally avoided by smarter compilers
and type systems along with use of lower level tools.  The OpenJDK HotSpot VM
largely does the inverse, using its magic JIT and GC to do the work the
compiler didn't do.  Implementing a performant dynamic language on the JVM is
far easier than on the CLR.

There is plenty more that .NET does better too, such as better documentation,
[standardised XML documentation format][.NET API docs], simpler configuration
(e.g. GC server/workstation modes), better SIMD API, [Godot][] supports it
which would allow code reuse if I want to build an IDE for it.

[Godot]: https://godotengine.org/
[.NET API docs]: https://github.com/dotnet/dotnet-api-docs

#### Expert opinions

> What was particularly interesting for me in implementing Clojure, was how
> much runtime tangibility and situated sensibilities were in the JVM design.
> The JVM is actually a very dynamic thing.  As much as Java looks like, say C#
> or C++, the JVM was written with the idea of we're going to embed these
> programs on set-top boxes, network them and need to send code around [to]
> update their capabilities.  [I]t's situated everywhere you turn, and the
> runtime has got a ton of excellent support for that.  Which makes it a great
> platform for languages like Clojure.  Thank goodness that the work that the
> people did on Self didn't die; that it actually carried through here.  Not
> everything did, but it's quite important and it'll be a sad day when someone
> says "let's just replace the JVM with some static compilation technology".
> And I'll tell you, targeting the JVM and the CLR, it's plain.  The CLR is
> static thinking, and the JVM is dynamic thinking.
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
> compilation; not doing any runtime optimisations [sic. not true any more with
> TC and PGO] other than straight JITing and straight inlining and stuff like
> that.  No HotSpot type of recompilation or things like that.  They really
> focus on getting the working set down [...], instead of trying to make it run
> as fast as possible.  They try to make it as efficient in memory usage and in
> terms of how many pages are touched when you start up a process.  I think
> this difference is primarily caused by the fact that Java became popular on
> the server, whereas Microsoft is still focused a lot on the client.  On the
> server it makes sense to spend some extra time optimising your process
> because it's usually a long running process, whereas on the client you start
> up lots of different applications and you also have multiuser systems.  [...]
> So this is why reified generics are very important, because if you don't have
> any HotSpot type optimisation where you can get rid of the cost of the erased
> generics that Java has, you need real generics to get rid of that cost.
>
> — Jeroen Frijters, 2019, [Inspiration from the Other Side](https://www.youtube.com/watch?v=ETg8oRXXhD8)

> Java has a lot of security vulnerabilities [...].  .NET also has its share,
> but they have an interesting concept here and I think Java can learn from
> this.  They separate their code into "transparent code" and "critical code"
> in terms of security [...].  "Safe critical code" is the bridge between the
> two.  You cannot call from _transparent code_ into _critical code_, and
> _critical code_ is the only code that can do trusted operations.  This
> separation means that when you audit your code you know where to look,
> because you just look for the _safe critical_ annotation and when you see
> that, you know you have to do a security audit on this code.
>
> — Jeroen Frijters, 2019, [Inspiration from the Other Side](https://www.youtube.com/watch?v=ETg8oRXXhD8)

> The ECMA CLI in general benefited from the lessons learned from Java and
> fixed a few issues. While the JVM bytecodes were designed to be easy to
> interpret, by the time the CLR came around it was already well understood
> that interpreters were cute but any VM worth using was going to use a JIT, so
> the instruction set does reflect this design. For example there is a single
> "add" opcode in the CLR and it requires the JIT compiler to track the types
> on the stack to ensure that the types are compatible at the time the
> operation is performed. The JVM on the other hand has type-specific add
> instructions (`fadd`, `dadd`, `iadd`, `ladd`).
>
> Some other features include:
>
> - Unsigned data types
> - Checked arithmetic (on overflow, they throw an exception)
> - Support for tail calls (for Lisp, F# and other functional languages)
> - Value types, these are structs that are not wrapped in an object
> - VM-level support for generics
> - Platform-invoke allows developers to call native code without having to
>   write any glue in C++ using JNI, it can all be done in the managed
>   language.
> - The Common Language Specification: a spec that set the rules for
>   interoperability across programming languages (for example: the rules for
>   public identifier casing, handling of int vs uint and so on).
> - Delegates allow user to keep a reference to a method or an instance method
>   and invoke it. The VM also can turn any delegate invocation into an
>   asynchronous invocation, so you can turn any method into an async method,
>   like this: mySortFunc.BeginInvoke (array)
> - Support for dynamic code generation through the Reflection.Emit API.
> - A database file format allows for efficient reading of the metadata from
>   assemblies. It does not require decompression and the database format is
>   suitable for lazy loading.
> - Attributes were special objects that could annotate most elements in the
>   virtual machine, you could annotate classes, methods, parameters, local
>   variables, and at runtime the VM or the runtime could do interesting things
>   with it.
> - Unsafe code (pointers) was added to support C++, Cobol and Fortran
>   compilers running on the CLI.
> - [...]
> - 64-bit arrays (although part of the spec, only Mono implements this).
>
> Generics in C# and the CLR are a pleasure to use compared to Java due to the
> native generic support for it. The explanation is straight forward, while the
> path that Java took ("type erasure") meant that everything became just too
> hard. This is what has lead to Java generics becoming almost impenetrable and
> a FAQ that makes Wikipedia seems small by comparison.
>
> For instance, you can not write code like this in Java generics:
>
>     class Stack<T> {
>         T [] items;
>     }
>
> Since Java does not allow a `T` array to be declared as an instance
> variable. You must use an `object []` and cast.
>
> With .NET 4 a few * Covariance and contravariance introduced with .NET 4 make
> even more generic cases a pleasure to use.
>
> The major differences happen at the class library level and at the language
> level (like lambda and iterator support) and that is a list too long to write
> up.
>
> — Miguel de Icaza, 2010, [Reddit: What are the technical differences between the CLR and JVM?](https://old.reddit.com/r/programming/comments/cys02/askproggit_what_are_the_technical_differences/c0wadup/)


The JVM now has lightweight threads (fibres) for IO.

Both implement IEEE-754 floating point maths.  Bad!  But largely have better
alternatives built in.

.NET can be statically compiled for use on a Docker `FROM scratch` image,
whereas OpenJDK can't.  .NET would be better suited for local machines,
containerisation and multi-user/application systems.

More competition among JVM languages than .NET languages.  Might be less
expectation that .NET libs would work on a .NET hosted Catalyst, than Java libs
on a JVM hosted Catalyst?

`invokedynamic` isn't even used by Clojure, so perhaps the more static CLR isn't
an issue?  Need to investigate the DLR vs. `invokedynamic` further.  (I
probably wouldn't even use the DLR anyway.)  Similar to how Julia optimises for
static LLVM.

Clojure-like vars limit the JVM JIT's ability to inline.  This may hit Catalyst
too, whether it is on the JVM or CLR.  On the JVM one solution is
`invokedynamic` or "safe points".  How can this be worked around in .NET?
Perhaps something akin to Smalltalk's `become:`?  For the most part since
Catalyst will wrap basically everything, `invokedynamic` may not actually be
all that useful?


### BEAM

[BEAM][] and [Erlang/OTP][] are excellent pieces of engineering (and is exactly
what I'm trying to achieve), but it unfortunately hasn't seen anywhere near the
support and effort that its counterparts (the JVM and CLR) have received into
tooling and making it fast.

BEAM is bad at numeric computation and copies data between processes.  The
latter is the biggest issue; I believe the combination of a faster VM and
persistent immutable data structures will easily be able to significantly
outperform BEAM in every aspect.  Every other guarantee that Erlang/OTP and
BEAM provide can be implemented a top of the JVM or CLR and should be
reasonably straightforward as I intend to severely limit (and wrap) the VM
capabilities directly available to programs written in my language to achieve
isolation and powerful hot reloading.

[BEAM]: https://en.wikipedia.org/wiki/BEAM_VM
[Erlang/OTP]: https://erlang.org/


### WebAssembly

[WebAssembly][] is still new and has not reached maturity yet.  While there are
some excellent projects in this area, such as [Cranelift][], [Wasmtime][] and
[Lunatic][] they are still very far behind the JVM, CLR, BEAM and SBCL.  The
WebAssembly GC being optional is also an issue for my language as that implies
that it'll be even less mature.  Even browser vendors don't care very much
about WASM.  As a final note on this, something doesn't sit right with me about
using Web technologies outside of the Web.

[Cranelift]: https://cranelift.dev/
[Wasmtime]: https://github.com/bytecodealliance/wasmtime
[WebAssembly]: https://webassembly.org/
[Lunatic]: https://lunatic.solutions/


### MoarVM

[MoarVM][] is designed for the [Raku][] programming language; a highly dynamic
and flexible language.  This dynamism is reflected in MoarVM, which makes it
a compelling choice.  Unfortunately though, it is still very young and sees
little development activity, hence its poor performance.  The final nail in the
coffin for targeting MoarVM is the lack of stable bytecode for it.

[MoarVM]: https://www.moarvm.org/
[Raku]: https://raku.org/


### SBCL

Condition system would make things easier.

Decent low-level performance.

Supports TCO.

Has a new parallel garbage collector.

[SBCL]: https://sbcl.org/


### Verdict

HotSpot or .NET.


[Graal JIT]: https://www.graalvm.org/latest/reference-manual/compiler/operations/
[.NET TC]: https://github.com/dotnet/runtime/blob/main/docs/design/features/tiered-compilation.md
[JVM]: https://en.wikipedia.org/wiki/Java_virtual_machine
[OpenJDK]: https://openjdk.org/
[CLR]: https://learn.microsoft.com/en-us/dotnet/standard/clr
[CLI]: https://en.wikipedia.org/wiki/Common_Language_Infrastructure
[.NET]: https://dotnet.microsoft.com/
