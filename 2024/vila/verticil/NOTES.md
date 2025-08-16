## Vila (VertiCIL IL Assembler)

___An interactive, runtime focused .NET [CIL][] (Common Intermediate Language)
assembler.___

## VertiCIL language

___A small, concatenative, stack-based programming language that compiles to
.NET [CIL][] whilst providing full [CLR][] access.___

[CIL]: https://en.wikipedia.org/wiki/Common_Intermediate_Language
[CLR]: https://en.wikipedia.org/wiki/Common_Language_Runtime


# VertiCIL

___A concatenative, stack-based programming language that compiles to .NET CIL
(Common Intermediate Language).___


___It's down the stack!___


## Rationale

1. A way for me to learn both .NET CIL and concatenative programming.
2. Provide quick and interactive experimentation with CLI features with direct
   access to it and easily convertable to CIL.
3. Alternative to Microsoft's ILasm tool.
4. Might be able to use it in some way as a part of a larger .NET based
   language.


If I want to use CIL as the compilation target of any future languages, I need
to understand how its features work and understand them well.


CIL is stack-based, so Forth really makes sense for it.  CIL only appears to be stack-based on method definitions?

<!-- (<- $0 $1 add) -->

Methods in CIL have a local stack, which makes programming it simpler and less error prone.

Need namespaces.  Foo::Bar
Language extensions need a separate namespace rather than living in the current class.

Statically typed (but not checked) with dynamic escape hatches.

Modern .NET uses proper assemblies instead of mscorlib

Namespace aliases.

Macros + metaprogramming.

Make use of type aliasing in CIL.

Consume and produce the assembly doc .xml files.

Consume and produce the assembly PDB files.  (Debug info.)
https://learn.microsoft.com/en-us/dotnet/core/diagnostics/symbols
Equivalent of `///` comments.

Be a CLI consumer and producer.  VertiCIL should be as good a .NET citizen as possible.


Name munging, like Clojure's.

Simple numbering rules:
```
digits + r + digits
- + value
digits + r + - + digits

16r12AB
2r010010010010
8r12
10r1234 = 1234
-1234
16r-1234
```


```cil
.assembly Hello {}
.assembly extern mscorlib {}
.method static void Main()
{
    .entrypoint
    .maxstack 1
    ldstr "Hello, world!"
    call void [mscorlib]System.Console::WriteLine(string)
    ret
}
```

```vcil
|Hello .assembly

|Main [] @ $ .method {
    "Hello, world" System.Console::WriteLine ;
} ;

$0 (method arg 0) $1 (method arg 1)

:> Main [ ^int32 ^int32 | ^void ] { $0 $1 add ret ; };

: Main [ ^int32 ^int32 | ^int32 ] -> $0 $1 add ret ;
```

- String, multiline strings, character and regex literals.
- Variety of number literals.
- Method args $x
- Local vars.


## Possible file extensions

- `*.vcil` <-- this seems the best to me.
- `*.vert`
- `*.vrt`
- `*.vt`
- `*.vtc`
- `*.vc`


Name choices:

- VertiCIL
- Facility  Facilitate
- CounCIL
- PenCIL
- StenCIL


---

Plan
  - Investigate Factor, Forth and other stack/concatenative languages.
  - Build CLI.vim.
  - Build an ILasm implementation in F#?
    - Designed for interactive development.
      - Docs
      - Load a file
      - Execute interactively
      - Maybe integrate with Godot .NET to create an IDE?
      - Enables writing CIL by hand and embedding into an application.
        - At runtime or compile time.
    - Microsoft's ILasm is not readily available
      - ILasm.exe packaged with VS on Windows, but not available on other platforms.
      - .NET (Core) ILasm can be built manually from .NET source (too awkward).
      - Not designed for interactive use and iterative development (constrcuts PEs to run) as I wish to use it.
      - Does not perform very good validation and checking.
      - Platform dependent builds.
    - Focus on runtime code generation!
    - Disassembler (ILdasm) ???
    - Debugger ???
    - Can auto-perform some optimisations (e.g. convert ldc.xx -1 and variants to optimised opcodes).
    - Test JIT optimisations work.
      - apparently "depends on the specific mode of CIL" (whatever that means)
      - e.g. vectorisation, pattern matching cache, constant time optimisation and more.
  - Build an interactive concatenative language (VertiCIL) based on ILasm with docs.
    - Probably not, but worth considering if it would be possible to implement as a superset of ILasm.
    - Async/concurrency support?  Delimited continuations?
  - Come up with a machine API for representing ILasm and generating VertiCIL.

Building up a platform for a new way of programming on .NET
