# F# vs. C# comparison

Change of plans.  I may write Vila in C#?


F# pros

- Concise, don't need to write many types.
- Nicer than C#.
- Immutable by default.
- Functions over static methods.
- Recursion.
- Excellent type system.
- Data structure literals.
- Units of measure.
- No 3-part for loops.
- More concise record definitions.
- Can inline IL.

F# cons

- So many operators!  https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/symbol-and-operator-reference/
- No polymorphic functions.
- Very type heavy.
- Bad tooling (both in Vim and VS Code).
- Limited contribution support and unknown future compared to C#.
- Even less popular than Clojure.
- REPL (fsi) sucks.
- You have to manually define the file load order.
- Unclear what types you're defining with the `type` keyword.  The structure of
  the body results in a wide range of different things.  (Class, interface, enum, type.)
  - Similar with the `let` keyword.
- Too many data structure literals with bizzare syntax.
- Lambda syntax is inconsistent with rest of the language.
- Multiple ways of doing async (including *both* the C# way and the F# way).
- Recommended package management is with Paket (a separate tool).
- Bigger resulting DLLs due to requiring F# standard lib.
- Namespaces and modules?!
- Complicated casting.
- No keyword arguments/params on functions.
- Ghost types in editors are really confusing.
- Awkward to mix F# and C# in the same project.
- Multiple ways of writing generics.
- Multiple ways of calling functions.


C# pros

- I already know it.
- Closer to CIL.
- All online examples are in C#.
- Has many F# features.
- Modern C# is reasonably concise.
- `unsafe`
- Faster (F# can be fast, but you loose the functional-ness).
- Clear public API for use by other .NET languages.
- Consistent name casing conventions.  (i.e. PascalCase)
- Better tooling available (in VS Code).
- Some of the new syntax makes it better and more similar to F# (and even Clojure).
  - E.g. `namespace` statements, records, primary constructors.
- Can program Godot in it for making a Vila IDE.

C# cons

- Lots of feature creep and bloat added in recent years.
- Boring.
- Bad type system.
- Awkward to mix F# and C# in the same project.
