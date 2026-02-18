- Nq dependency resolver.
- Nq compiler.
- Nq source loader.

---

- Watch a Git repo (via HTTPS or deploy keys) for source to load.  This provides several benefits:
  1. Automatic deployments.
  2. Rely on Git processes to handle deployments.
  3. Encourages Git tagging?
- How to distinguish between prod and dev deployments?
- TODO: loading secrets and other config?
- Maybe instead of watching, pipeline calls it with the commit hash?

---

- Custom expressive language for generating CIL near-directly.
  - Access to all CLR features, even those that C# and F# don't enable.
  - Some kind of FORTH derivitive?
  - Avoid extra C#/F# inserted stuff.
- Custom toolchain for building DLLs.
  - ilasm.exe replacement.
  - Validate CIL?
  - CIL REPL.


```
public static int Add2(int P_0)
{
    return P_0 + 2;
}
```

```
.method public static hidebysig int32 Add2(int32){
    ldarg.0
    ldc.i4 2
    add
    ret
}
```

```
pub fn Add2 [ P_0 ]
  P_0 + 2

(defn Add2 [p_0]
  (+ p_0 2))
```

CILisp

```
.assembly extern mscorlib {}
.assembly Test1 {}
.method static void Main()
{
    .entrypoint
    ldstr      “Hello World”
    call       void [mscorlib]System.Console::WriteLine(string)
    ret
}
```


```
(asmreq mscorlib {s.c System.Console})

(asm Test1)

(fn Main ^void []
  (s.c::WriteLine "Hello world"))
```

```
.class public Foo {
    .method public static int32 Add(int32, int32) cil managed {
        .maxstack 2
        ldarg.0 // load the first argument;
        ldarg.1 // load the second argument;
        add     // add them;
        ret     // return the result;
    }
}
```

```
(class Foo)

(method Foo::Add int32 [foo: int32 bar: int32]
  (,add foo bar))

.tail
```

```
(class Foo)

(fn Foo::Add
  [foo: int32, bar: int32]: int32
  (+ foo bar))
```

- fn = static method
- method = method
- class = class

Could I create a pre-processor / macro system for CIL?  E.g. swap out simpler `{ ... }` with actual bytecode.

```
.class public Foo {

}
```

> [!NOTE]
> See also: <../../2024/vila>
