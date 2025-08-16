// Hello world in CIL.

.assembly Hello {}
.assembly extern mscorlib {}  // FIXME: different name in new .NET

.method static void Main()
{
    .entrypoint
    .maxstack 1
    ldstr "Hello, world!"
    call void [mscorlib]System.Console::WriteLine(string)
    ret
}
