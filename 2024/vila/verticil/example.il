.method private hidebysig static void Main(string[] args) cil managed
{
    .entrypoint
    .maxstack  2
    .locals init (int32 V_0, int32 V_1)
              ldc.i4.2
              stloc.0
              br.s       IL_001f
    IL_0004:  ldc.i4.2
              stloc.1
              br.s       IL_0011
    IL_0008:  ldloc.0
              ldloc.1
              rem
              brfalse.s  IL_001b
              ldloc.1
              ldc.i4.1
              add
              stloc.1
    IL_0011:  ldloc.1
              ldloc.0
              blt.s      IL_0008
              ldloc.0
              call       void [mscorlib]System.Console::WriteLine(int32)
    IL_001b:  ldloc.0
              ldc.i4.1
              add
              stloc.0
    IL_001f:  ldloc.0
              ldc.i4     0x3e8
              blt.s      IL_0004
              ret
}
