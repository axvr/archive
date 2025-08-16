" syntax case ignore

syntax match cilAsmDelimiters /[{}<>()[\]]/

syntax match cilAsmComment ,//.*$, contains=@Spell

syn region cilAsmString start=/"/ skip=/\\"/ end=/"/ contains=@Spell
hi def link cilAsmString String

syn match cilAsmNumber /\<0x\x\+\>/
hi def link cilAsmNumber Constant

syn keyword cilAsmDirective
            \ .assembly .class .method .entrypoint .maxstack .locals .namespace
            \ .import .field .property

syntax keyword cilAsmMethodAttr
            \ static public private family final specialname virtual abstract
            \ assembly famandassem famorassem privatescope hidebysig newslot
            \ rtspecialname unmanagedexp reqsecobj

syntax keyword cilAsmType
            \ int int8 int16 int32 int64 char date decimal currency error
            \ true false null string void bool variant syschar sysstring
            \ nullref bytearray float float32 float64 native unsigned object

syntax keyword cilAsmNumericOp
            \ add add.ovf add.ovf.un sub sub.ovf sub.ovf.un
            \ mul mul.ovf mul.ovf.un div div.un
            \ rem rem.un
            \ shl shr shr.un ceq cgt cgt.un clt clt.un ckfinite

syntax keyword cilAsmBoolOp
            \ not or and xor

syntax keyword cilAsmBranchOp
            \ nextgroup=cilAsmLabel skipwhite skipnl
            \ beq beq.s bge bge.s bge.un bge.un.s bgt bgt.s bgt.un bgt.un.s
            \ ble ble.s ble.un ble.un.s blt blt.s blt.un blt.un.s
            \ bne.un bne.un.s br br.s
            \ brfalse brfalse.s brinst brinst.s brnull brnull.s
            \ brtrue brtrue.s brzero brzero.s jmp

syntax keyword cilAsmLoadOp
            \ ldarg ldarg.0 ldarg.1 ldarg.2 ldarg.3 ldarg.s ldarga ldarga.s
            \ ldc.i4 ldc.i4.0 ldc.i4.1 ldc.i4.2 ldc.i4.3 ldc.i4.4 ldc.i4.5
            \ ldc.i4.6 ldc.i4.7 ldc.i4.8 ldc.i4.M1 ldc.i4.m1 ldc.i4.s ldc.i8
            \ ldc.r4 ldc.r8
            \ ldelem ldelem.i ldelem.i1 ldelem.i2 ldelem.i4 ldelem.i8
            \ ldelem.r4 ldelem.r8 ldelem.ref ldelem.u1 ldelem.u2 ldelem.u4
            \ ldelem.u8 ldelema ldfld ldflda ldftn ldind.i ldind.i1 ldind.i2
            \ ldind.i4 ldind.i8 ldind.r4 ldind.r8 ldind.ref ldind.u1 ldind.u2
            \ ldind.u4 ldind.u8 ldlen ldloc ldloc.0 ldloc.1 ldloc.2 ldloc.3
            \ ldloc.s ldloca ldloca.s ldnull ldobj ldsfld ldsflda ldstr
            \ ldtoken ldvirtftn

syntax keyword cilAsmStoreOp
            \ starg starg.s stdcall stdcall
            \ stelem stelem.i stelem.i1 stelem.i2 stelem.i4 stelem.i8
            \ stelem.r4 stelem.r8 stelem.ref
            \ stfld
            \ stind.i stind.i1 stind.i2 stind.i4 stind.i8 stind.r4 stind.r8
            \ stind.ref
            \ stloc stloc.0 stloc.1 stloc.2 stloc.3 stloc.s stobj

syntax keyword cilAsmConvertOp
            \ castclass
            \ conv.i conv.i1 conv.i2 conv.i4 conv.i8 conv.ovf.i conv.ovf.i.un
            \ conv.ovf.i1 conv.ovf.i1.un conv.ovf.i2 conv.ovf.i2.un
            \ conv.ovf.i4 conv.ovf.i4.un conv.ovf.i8 conv.ovf.i8.un
            \ conv.ovf.u conv.ovf.u.un conv.ovf.u1 conv.ovf.u1.un conv.ovf.u2
            \ conv.ovf.u2.un conv.ovf.u4 conv.ovf.u4.un conv.ovf.u8
            \ conv.ovf.u8.un conv.r.un conv.r4 conv.r8 conv.u conv.u1 conv.u2
            \ conv.u4 conv.u8

syntax keyword cilAsmDebug break

syntax keyword cilAsmException
            \ throw rethrow .try catch filter endfilter finally endfinally fault endfault

syntax match cilAsmLabel /\k\+/ contained
syntax match cilAsmLabelDef /\k\+:[^:]/ contains=cilAsmLabel

hi def link cilAsmDelimiters Delimiter
hi def link cilAsmComment Comment
hi def link cilAsmNumericOp Function
hi def link cilAsmBoolOp Function
hi def link cilAsmType Type
hi def link cilAsmLabel Tag
hi def link cilAsmDirective PreProc
hi def link cilAsmMethodAttr StorageClass
hi def link cilAsmBranchOp Conditional
hi def link cilAsmLoadOp Statement
hi def link cilAsmStoreOp Statement
hi def link cilAsmDebug Debug
hi def link cilAsmException Exception
