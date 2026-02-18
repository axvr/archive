module Experiment

open System
open System.Reflection
open System.Runtime.Loader
open System.Runtime

// We first load the Enqueue compiler into the assembly and give it the source code to compile into the assembly.
// The Enqueue runtime (embedded within the compiler can handle the dependency management)

// This enables multiple versions of the compiler and runtime to run at the same time.

// Relying on AssemblyLoadContext might make passing shared data structures around more difficult.
// However, its use will allow safer shutdown and total system replacement.  Less in the kernel.

// Let .NET handle the unloading of built in dependencies.  Each Enqueue application will be built into a single assembly?

type NqAssemblyLoadContext(name) =
    inherit AssemblyLoadContext(name, true)
    let mutable _resolver = AssemblyDependencyResolver("")
    override this.Load(name) = null

let startSystem asmPath =
    let sys1 = NqAssemblyLoadContext("Sys1")
    let asm1 = sys1.LoadFromAssemblyPath(asmPath)
    // for t in asm1.GetTypes() do
    //     printf "%s\n" t.
    printf "%s\n" asm1.Location
    sys1.Unload()

    // Might not be needed.
    GC.Collect()
    GC.WaitForPendingFinalizers()

// WeakReference


// Generic assembly loader.
// Collection of loaded assemblies that all unload at once.

// The Enqueue compiler will build runtime assemblies.
