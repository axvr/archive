module AsmManager

open System
open System.Runtime.Loader
open System.Runtime

type NqAssemblyLoadContext(name) =
    inherit AssemblyLoadContext(name, true)
    let mutable _resolver = AssemblyDependencyResolver("")
    override this.Load(name) = null

let loadAsm(name) = null

let createSystem = null
