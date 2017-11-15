module Compiler.AssemblyWriter

open System
open System.Reflection
open System.Reflection.Emit

let fileAssemblyBuilder (path : string) name =
    AppDomain.CurrentDomain.DefineDynamicAssembly(
        AssemblyName (name), AssemblyBuilderAccess.Save,
        path).DefineDynamicModule(name)
    