module Compiler.Tests.Runner

open Expecto

[<EntryPoint>]
let main argv =
    runTestsInAssembly defaultConfig argv
