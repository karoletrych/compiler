module Compiler.Tests

open Expecto

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly {defaultConfig with mySpiritIsWeak = true} argv