#if !INTERACTIVE
module Compiler.CompilerResult.Tests
#endif

#if INTERACTIVE
#r "bin/Debug/net461/Expecto.dll"
#load @"../../src/Compiler/Ast.fs"
#load @"../../src/Compiler/CompilerResult.fs"
#load @"ResultTestHelper.fs"
#endif

open Compiler.CompilerResult
open Compiler.Ast
open Compiler.Tests.ResultTestHelper

let failure1 = Result.failure (SyntaxError "error")
let failure2 = Result.failure (TypeNotFound (CustomTypeSpec([], {Name = "A"; GenericArguments = []})))

let x = 
    Result.succeed 1
    |> Result.bind (fun _ -> failure1)
    |> Result.bind (fun _ -> Result.succeed 2)

isError x ""

let results = [Result.succeed 1; Result.succeed 2; Result.succeed 3; failure1; failure2]
let mergedResult = results |> Result.merge
