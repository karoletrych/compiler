module Compiler.TypeResolving.Tests

open Expecto
open Compiler.Ast
open Compiler.Parser


[<Tests>]
let tests =
  testList "TypeResolving.Tests" [
    // testCase "resolving System.Console.WriteLine call" <| fun _ ->
    //     let program = parse @"
    //             fun main
    //             {
    //                 System::Console:.WriteLine(""Hello, world!"");
    //             }"
    //     Expect.equal program [] ""
    ]