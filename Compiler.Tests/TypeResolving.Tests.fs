module Compiler.TypeResolving.Tests

open Expecto
open Compiler.Ast
open Compiler.Parser


[<Tests>]
let tests =
  testList "TypeResolving.Tests" [
    testCase "resolving valid System.Console.WriteLine call" <| fun _ ->
        let program = parse @"
                fun main
                {
                    System::Console:.WriteLine(""Hello, world!"");
                }"
        // resolveTypes program []
        Expect.equal program [] ""
    testCase "resolving invalid Sys.Cons.WriteL call" <| fun _ ->
        let program = parse @"
                fun main
                {
                    Sys::Cons:.WriteL(""Hello, world!"");
                }"
        // resolveTypes program []
        Expect.equal program [] ""
    ]