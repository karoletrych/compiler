module Compiler.TypeResolving.Tests

open Expecto
open Compiler.Ast
open Compiler.Parser
open Compiler.TypeResolving
open Compiler.TypeResolving.TypesScanner


[<Tests>]
let tests =
  testList "TypeResolving.Tests" [
    testCase "resolving valid System.Console.WriteLine call" <| fun _ ->
        let scanResult = 
            @"fun main {System::Console:.WriteLine(""Hello, world!"");}" 
            |> parse 
            |> scanTypes
        Expect.equal scanResult [[ValidTypeSpec]] ""
    testCase "resolving invalid Sys.Cons.WriteL call" <| fun _ ->
        let scanResult = 
            @"fun main{Sys::Cons:.WriteL(""Hello, world!"");}"
            |> parse
            |> scanTypes
        Expect.equal scanResult [[InvalidTypeSpec(CustomTypeSpec ([Identifier "Sys"],CustomType (Identifier "Cons",[])))]]  ""
    ]