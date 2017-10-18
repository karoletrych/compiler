module Compiler.TypeResolving.Tests

open Expecto
open Compiler.Ast
open Compiler.Parser
open Compiler.TypeResolving
open Compiler.TypeResolving.TypesScanner
open Compiler.Result


[<Tests>]
let tests =
  testList "TypeResolving.Tests" [
    testCase "resolving valid System.Console.WriteLine call" <| fun _ ->
        let scanResult = 
            @"fun main {System::Console:.WriteLine(""Hello, world!"");}" 
            |> parse
            |> function
               | Ok x -> scanTypes x
               | _ -> failwith "error in test"
        Expect.equal scanResult [[Success ((),[])]]  ""
    testCase "resolving invalid Sys.Cons.WriteL call" <| fun _ ->
        let scanResult = 
            @"fun main{Sys::Cons:.WriteL(""Hello, world!"");}"
            |> parse
            |> function
               | Ok x -> scanTypes x
               | _ -> failwith "error in test"
        Expect.equal scanResult [[Failure
                                    [CannotResolveType
                                       (CustomTypeSpec (["Sys"],CustomType ("Cons",[])))]]] ""
    ]