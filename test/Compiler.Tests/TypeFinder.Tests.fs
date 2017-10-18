module Compiler.TypeFinder.Tests
open Expecto
open Compiler.Parser
open Compiler.TypeFinder
open Compiler.Types

[<Tests>]
let tests =
    testList "TypeResolving.Tests" [
        testCase "resolving valid System.Console.WriteLine call" <| fun _ ->
                let typesResult = 
                    @"
                    fun main 
                    {
                    }
                    fun foo (i:int)
                    {
                    }
                    class C 
                    {
                        fun foo
                        {
                        }
                    }
                    " 
                    |> parse
                    |> Result.map createUserDeclaredModule
                Expect.isOk typesResult ""
    ]