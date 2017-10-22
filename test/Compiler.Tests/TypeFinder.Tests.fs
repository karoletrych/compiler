module Compiler.TypeFinder.Tests

open Expecto
open Compiler.Parser
open Compiler.TypeFinder
open Compiler.Tests.ResultTestHelper
open Compiler.Result

[<Tests>]
let tests =
    testList "TypeResolving.Tests" [
        testCase "createUserDeclaredModule" <| fun _ ->
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
                    |> map createUserDeclaredModule
                isOk typesResult ""

        testCase "userDeclaredTypes" <| fun _ ->
                let parserResult = 
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

                let typeFinderResult = 
                    parserResult 
                    |> Compiler.Result.map (fun r -> (userDeclaredTypes "MyModule" r))
                isError typeFinderResult ""
    ]