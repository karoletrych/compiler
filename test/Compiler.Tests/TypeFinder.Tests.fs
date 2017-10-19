module Compiler.TypeFinder.Tests
open Expecto
open Compiler.Parser
open Compiler.TypeFinder

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
                    |> Result.map createUserDeclaredModule
                Expect.isOk typesResult ""

        testCase "userDeclaredTypes" <| fun _ ->
                let typeFinderResult = 
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
                    |> Result.map (fun r -> (userDeclaredTypes "MyModule" r) )
                Expect.isError typeFinderResult ""
    ]