module Compiler.TypeFinding.Tests

open Expecto
open Compiler.Ast
open Compiler.Parser
open Compiler.Tests.ResultTestHelper
open Compiler.CompilerResult

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
                    |> parseDeclarations
                    |> Result.map Module.createDefault
                isOk typesResult ""
    ]