module Compiler.TypeFinding.Tests

open Expecto
open Compiler.Parser
open Compiler.CompilerResult
open Compiler.TypeIdentifiersFinding
open Compiler.TypeResolving
open Compiler.ReferencedAssembliesMetadata
open Compiler.TypeFinding


let findTypes src = 
    let externals = externalTypes [System.Reflection.Assembly.GetAssembly(typeof<obj>)]
    [("test",src)]
    |> parseModules 
    >>= allKnownTypeIdentifiers externals
    >>= resolve
    >>= allKnownTypes externals
    |> Result.map snd
    |> Result.get

[<Tests>]
let tests =
    testList "TypeFinding.Tests" [
        testCase "createUserDeclaredModule" <| fun _ ->
                let types = 
                    @"
                    fun main 
                    {
                    }
                    fun foo (i:int)
                    {
                    }
                    class Cub 
                    {
                        fun foo
                        {
                        }
                    }
                    " 
                    |> findTypes
                Expect.equal (types |> Map.toList).Length 1533 ""
    ]