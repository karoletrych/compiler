module Compiler.TypeFinding.Tests

open Expecto
open Compiler.Parser
open Compiler.CompilerResult
open Compiler.TypeIdentifiersFinding
open Compiler.TypeResolving
open Compiler.ReferencedDllsMetadataRetriever
open Compiler.TypeFinding


let findTypes src = 
    let externals = getExternalTypes [System.Reflection.Assembly.GetAssembly(typeof<obj>)]
    [{Path = "test"; Code = src}]
    |> parseModules 
    >>= (fun modules -> resolve (modules, findtypeIdentifiers externals modules))
    |> Result.map (find externals)
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
                Expect.isTrue ((types |> Map.toList).Length > 10) ""
    ]