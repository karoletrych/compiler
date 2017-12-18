module Compiler.SemanticCheck.Tests

open Expecto
open Compiler.Parser
open Compiler.CompilerResult
open Compiler.TypeIdentifiersFinding
open Compiler.TypeResolving
open Compiler.ReferencedAssembliesMetadata
open Compiler.TypeInference
open Compiler.TypeFinding
open Compiler.SemanticCheck


let semanticCheck src = 
    let externals = externalTypes [System.Reflection.Assembly.GetAssembly(typeof<obj>)]
    [{Path = "test"; Code = src}]
    |> parseModules 
    >>= (fun modules -> resolve (modules, typeIdentifiers externals modules))
    >>= (fun modules -> inferTypes (modules, typesDictionary externals modules))
    >>= semanticCheck

[<Tests>]
let tests =
    testList "SemanticCheck.Tests" [
        ftestCase "non boolean type in if/while statements" <| fun _ ->
            let semanticCheckResult = 
                @"
                fun main 
                {
                    while(3)
                    {

                    }
                    if(""true"")
                        return 3;
                    else 
                        return4;
                }
                "
                |> semanticCheck
            Expect.equal 
                semanticCheckResult 
                (Failure  [
                    NonBooleanExpressionInIfStatement {Namespace = ["System"];
                                          TypeName = {Name = ["String"];
                                                      GenericArguments = [];};}
                    NonBooleanExpressionInWhileStatement {Namespace = ["System"];
                                             TypeName = {Name = ["Int32"];
                                                     GenericArguments = [];};};
                   ]) ""
    ]