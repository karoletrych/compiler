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
    >>= (semanticCheck true)

[<Tests>]
let tests =
    testList "SemanticCheck.Tests" [
        testCase "non boolean type in if/while statements" <| fun _ ->
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
        testCase "assignment of invalid type in variable declaration" <| fun _ ->
            let semanticCheckResult = 
                @"
                fun main 
                {
                    var a : string = 3;
                    val b : int = 4.0;
                }
                "
                |> semanticCheck
            Expect.equal 
                semanticCheckResult 
                (Failure  [
                    InvalidTypeInVariableDeclaration
                     ("b",{Namespace = ["System"];
                           TypeName = {Name = ["Int32"];
                                       GenericArguments = [];};},
                      {Namespace = ["System"];
                       TypeName = {Name = ["Single"];
                                   GenericArguments = [];};});
                   InvalidTypeInVariableDeclaration
                     ("a",{Namespace = ["System"];
                           TypeName = {Name = ["String"];
                                       GenericArguments = [];};},
                      {Namespace = ["System"];
                       TypeName = {Name = ["Int32"];
                                   GenericArguments = [];};})]) ""
        testCase "assignment to variable of incompatible type" <| fun _ ->
            let semanticCheckResult = 
                @"
                fun main 
                {
                    val a : string = 3;
                    val b : int = 4.0;
                }
                "
                |> semanticCheck
            Expect.equal 
                semanticCheckResult 
                (Failure  [
                    InvalidTypeInVariableDeclaration
                     ("b",{Namespace = ["System"];
                           TypeName = {Name = ["Int32"];
                                       GenericArguments = [];};},
                      {Namespace = ["System"];
                       TypeName = {Name = ["Single"];
                                   GenericArguments = [];};});
                   InvalidTypeInVariableDeclaration
                     ("a",{Namespace = ["System"];
                           TypeName = {Name = ["String"];
                                       GenericArguments = [];};},
                      {Namespace = ["System"];
                       TypeName = {Name = ["Int32"];
                                   GenericArguments = [];};})]) ""
        testCase "assignment to readonly variable" <| fun _ ->
            let semanticCheckResult = 
                @"
                fun main 
                {
                    val a : string = ""123"";
                    a = a + ""321"";
                }
                "
                |> semanticCheck
            Expect.equal 
                semanticCheckResult 
                (Failure [AssignmentToReadOnlyVariable "a"]) ""                                   
        testCase "assignment to readonly field" <| fun _ ->
            let semanticCheckResult = 
                @"
                class A
                {
                    val b : string = ""BBB""

                    fun setB(arg : string)
                    {
                        b = arg;
                    }
                }

                fun main 
                {
                    val a = new A();
                    a.b = ""CCC"";
                    a.setB(""CCC"");
                }
                "
                |> semanticCheck
            Expect.equal 
                semanticCheckResult 
                (Failure [AssignmentToReadOnlyFieldOnType ({Namespace = [];
                                     TypeName = {Name = ["A"; "test"];
                                                 GenericArguments = [];};},"b");
   AssignmentToReadOnlyLocalField "b"]) ""                                   
        
        testCase "operator is applicable for type" <| fun _ ->
            let semanticCheckResult = 
                @"
                class A
                {
                }
                fun main 
                {
                    var a = new A() + new A();
                }
                "
                |> semanticCheck
            Expect.equal 
                semanticCheckResult 
                (Failure  [
                    InvalidTypeInVariableDeclaration
                     ("b",{Namespace = ["System"];
                           TypeName = {Name = ["Int32"];
                                       GenericArguments = [];};},
                      {Namespace = ["System"];
                       TypeName = {Name = ["Single"];
                                   GenericArguments = [];};});
                   InvalidTypeInVariableDeclaration
                     ("a",{Namespace = ["System"];
                           TypeName = {Name = ["String"];
                                       GenericArguments = [];};},
                      {Namespace = ["System"];
                       TypeName = {Name = ["Int32"];
                                   GenericArguments = [];};})]) ""
        testCase "entry point" <| fun _ ->
            let semanticCheckResult = 
                @"
                
                "
                |> semanticCheck
            Expect.equal 
                semanticCheckResult 
                (Failure  [NoEntryPointOrMoreThanOne]) ""
    ]