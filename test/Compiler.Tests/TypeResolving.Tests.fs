module Compiler.TypeResolving.Tests

open Expecto
open Compiler.Ast
open Compiler.Parser
open Compiler.CompilerResult
open Compiler.TypeIdentifiersFinding
open Compiler.ReferencedAssembliesMetadata
open System.Reflection


let resolve src = 
    let externals = externalTypes [Assembly.GetAssembly(typeof<obj>)]
    [{Path = "test"; Code = src}]
    |> parseModules 
    >>= (fun modules -> resolve (modules, typeIdentifiers externals modules))

[<Tests>]
let tests =
  testList "TypeResolving.Tests" [
    testCase "resolving valid System.Console.WriteLine call" <| fun _ ->
        let scanResult = 
            @"fun main {System::Console:.WriteLine(""Hello, world!"");}" 
            |> resolve
        Expect.equal scanResult (
                Result.succeed 
                    [{Identifier = {Namespace = [];
                          TypeName = {Name = ["test"];
                                      GenericArguments = [];};};
                    Functions = [{
                                   Name = "main";
                                   Parameters = [];
                                   ReturnType = None;
                                   Body = [
                                            StaticFunctionCallStatement(TypeIdentifier {Namespace = ["System"];
                                                        TypeName = {Name = ["Console"];
                                                                    GenericArguments = [];};},{
                                                Name = "WriteLine";
                                                 GenericArguments = [];
                                                 Arguments =
                                                  [AstExpression (LiteralExpression (StringLiteral "Hello, world!"))]
                                                  })];}]; Classes = [];}]) ""
    testCase "resolving invalid Sys.Cons.WriteL call" <| fun _ ->
        let scanResult = 
            @"fun main{Sys::Cons:.WriteL(""Hello, world!"");}"
            |> resolve
        Expect.equal scanResult (Result.failure (TypeNotFound (CustomTypeSpec (["Sys"],{Name = "Cons";
                                                     GenericArgs = [];})))) ""
        
    
    testCase "classes in the same module are found and substituted" <| fun _ ->
        let scanResult = 
            @"
                class A
                {
                }
                class B : A
                {
                    fun main
                    {
                        A:.Foo();
                    }
                }
            
            "
            |> resolve
        Expect.equal scanResult (Result.succeed 
                [{Identifier = {Namespace = [];
                      TypeName = {
                                    Name = ["test"];
                                  GenericArguments = [];};};
        Functions = [];
        Classes =
                 [{Identifier = {Namespace = [];
                                 TypeName = {Name = ["A"; "test"];
                                             GenericArguments = [];};};
                   BaseClass = None;
                   Fields = [];
                   Constructors = [];
                   Functions = [];};
                  {Identifier = {Namespace = [];
                                 TypeName = {Name = ["B"; "test"];
                                             GenericArguments = [];};};
                   BaseClass = Some (TypeIdentifier {Namespace = [];
                                                     TypeName = {Name = ["A"; "test"];
                                                                 GenericArguments = [];};});
                   Fields = [];
                   Constructors = [];
                   Functions =
                    [{Name = "main";
                      Parameters = [];
                      ReturnType = None;
                      Body =
                       [StaticFunctionCallStatement
                          (TypeIdentifier {Namespace = [];
                                           TypeName = {Name = ["A"; "test"];
                                                       GenericArguments = [];};},
                           {Name = "Foo";
                            GenericArguments = [];
                            Arguments = [];})];}];}];}]) ""

    ]