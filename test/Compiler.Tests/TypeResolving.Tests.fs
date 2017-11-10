module Compiler.TypeResolving.Tests

open Expecto
open Compiler.Ast
open Compiler.Parser
open Compiler.CompilerResult
open Compiler.SemanticCheck
open Compiler.TypeFinding
open Compiler.ReferencedAssembliesMetadata
open System.Reflection


let resolve src = 
    [("test",src)]
    |> parseModules
    >>= allKnownTypeIdentifiers (externalTypes [Assembly.GetAssembly(typeof<obj>)] )
    >>= resolve
    |> Result.map fst

[<Tests>]
let tests =

  testList "TypeResolving.Tests" [
    testCase "resolving valid System.Console.WriteLine call" <| fun _ ->
        let scanResult = 
            @"fun main {System::Console:.WriteLine(""Hello, world!"");}" 
            |> resolve
        Expect.equal scanResult (
            Result.succeed 
              {Classes =
    [{Name = "test";
      GenericTypeParameters = [];
      BaseClass = None;
      ImplementedInterfaces = [];
      Properties = [];
      Constructor = None;
      FunctionDeclarations =
       [{Name = "main";
         GenericParameters = [];
         Parameters = [];
         ReturnType = None;
         Body =
          [StaticFunctionCallStatement
             (TypeIdentifier {Namespace = ["System"];
                              TypeName = {Name = ["Console"];
                                          GenericArguments = [];};},
              {Name = "WriteLine";
               GenericArguments = [];
               Arguments = [LiteralExpression (StringLiteral "Hello, world!")];})];}];}];}
                           ) ""
    testCase "resolving invalid Sys.Cons.WriteL call" <| fun _ ->
        let scanResult = 
            @"fun main{Sys::Cons:.WriteL(""Hello, world!"");}"
            |> resolve
        Expect.equal scanResult (Result.failure (CannotResolveType (CustomTypeSpec (["Sys"],{Name = "Cons";
                                                     GenericArgs = [];})))) ""
        
    
    testCase "classes in the same module are found and substituted" <| fun _ ->
        let scanResult = 
            @"
                class A
                {
                }
                class B extends A
                {
                    fun main
                    {
                        A:.Foo();
                    }
                }
            
            "
            |> resolve
        Expect.equal scanResult (Result.succeed {Classes =
    [{Name = "test";
      GenericTypeParameters = [];
      BaseClass = None;
      ImplementedInterfaces = [];
      Properties = [];
      Constructor = None;
      FunctionDeclarations = [];}; {Name = "test::A";
                                    GenericTypeParameters = [];
                                    BaseClass = None;
                                    ImplementedInterfaces = [];
                                    Properties = [];
                                    Constructor = None;
                                    FunctionDeclarations = [];};
     {Name = "test::B";
      GenericTypeParameters = [];
      BaseClass = Some (TypeIdentifier {Namespace = ["test"];
                                        TypeName = {Name = ["A"];
                                                    GenericArguments = [];};});
      ImplementedInterfaces = [];
      Properties = [];
      Constructor = None;
      FunctionDeclarations =
       [{Name = "main";
         GenericParameters = [];
         Parameters = [];
         ReturnType = None;
         Body =
          [StaticFunctionCallStatement
             (TypeIdentifier {Namespace = ["test"];
                              TypeName = {Name = ["A"];
                                          GenericArguments = [];};},
              {Name = "Foo";
               GenericArguments = [];
               Arguments = [];})];}];}];}) ""

    ]